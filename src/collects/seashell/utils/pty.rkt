#lang typed/racket
;; Seashell's FFI to PTY functions.
;; Copyright (C) 2017 The Seashell Maintainers;
;;   based on pty.rkt from willghatch/rackterm -- Copyright (C) 2015.
;;   Originally licensed under LGPLv3+.
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; See also 'ADDITIONAL TERMS' at the end of the included LICENSE file.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.
(module ffi racket/base
  (require ffi/unsafe
           ffi/unsafe/define
           racket/string
           racket/port
           racket/system)
  (provide (all-defined-out))

  (define-ffi-definer define-self (ffi-lib #f))
  (define-ffi-definer define-pty (ffi-lib "libutil"))
  (define-cstruct _winsize ([height _ushort]
                            [width _ushort]
                            [x _ushort]
                            [y _ushort]))
  (define TIOCSWINSZ_gnu #x5414)
  (define TIOCGWINSZ_gnu #x5413)
  (define TIOCSCTTY_gnu #x540E)
  (define TIOCNOTTY_gnu #x5422)
  (define TIOCSWINSZ_bsd #x80087467)
  (define TIOCGWINSZ_bsd #x40087468)
  (define TIOCSCTTY_bsd #x20007461)
  (define TIOCNOTTY_bsd #x20007471)

  (define os-type (system-type 'os))
  (define uname-s (if (equal? os-type 'windows)
                      "windows"
                      (string-trim (with-output-to-string
                                     (lambda () (system "uname -s"))))))
  (define freebsd-ioctls? (or (string-ci=? uname-s "FreeBSD")
                              (string-ci=? uname-s "Darwin")))
  ;; ioctl request parameters are ints in Linux, but longs in FreeBSD and MacOSX
  (define ioctl-req-type (if (or (equal? (system-type 'word) 64)
                                 freebsd-ioctls?)
                             _long _int))

  (define TIOCSWINSZ (if freebsd-ioctls? TIOCSWINSZ_bsd TIOCSWINSZ_gnu))
  (define TIOCGWINSZ (if freebsd-ioctls? TIOCGWINSZ_bsd TIOCGWINSZ_gnu))
  (define TIOCSCTTY (if freebsd-ioctls? TIOCSCTTY_bsd TIOCSCTTY_gnu))
  (define TIOCNOTTY (if freebsd-ioctls? TIOCNOTTY_bsd TIOCNOTTY_gnu))

  (define-self scheme_make_fd_output_port
               (_fun _int _scheme _int _int _int -> _scheme))

  (define (new-winsize [width 80] [height 24])
    (make-winsize height width 0 0))

  (define (_openpty [width 80] [height 24])
    (define winsize (new-winsize width height))
    (define-pty openpty (_fun (amaster : (_ptr o _int))
                              (aslave : (_ptr o _int))
                              (name : _pointer)
                              (termios-ptr : _pointer)
                              (winsize : (_ptr i _winsize))
                              -> (r : _int)
                              -> (if ( < 0 r )
                                   (error "openpty failed: ~a." r)
                                   (values amaster aslave))))
    (define-values (masterfd slavefd) (openpty #f #f winsize))
    (define-values (m-in m-out) (scheme_make_fd_output_port masterfd (format "<pty-master:~a>" masterfd) 0 0 1))
    (define-values (s-in s-out) (scheme_make_fd_output_port slavefd (format "<pty-slave:~a>" slavefd) 0 0 1))
    (values m-in m-out s-in s-out masterfd slavefd))

  (define (_set-pty-size fd width height)
    (define-pty ioctl (_fun (fd : _int)
                            (request : ioctl-req-type)
                            (winsize : (_ptr i _winsize))
                            -> (r : _int)
                            -> (if ( < 0 r )
                                 (error "ioctl (setting PTY size) failed: ~a." r)
                                 (void))))
    (ioctl fd TIOCSWINSZ (new-winsize width height)))

  (define (_get-pty-size fd)
    (define-pty ioctl (_fun (fd : _int)
                            (request : ioctl-req-type)
                            (winsize : (_ptr o _winsize))
                            -> (r : _int)
                            -> (if ( < 0 r )
                                 (error "ioctl (setting PTY size) failed: ~a." r)
                                 winsize)))
    (define winsize (ioctl fd TIOCGWINSZ))
    (values (winsize-width winsize) (winsize-height winsize))))

(require/typed (submod "." ffi)
               [_openpty (->* () (Integer Integer) (Values Input-Port Output-Port Input-Port Output-Port Integer Integer))]
               [_set-pty-size (-> Integer Integer Integer Any)]
               [_get-pty-size (-> Integer (Values Integer Integer))])
(provide (all-defined-out))

(struct PTY ([master : Integer]
             [slave : Integer]
             [m-in : Input-Port]
             [m-out : Output-Port]
             [s-in : Input-Port]
             [s-out : Output-Port]
             [cust : Custodian]) #:transparent)

(: make-pty (->* () (Integer Integer) PTY))
(define (make-pty [width 80] [height 24])
  (define cust (make-custodian))
  (parameterize ([current-custodian cust])
    (define-values (m-in m-out s-in s-out master slave) (_openpty width height))
    (PTY master slave m-in m-out s-in s-out cust)))

(: close-pty (-> PTY Any))
(define (close-pty pty)
  (custodian-shutdown-all (PTY-cust pty)))

(: close-pty-slave-end (-> PTY Any))
(define (close-pty-slave-end pty)
  (unless (port-closed? (PTY-s-in pty)) (close-input-port (PTY-s-in pty))
  (unless (port-closed? (PTY-s-out pty)) (close-output-port (PTY-s-out pty)))))

(: pty-resize (-> PTY Integer Integer Any))
(define (pty-resize pty width height)
  (_set-pty-size (PTY-master pty) width height))

(: pty-getsize (-> PTY (Values Integer Integer)))
(define (pty-getsize pty)
  (_get-pty-size (PTY-master pty)))
