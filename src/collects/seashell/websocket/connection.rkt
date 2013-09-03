#lang racket
;; Seashell's websocket library.
;; Copyright (C) 2013 The Seashell Maintainers.
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

;; seashell-websocket-connection
;; Internal data structure for the Seashell Websocket connection.
;;
;; Fields:
;;   in/out-thread - Input/Output thread for handling fragmented packets
;;   and synchronization functionality.
;;
;;   in-port - (Raw) input port.
;;   out-port - (Raw) output port.
;;
;;   in-chan - Input (read) channel.
;;   out-chan - Output (write) channel.
;;
;;   cline - Connection line.
;;   headers - HTTP request headers.
;;
;;   closed? - connection closed/dead or not?
;;
;; A connection is a synchronizable event that is ready when
;; seashell-websocket-read-frame will not block with synchronization result
;; itself.
(require racket/async-channel)

(struct seashell-websocket-connection
  ([closed? #:mutable]
   in-thread
   out-thread
   in-port out-port in-chan out-chan
   cline headers)
  #:transparent
  #:property prop:evt
  (lambda (conn)
    (wrap-evt (seashell-websocket-connection-in-chan conn)
              (lambda (conn) conn))))

;; seashell-websocket-frame
;; Internal data structure for a WebSocket frame.
(struct seashell-websocket-frame
  (final? rsv opcode data))

;; exn:websocket
;; Internal websocket exception structure.
(struct exn:websocket exn:fail:user ())

;; Handy syntax rule for EOF checking
(define-syntax-rule (check-eof x)
  (when (eof-object? x)
    (error 'read-frame "Premature connection close!")))

;; (mask data key) -> bytes?
;; Masks/Unmasks data from a WebSocket connection.
;;
;; Arguments:
;;  data - data to (un)mask.
;;  key  - masking key.
;;
;; Returns:
;;  (Un)masked data.
(define/contract (mask data key)
  (-> bytes? bytes? bytes?)

  ;; Recursive helper that mutates the byte string
  ;; to produce the unmasked version.
  (define (mask-helper data key index)
    (cond
      [(equal? index (bytes-length data))
       data]
      [else
       (bytes-set! data index
                   (bitwise-xor (bytes-ref data index)
                                (bytes-ref key (remainder index 4))))
       (mask-helper data key (add1 index))]))

  (mask-helper (bytes-copy data) key 0))



;; (read-frame port) -> seashell-websocket-frame?
;; Reads a WebSocket frame in from port.
;;
;; Arguments:
;;  port - Input port.
;; Returns:
;;  WebSocket frame.
(define/contract (read-frame port)
  (-> port? seashell-websocket-frame?)
  ;; Read the framing byte.
  (define framing-byte (read-byte in-port))
  (check-eof framing-byte)

  ;; Extract bit information
  (define final?
    (bitwise-bit-set? framing-byte 7))
  (define rsv-field
    (bitwise-bit-field framing-byte 4 7))
  (define opcode
    (bitwise-bit-field framing-byte 0 4))

  ;; Read first byte of length and masking information
  (define length/mask-byte (read-byte in-port))
  (check-eof length/mask-byte)

  (define masked?
    (bitwise-bit-set? length/mask-byte 7))
  (define length
    (bitwise-bit-field length/mask-byte 0 7))

  ;; Interpret the length field
  (cond
    ;; Read the next two bytes, and interpret it as
    ;; a big-endian integer
    [(equal? length 126)
     (define length-bs (read-bytes 2 in-port))
     (check-eof length-bs)
     (set! length (integer-bytes->integer length-bs #f #t))]
    [(equal? length 127)
     (define length-bs (read-bytes 8 in-port))
     (check-eof length-bs)
     (set! length (integer-bytes->integer length-bs #f #t))]
    [else void])

  ;; Read the mask if it's set.
  (define masking
    (if masked?
        (let ([mask-bs (read-bytes 4 in-port)])
          (check-eof mask-bs)
          mask-bs)
        #"\0\0\0\0"))

  ;; Read data, and unmask - note XOR is symmetric.
  (define data
    (if masked? (mask (read-bytes length in-port) masking)
        (read-bytes length in-port)))

  ;; Return the frame.
  (seashell-websocket-frame final? rsv-field opcode data))

;; (send-frame frame port #:mask) -> void?
;; Sends a WebSocket frame along the port.
;;
;; Arguments:
;;  frame - WebSocket frame to send.
;;  port  - port to send it on.
;; Optional Arguments:
;;  mask - mask data or not.
;;
;; Returns:
;;  Nothing.
(define/contract (send-frame frame port
                             #:mask? [mask? #f])
  (->* (seashell-websocket-frame? port?)
       (#:mask?
        boolean?)
       void?)

  ;; Construct the framing byte
  (define framing-byte
    (bitwise-ior (seashell-websocket-frame-opcode frame)
                 (arithmetic-shift (seashell-websocket-frame-rsv frame) 4)
                 (if (seashell-websocket-frame-final? frame) 128 0)))

  ;; Grab data
  (define data (seashell-websocket-frame-data frame))
  (when (equal? (seashell-websocket-frame-opcode frame) 1)
    (set! data (string->bytes/utf-8 data)))
  (define data-length (bytes-length data))

  ;; Construct length and mask flag information
  (define length-bstr
    (cond
      [(< 126 data-length)
       (bytes (bitwise-ior (if mask? 128 0) data-length))]
      [(< 65536 data-length)
       (bytes-append (if mask? 254 126) (integer->integer-bytes
                                         data-length 2 #f #t))]
      [else
       (bytes-append (if mask? 255 127) (integer->integer-bytes
                                         data-length 8 #f #t))]))

  ;; Generate a 32-bit randon number
  (define masking
    (if mask? (integer->integer-bytes data-length 4 #f #t (random 4294967087)) 0))

  ;; Mask data
  (when mask?
    (set! data (mask data masking)))

  ;; Write everything out
  (write-byte framing-byte port)
  (write-bytes length-bstr port)
  (write-bytes masking port)
  (write-bytes data port)
  (flush-output port))

;; (in-thread port channel control)
;; Starts the input thread which reads frames off of the input port
;; and writes frames into the channel.
;;
;; Arguments:
;;  port - Input port
;;  channel - Async channel to put completed frames in.
;;  control - Control function that handles any control frames sent
;;   in the channel.  If the control function returns #f,
;;   this thread will die.  This is typically used to deal
;;   with the CLOSE control frame.
;;   Control will also be sent exception objects as they occur.
;;
;; Returns:
;;  Thread object.  Send thread a message to make it quit.  Thread
;;  will also die if a exn:websocket? is raised.
;;
;; Notes:
;;  The only objects that will be written to channel will be completed frames
;;  OR exception objects that denote a failure condition.
;;
;; TODO: Might be worthwhile to support partial incomplete reads
;; with some sort of signaling mechanism.
(define/contract (in-thread port channel control)
  (-> port? async-channel? (-> seashell-websocket-frame? boolean?) thread?)

  ;; Internal state for dealing with fragmented frames.
  (define fragmented-buffer #f)
  (define fragmented-opcode #f)
  (define fragmented-rsv #f)
  (define fragmented? #f)

  (thread
   (lambda ()
     (let loop ()
       (define sync-result (sync (thread-receive-evt) port))
       (cond
         ;; Read a frame off of the port
         [(eq? sync-result port)
          (with-handlers
              ;; Got an exception - report it,
              ;; and then quit immediately.
              ;; This will be an unclean shutdown in all cases.
              [(exn:websocket?
                (lambda (exn)
                  (control exn)
                  (async-channel-put channel exn)))]
            (define frame (read-frame port))
            (cond
              ;; Case 0 - Control frame.  When (control frame)
              ;; returns #f this thread will quit.  It is the responsibility
              ;; of control to close the port and everything.
              [(> (seashell-websocket-frame-opcode frame) 7)
               (when (control frame)
                 (loop))]
              ;; Case 1 - first and only (unfragmented) frame.
              [(and (not fragmented?)
                    (seashell-websocket-frame-final? frame))
               (async-channel-put channel frame)
               (loop)]
              ;; Case 2 - start of a fragmented frame.
              [(and (not fragmented?)
                    (not (seashell-websocket-frame-final? frame)))
               (set! fragmented? #t)
               (set! fragmented-rsv (seashell-websocket-frame-rsv frame))
               (set! fragmented-opcode (seashell-websocket-frame-opcode frame))
               (set! fragmented-buffer (seashell-websocket-frame-data frame))
               (loop)]
              ;; Case 3 - continuation frame.
              [(and fragmented?
                    (not (seashell-websocket-frame-final? frame))
                    (equal? 0 (seashell-websocket-frame-opcode frame)))
               (set! fragmented-buffer
                     (bytes-append fragmented-buffer (seashell-websocket-frame-data frame)))
               (loop)]
              ;; Case 4 - final frame.
              [(and fragmented?
                    (seashell-websocket-frame-final? frame)
                    (equal? 0 (seashell-websocket-frame-opcode frame)))
               (set! fragmented-buffer
                     (bytes-append fragmented-buffer (seashell-websocket-frame-data frame)))
               ;; Reset fragmented?
               (set! fragmented? #f)
               ;; Write the entire frame to the port
               (async-channel-put channel (seashell-websocket-frame #t fragmented-rsv fragmented-opcode fragmented-buffer))
               (loop)]
              [else
               (raise (exn:websocket (format "Unknown frame ~a!" frame)
                                     (current-continuation-marks)))]
              ))]
         ;; Read a message
         ;; Might be worthwhile to handle other sorts of messages.
         [else
          ;; Quit.
          #t])))))

;; (out-thread port channel)
;; Starts the out thread which reads frames off of the output channel
;; and writes frames into the port.
;;
;; Arguments:
;;  port - Output port
;;  channel - Async channel to read frames from.
;;  mask? - Mask any frames we send?
;;
;; Returns:
;;  Thread object.  Send thread a message to get it to write a CLOSE
;;  frame into the port and then quit.
(define/contract (out-thread port channel mask?)
  (-> port? async-channel? boolean? thread?)
  ;; Internal fragmentation state.
  (define fragmented? #f)

  (thread
   (lambda ()
     (let loop ()
       (define sync-result (sync (thread-receive-evt) channel))
       (cond
         ;; Message on the thread mailbox.
         [(eq? sync-result (thread-receive-evt))
          ;; Send a CLOSE frame immediately, then quit.
          (send-frame (seashell-websocket-frame #t 0 8 #"") port #:mask? mask?)
          #t]
         [else
          ;; Send the frame, repeat
          (send-frame sync-result port #:mask? mask)])))))





