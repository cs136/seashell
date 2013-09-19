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
;;   closed? - connection closed/dead or not?
;;
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
;;   control - Control function which deals with control frames.
;;
;;   mask?   - Mask connection or not.
;;
;;   last-pong - Time at which the most recent pong frame was received from the client.
;;
;; A connection is a synchronizable event that is ready when
;; seashell-websocket-read-frame will not block with synchronization result
;; itself.
(require racket/async-channel
         seashell/log)
(provide seashell-websocket-connection?
         make-seashell-websocket-connection
         make-seashell-websocket-control
         ws-send
         ws-recv
         ws-recv-evt
         ws-close!
         ws-destroy!)

(struct seashell-websocket-connection
  ([closed? #:mutable]
   [in-thread #:mutable]
   [out-thread #:mutable]
   in-port out-port in-chan out-chan
   cline headers
   control mask?
   [last-pong #:mutable])
  #:transparent
  #:property prop:evt
  (lambda (conn)
    (wrap-evt (seashell-websocket-connection-in-chan conn)
              (lambda (conn) conn))))

;; seashell-websocket-frame
;; Internal data structure for a WebSocket frame.
(struct seashell-websocket-frame
  (final? rsv opcode data)
  #:transparent)

;; (make-seashell-websocket-connection in-port out-port control cline headers mask?) ->
;; Creates a new WebSocket connection given in/out ports, headers,
;; and a control function for dealing with control frames.
;;
;; See above for arguments details.
(define/contract (make-seashell-websocket-connection in-port out-port control cline headers mask?)
  (-> port? port? (-> seashell-websocket-connection? (or/c seashell-websocket-frame? exn?) any/c) any/c any/c boolean? seashell-websocket-connection?)
  (define in-chan (make-async-channel))
  (define out-chan (make-async-channel))
  (define conn (seashell-websocket-connection #f
                                 #f
                                 #f
                                 in-port out-port
                                 in-chan out-chan
                                 cline headers
                                 control
                                 mask?
                                 0))
  (set-seashell-websocket-connection-in-thread! conn (in-thread conn))
  (set-seashell-websocket-connection-out-thread! conn (out-thread conn))
  conn)

;; (make-seashell-websocket-control) ->
;; Evaluates to a function suitable for use as the 'control' argument to make-seashell-websocket-connection.
(define/contract (make-seashell-websocket-control)
  (-> (-> seashell-websocket-connection? (or/c seashell-websocket-frame? exn?) any/c))
  (define (simple-control conn frame-or-exn)
    (match frame-or-exn
      [(? exn?) (raise frame-or-exn)]  ;; TODO something else
      [(seashell-websocket-frame #t rsv 9 data) ;; ping
       (async-channel-put (seashell-websocket-connection-out-chan conn)
                          (seashell-websocket-frame #t 0 10 data))
       #t]
      [(seashell-websocket-frame #t rsv 10 data) ;; pong
       (set-seashell-websocket-connection-last-pong! conn (current-inexact-milliseconds))]
      [(seashell-websocket-frame #t rsv 8 data) ;; close
       (unless (seashell-websocket-connection-closed? conn)
         (async-channel-put (seashell-websocket-connection-out-chan conn)
                            (seashell-websocket-frame #t 0 8 data))
         (set-seashell-websocket-connection-closed?! conn #t))
       #f]
      [else ;; TODO - log unhandled control frame event.
            #t]))
  simple-control)


;; (ws-send conn bytes) ->
;; Sends bytes over websocket connection conn.
(define/contract (ws-send conn bytes)
  (-> seashell-websocket-connection? bytes? void?)
  (async-channel-put (seashell-websocket-connection-out-chan conn)
                     (seashell-websocket-frame #t 0 2 bytes)))

;; (ws-recv conn bytes) ->
;; Receives bytes synchronously from websocket connection conn.
(define/contract (ws-recv conn)
  (-> seashell-websocket-connection? bytes?)
  (seashell-websocket-frame-data
    (async-channel-get (seashell-websocket-connection-in-chan conn))))

;; (ws-recv-evt conn bytes) ->
;; Produces an event that is ready when data can be received on the websocket.
;; The synchronization result is data read from the socket.
(define/contract (ws-recv-evt conn)
  (-> seashell-websocket-connection? evt?)
  (wrap-evt (seashell-websocket-connection-in-chan conn)
            seashell-websocket-frame-data))

;; (ws-close! conn) ->
;; Closes a websocket connection. Does nothing if the connection has already been closed.
;; Note that this routine does not close the underlying socket. See ws-destroy!.
(define/contract (ws-close! conn)
  (-> seashell-websocket-connection? void?)
  (unless (seashell-websocket-connection-closed? conn)
    (async-channel-put (seashell-websocket-connection-out-chan conn)
                      (seashell-websocket-frame #t 0 8 #""))
    (set-seashell-websocket-connection-closed?! conn #t)))

;; (ws-destroy! conn) ->
;; Frees all resources used by a websocket connection. Consider calling ws-close! sometime
;; before you call ws-destroy!.
(define/contract (ws-destroy! conn)
  (-> seashell-websocket-connection? void?)
  (set-seashell-websocket-connection-closed?! conn #t)
  (kill-thread (seashell-websocket-connection-in-thread conn))
  (kill-thread (seashell-websocket-connection-out-thread conn))
  (close-input-port (seashell-websocket-connection-in-port conn))
  (close-output-port (seashell-websocket-connection-out-port conn))
  (void))

;; exn:websocket
;; Internal websocket exception structure.
(struct exn:websocket exn:fail:user ())

;; Handy syntax rule for EOF checking
(define-syntax-rule (check-eof x)
  (when (eof-object? x)
    (raise (exn:websocket (format "read-frame: Unexpected end of file!")
                          (current-continuation-marks)))))

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
  (define framing-byte (read-byte port))
  (check-eof framing-byte)

  ;; Extract bit information
  (define final?
    (bitwise-bit-set? framing-byte 7))
  (define rsv-field
    (bitwise-bit-field framing-byte 4 7))
  (define opcode
    (bitwise-bit-field framing-byte 0 4))

  ;; Read first byte of length and masking information
  (define length/mask-byte (read-byte port))
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
     (define length-bs (read-bytes 2 port))
     (check-eof length-bs)
     (set! length (integer-bytes->integer length-bs #f #t))]
    [(equal? length 127)
     (define length-bs (read-bytes 8 port))
     (check-eof length-bs)
     (set! length (integer-bytes->integer length-bs #f #t))]
    [else void])

  ;; Read the mask if it's set.
  (define masking
    (if masked?
        (let ([mask-bs (read-bytes 4 port)])
          (check-eof mask-bs)
          mask-bs)
        #"\0\0\0\0"))

  ;; Read data, and unmask - note XOR is symmetric.
  (define data
    (if masked? (mask (read-bytes length port) masking)
        (read-bytes length port)))

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
      [(> 126 data-length)
       (bytes (bitwise-ior (if mask? 128 0) data-length))]
      [(> 65536 data-length)
       (bytes-append (bytes (if mask? 254 126)) (integer->integer-bytes
                                         data-length 2 #f #t))]
      [else
       (bytes-append (bytes (if mask? 255 127)) (integer->integer-bytes
                                         data-length 8 #f #t))]))

  ;; Generate a 32-bit random number
  (define masking
    (if mask? (integer->integer-bytes (random 4294967087) 4 #f #t) 0))

  ;; Mask data
  (when mask?
    (set! data (mask data masking)))

  ;; Write everything out
  (write-byte framing-byte port)
  (write-bytes length-bstr port)
  (when mask? (write-bytes masking port))
  (write-bytes data port)
  (flush-output port))

;; (in-thread conn)
;; Starts the input thread which reads frames off of the input port
;; and writes frames into the channel.
;;
;; Arguments (given through connection):
;;  port - Input port
;;  channel - Async channel to put completed frames in.
;;  control - Control function that handles any control frames sent
;;   in the channel.  If the control function returns #f,
;;   this thread will die.  This is typically used to deal
;;   with the CLOSE control frame.
;;   Control will also be sent exception objects as they occur.
;;
;;   Note that this thread will not close the I/O ports.
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
(define/contract (in-thread conn)
  (-> seashell-websocket-connection? thread?)

  ;; Internal state for dealing with fragmented frames.
  (define fragmented-buffer #f)
  (define fragmented-opcode #f)
  (define fragmented-rsv #f)
  (define fragmented? #f)
  (define port (seashell-websocket-connection-in-port conn))
  (define channel (seashell-websocket-connection-in-chan conn))
  (define control (seashell-websocket-connection-control conn))

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
                  (control conn exn)
                  (async-channel-put channel exn)))]
            (define frame (read-frame port))
            (logf 'info "Read frame ~s" frame)

            (cond
              ;; Case 0 - Control frame.  When (control frame)
              ;; returns #f this thread will quit.  It is the responsibility
              ;; of control to close the port and everything.
              [(> (seashell-websocket-frame-opcode frame) 7)
               (when (control conn frame)
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
;;  conn - Seashell WebSocket connection.  This function uses
;;  the output port, output channel, and the mask? fields of the connection.
;;
;; Returns:
;;  Thread object.  Send thread a message to get it to write a CLOSE
;;  frame into the port and then quit.
(define/contract (out-thread conn)
  (-> seashell-websocket-connection? thread?)
  (define port (seashell-websocket-connection-out-port conn))
  (define channel (seashell-websocket-connection-out-chan conn))
  (define mask? (seashell-websocket-connection-mask? conn))

  ;; Internal fragmentation state.
  ;; TODO handle sending fragmented frames. (A sequence of non-final frames followed by a final frame)
  (define fragmented? #f)

  ;; Does this belong in the connection object?
  (define last-ping-send-time 0)

  (thread
   (lambda ()
     (let loop ()
       (define alrm (alarm-evt (+ last-ping-send-time 30000)))
       (define sync-result (sync (thread-receive-evt) channel alrm))
       (cond
         ;; Message on the thread mailbox. Quit immediately.
         [(eq? sync-result (thread-receive-evt)) #t]
         ;; Ping alarm went off. Send ping, reset alarm, repeat.
         [(eq? sync-result alrm)
          (send-frame (seashell-websocket-frame #t 0 9 #"") port #:mask? mask?)
          (set! last-ping-send-time (current-inexact-milliseconds))
          (loop)]
         [else
          ;; Send the frame, repeat
          (send-frame sync-result port #:mask? mask?)
          (loop)])))))
