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

;; ws-connection
;; Internal data structure for the Websocket connection.
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
;; ws-read-frame will not block with synchronization result
;; the data that would be read from ws-recv invoked on itself,
;; <eof>, or an exception object.
(require racket/async-channel
         seashell/log)

(provide ws-connection?
         make-ws-connection
         make-ws-control
         ws-send
         ws-recv
         ws-close
         exn:websocket?
         exn:websocket)

;; exn:websocket
;; Internal websocket exception structure.
(struct exn:websocket exn:fail:user ())

;; ws-frame
;; Internal data structure for a WebSocket frame.
(struct ws-frame
  (final? rsv opcode data)
  #:transparent)

;; ws-connection
;; Connection structure.  Usable as a synchronizable event
;; with the following possible results:
;;  bytes? - Regular websocket frame.
;;  eof-object? - End of file/Websocket closed.
;;  <raised exception> - error.
(struct ws-connection
  (closed-semaphore
   [in-thread #:mutable]
   [out-thread #:mutable]
   in-port out-port in-chan out-chan
   cline headers
   control mask?
   [last-ping #:mutable]
   [last-pong #:mutable]
   close-semaphore)
  #:transparent
  #:property prop:evt
  (lambda (conn)
    (choice-evt
     (wrap-evt (ws-connection-in-chan conn)
               (lambda (frame-or-exn)
                 (cond
                   [(exn? frame-or-exn)
                    ;; This will work as expected, actually.
                    (raise frame-or-exn)]
                   [else
                    (ws-frame-data frame-or-exn)])))
     (wrap-evt (semaphore-peek-evt
                (ws-connection-closed-semaphore conn))
               (lambda (ignored)
                 eof)))))

;; (make-ws-connection in-port out-port control cline headers mask?) ->
;; Creates a new WebSocket connection given in/out ports, headers,
;; and a control function for dealing with control frames.
;;
;; See above for arguments details.
(define/contract (make-ws-connection in-port out-port control cline headers mask?)
  (-> port? port? (-> ws-connection? (or/c ws-frame? exn?) any/c) any/c any/c boolean? ws-connection?)
  (define in-chan (make-async-channel))
  (define out-chan (make-async-channel))
  (define conn (ws-connection (make-semaphore 0)
                                 #f
                                 #f
                                 in-port out-port
                                 in-chan out-chan
                                 cline headers
                                 control
                                 mask?
                                 0 0
                                 (make-semaphore 1)))
  (set-ws-connection-in-thread! conn (in-thread conn))
  (set-ws-connection-out-thread! conn (out-thread conn))
  conn)

;; (ws-connection-closed? conn)
;; Tests if a connection is closed.
;;
;; Arguments:
;;  conn - Connection.
;; Returns:
;;  true if closed, false otherwise.
(define/contract (ws-connection-closed? conn)
  (-> ws-connection? boolean?)
  (if (sync/timeout 0
                    (semaphore-peek-evt
                      (ws-connection-closed-semaphore conn)))
    #t #f))

;; (make-ws-control) ->
;; Evaluates to a function suitable for use as the 'control' argument to make-ws-connection.
;;
;; Returns:
;;  (ws-connection? (or/c ws-frame? exn) -> any/c)
;;  Function that'll work as a control to a websocket connection.  By default,
;;  this function will close automatically and destroy all resources associated
;;  with a connection when the CLOSE handshake is received.
(define/contract (make-ws-control)
  (-> (-> ws-connection? (or/c ws-frame? exn?) any/c))
  (define (simple-control conn frame-or-exn)
    (match frame-or-exn
      [(? exn?)
       ;; There's not much we can do.  We may as well log it and
       ;; quit.  Raising an exception here is probably a bad idea,
       ;; as we're running in a separate thread.
       (logf 'error #f
              (format "WebSocket received exception: ~a" (exn-message frame-or-exn)))
       (thread (thunk (ws-close! conn)))
       #f]
      [(ws-frame #t rsv 9 data)
       ;; Ping frame.
       (async-channel-put (ws-connection-out-chan conn)
                          (ws-frame #t 0 10 data))
       #t]
      [(ws-frame #t rsv 10 data)
       ;; Pong frame.
       (set-ws-connection-last-pong! conn (current-inexact-milliseconds))]
      [(ws-frame #t rsv 8 data)
       ;; Behaviour seems poorly documented here in the RFC.
       ;; Closing the TCP port is supposed to be handled by the server.
       ;; In any case, we'll have to send a CLOSE frame,
       ;; which is handled by ws-close!
       ;;
       ;; As we're receiving a CLOSE frame, even though it's recommended that
       ;; the server closes the connection, we may as well do it anyways.

       ;; Kill the connection.  (note: ws-close! ensures that the connection is closed only once).
       (thread (thunk (ws-close! conn)))
       #f]
      [else
       ;; Unhandled message - log it, quit.
       (logf 'warning
              (format "WebSocket - unknown control message: ~s" frame-or-exn))
       ]))
  simple-control)

;; (ws-send conn bytes) ->
;; Sends bytes over websocket connection conn.
;;
;; Arguments:
;;  conn - Websocket connection.
;;  bytes - Bytes to send.
(define/contract (ws-send conn bytes)
  (-> ws-connection? bytes? void?)
  (when (ws-connection-closed? conn)
    (raise (exn:websocket "Connection closed!"
                          (current-continuation-marks))))
  (async-channel-put (ws-connection-out-chan conn)
                     (ws-frame #t 0 2 bytes)))

;; (ws-recv conn) -> bytes?
;; Receives bytes synchronously from websocket connection conn.
;; This function will raise an exception if an error happened
;; on the connection.
;;
;; Arguments:
;;  conn - Websocket connection.
;; Returns:
;;  Bytes if not closed, eof-object? if closed.
;; Raises:
;;  exn:websocket if any connections occurred.
(define/contract (ws-recv conn)
  (-> ws-connection? (or/c bytes? eof-object?))
  (define result (sync conn))
  (cond 
    [(exn? result) (raise result)]
    [else result]))

;; (ws-close conn) ->
;; Sends a CLOSE frame on a websocket connection,
;; which will (hopefully) trigger the remote end to respond
;; with the second part of the close handshake, which
;; will cause the default control to close the connection.
;;
;; Fails the connection after some time has passed.
;;
;; Everything is done asynchronously.
;;
;; Arguments:
;;  conn - Websocket connection.
;;  timeout - Timeout.
(define/contract (ws-close conn #:timeout [timeout 5])
  (->* (ws-connection?) (#:timeout (and/c real? (not/c negative?)))
       void?)
  (thread
    (thunk
      (call-with-semaphore (ws-connection-close-semaphore conn)
        (thunk
          (unless (ws-connection-closed? conn)
            (async-channel-put
              (ws-connection-out-chan conn)
              (ws-frame #t 0 8 #"")))))
      (unless (sync/timeout timeout
                            (semaphore-peek-evt
                              (ws-connection-closed-semaphore conn)))
        (ws-close! conn))))
  (void))

;; (ws-close! conn) ->
;; Frees all resources used by a ws connection.
;; Also sends a CLOSE frame.  This call is nonblocking
;; and all socket I/O is done in a different thread.
;;
;; Arguments:
;;  conn - Websocket connection.
(define/contract (ws-close! conn)
  (-> ws-connection? void?)
  (call-with-semaphore (ws-connection-close-semaphore conn)
    (thunk
      (unless (ws-connection-closed? conn)
        (semaphore-post (ws-connection-closed-semaphore conn))
        (kill-thread (ws-connection-in-thread conn))
        (kill-thread (ws-connection-out-thread conn))
        (send-frame (ws-frame #t 0 8 #"")
                    (ws-connection-out-port conn)
                    #:mask?
                    (ws-connection-mask? conn))
        (close-input-port (ws-connection-in-port conn))
        (close-output-port (ws-connection-out-port conn))
        (void)))))

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

;; (read-frame port) -> ws-frame?
;; Reads a WebSocket frame in from port.
;;
;; Arguments:
;;  port - Input port.
;; Returns:
;;  WebSocket frame.
(define/contract (read-frame port)
  (-> port? ws-frame?)
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
  (ws-frame final? rsv-field opcode data))

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
  (->* (ws-frame? port?)
       (#:mask?
        boolean?)
       void?)

  ;; Construct the framing byte
  (define framing-byte
    (bitwise-ior (ws-frame-opcode frame)
                 (arithmetic-shift (ws-frame-rsv frame) 4)
                 (if (ws-frame-final? frame) 128 0)))

  ;; Grab data
  (define data (ws-frame-data frame))
  (when (equal? (ws-frame-opcode frame) 1)
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
  (-> ws-connection? thread?)

  ;; Internal state for dealing with fragmented frames.
  (define fragmented-buffer #f)
  (define fragmented-opcode #f)
  (define fragmented-rsv #f)
  (define fragmented? #f)
  (define port (ws-connection-in-port conn))
  (define channel (ws-connection-in-chan conn))
  (define control (ws-connection-control conn))

  (thread
   (lambda ()
     (let loop ()
       (define sync-result (sync port))
       (with-handlers
           ;; Got an exception - report it,
           ;; and then quit immediately.
           ;; This will be an unclean shutdown in all cases.
           [(exn:websocket?
             (lambda (exn)
               (control conn exn)
               (async-channel-put channel exn)))]
         (define frame (read-frame port))
         (cond
           ;; Case 0 - Control frame.
           ;; It is the responsibility of control to close the port and
           ;; kill in/out threads.
           [(> (ws-frame-opcode frame) 7)
             (control conn frame)
             (loop)]
           ;; Case 1 - first and only (unfragmented) frame.
           [(and (not fragmented?)
                 (ws-frame-final? frame))
             (async-channel-put channel frame)
             (loop)]
           ;; Case 2 - start of a fragmented frame.
           [(and (not fragmented?)
                 (not (ws-frame-final? frame)))
             (set! fragmented? #t)
             (set! fragmented-rsv (ws-frame-rsv frame))
             (set! fragmented-opcode (ws-frame-opcode frame))
             (set! fragmented-buffer (ws-frame-data frame))
             (loop)]
           ;; Case 3 - continuation frame.
           [(and fragmented?
                 (not (ws-frame-final? frame))
                 (equal? 0 (ws-frame-opcode frame)))
             (set! fragmented-buffer
                   (bytes-append fragmented-buffer (ws-frame-data frame)))
             (loop)]
           ;; Case 4 - final frame.
           [(and fragmented?
                 (ws-frame-final? frame)
                 (equal? 0 (ws-frame-opcode frame)))
             (set! fragmented-buffer
                   (bytes-append fragmented-buffer (ws-frame-data frame)))
             ;; Reset fragmented?
             (set! fragmented? #f)
             ;; Write the entire frame to the port
             (async-channel-put channel (ws-frame #t fragmented-rsv fragmented-opcode fragmented-buffer))
             (loop)]
           [else
            (raise (exn:websocket (format "Unknown frame ~a!" frame)
                                  (current-continuation-marks)))]))))))

;; (out-thread port channel)
;; Starts the out thread which reads frames off of the output channel
;; and writes frames into the port.
;;
;; Arguments:
;;  conn - Seashell WebSocket connection.  This function uses
;;  the output port, output channel, and the mask? fields of the connection.
;;
;; Returns:
;;  Thread object.
(define/contract (out-thread conn)
  (-> ws-connection? thread?)
  (define port (ws-connection-out-port conn))
  (define channel (ws-connection-out-chan conn))
  (define mask? (ws-connection-mask? conn))

  ;; Internal fragmentation state.
  ;; TODO handle sending fragmented frames. (A sequence of non-final frames followed by a final frame)
  (define fragmented? #f)

  (thread
   (lambda ()
     (let loop ()
       (define alrm (alarm-evt (+ (ws-connection-last-ping conn) 30000)))
       (define sync-result (sync channel alrm))
       (cond
         ;; Ping alarm went off. Send ping, reset alarm, repeat.
         [(eq? sync-result alrm)
          (send-frame (ws-frame #t 0 9 #"") port #:mask? mask?)
          (set-ws-connection-last-ping! conn (current-inexact-milliseconds))
          (loop)]
         [else
          ;; Send the frame, repeat
          (send-frame sync-result port #:mask? mask?)
          (loop)])))))
