#lang typed/racket
;; Seashell's websocket library.
;; Copyright (C) 2013-2015 The Seashell Maintainers.
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
;;   uri - HTTP request URL.
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
(require typed/racket/async-channel
         seashell/log
         typed/web-server/http
         typed/net/url
         seashell/utils)

(provide ws-connection?
         make-ws-connection
         make-ws-control
         ws-send
         ws-recv
         ws-close
         ws-connection-closed-evt
         ws-connection-closed?
         (struct-out exn:websocket)
         Websocket-Connection)

;; exn:websocket
;; Internal websocket exception structure.
(struct exn:websocket exn:fail:user ())

;; ws-frame
;; Internal data structure for a WebSocket frame.
(struct ws-frame
  ([final? : Boolean] [rsv : Integer] [opcode : Integer] [data : Bytes])
  #:transparent #:type-name Websocket-Frame)

;; Type of Control Function
(define-type Websocket-Control (-> Websocket-Connection (U Websocket-Frame exn) Any))

;; ws-connection
;; Connection structure.  Usable as a synchronizable event
;; with the following possible results:
;;  bytes? - Regular websocket frame.
;;  eof-object? - End of file/Websocket closed.
;;  <raised exception> - error.
(struct ws-connection
  ([closed-semaphore : Semaphore]
   [in-thread : (U Thread False)]
   [out-thread : (U Thread False)]
   [in-port : Input-Port] [out-port : Output-Port]
   [in-chan : (Async-Channelof (U Websocket-Frame exn))]
   [out-chan : (Async-Channelof Websocket-Frame)]
   [method : Bytes] [uri : URL] [headers : (Listof Header)]
   [control : Websocket-Control] [mask? : Boolean]
   [last-ping : Flonum]
   [last-pong : Flonum]
   [close-semaphore : Semaphore])
  #:transparent
  #:mutable
  #:type-name Websocket-Connection
  #:property prop:evt
  (lambda ([conn : Websocket-Connection])
    (choice-evt
     (wrap-evt (ws-connection-in-chan conn)
               (lambda ([frame-or-exn : (U Websocket-Frame exn)])
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
(: make-ws-connection (-> Input-Port Output-Port Websocket-Control Bytes URL (Listof Header) Boolean Websocket-Connection))
(define (make-ws-connection in-port out-port control method uri headers mask?)
  (define in-chan #{(make-async-channel) :: (Async-Channelof (U Websocket-Frame exn))})
  (define out-chan #{(make-async-channel) :: (Async-Channelof Websocket-Frame)})
  (define conn (ws-connection (make-semaphore 0)
                                 #f
                                 #f
                                 in-port out-port
                                 in-chan out-chan
                                 method uri headers
                                 control
                                 mask?
                                 0.0 0.0
                                 (make-semaphore 1)))
  (set-ws-connection-in-thread! conn (in-thread conn))
  (set-ws-connection-out-thread! conn (out-thread conn))
  conn)

;; (ws-connection-closed-evt conn)
;; Gets an event that is ready when the connection is closed.
;;
;; Arguments:
;;  conn - Connection.
;; Returns:
;;  Event.
(: ws-connection-closed-evt (-> Websocket-Connection (Rec x (Evtof x))))
(define (ws-connection-closed-evt conn)
  (semaphore-peek-evt (ws-connection-closed-semaphore conn)))

;; (ws-connection-closed? conn)
;; Tests if a connection is closed.
;;
;; Arguments:
;;  conn - Connection.
;; Returns:
;;  true if closed, false otherwise.
(: ws-connection-closed? (-> Websocket-Connection Boolean))
(define (ws-connection-closed? conn)
  (if (sync/timeout 0
                    (ws-connection-closed-evt conn))
    #t #f))

;; (make-ws-control) ->
;; Evaluates to a function suitable for use as the 'control' argument to make-ws-connection.
;;
;; Returns:
;;  (ws-connection? (or/c ws-frame? exn) -> any/c)
;;  Function that'll work as a control to a websocket connection.  By default,
;;  this function will close automatically and destroy all resources associated
;;  with a connection when the CLOSE handshake is received.
(: make-ws-control (-> Websocket-Control))
(define (make-ws-control)
  (: simple-control Websocket-Control)
  (define (simple-control conn frame-or-exn)
    (match frame-or-exn
      [(? exn?)
       ;; There's not much we can do.  We may as well log it and
       ;; quit.  Raising an exception here is probably a bad idea,
       ;; as we're running in a separate thread.
       (logf 'error (format "WebSocket received exception: ~a" (exn-message frame-or-exn)))
       (thread (lambda () (ws-close! conn)))
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
       (thread (lambda () (ws-close! conn)))
       #f]
      [else
       ;; Unhandled message - log it, quit.
       (logf 'warning (format "WebSocket - unknown control message: ~s" frame-or-exn))
       ]))
  simple-control)

;; (ws-send conn bytes) ->
;; Sends bytes over websocket connection conn.
;;
;; Arguments:
;;  conn - Websocket connection.
;;  bytes - Bytes to send.
(: ws-send (-> Websocket-Connection Bytes Void))
(define (ws-send conn bytes)
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
(: ws-recv (-> Websocket-Connection (U Bytes EOF)))
(define (ws-recv conn)
  (define result (sync (cast conn (Evtof (U Bytes EOF)))))
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
(: ws-close (->* (Websocket-Connection) (#:timeout Nonnegative-Real) Void))
(define (ws-close conn #:timeout [timeout 5])
  (thread
    (lambda ()
      (call-with-semaphore (ws-connection-close-semaphore conn)
        (lambda ()
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
(: ws-close! (-> Websocket-Connection Void))
(define (ws-close! conn)
  (call-with-semaphore (ws-connection-close-semaphore conn)
    (lambda ()
      (with-handlers
        ([exn:fail:network? void])
        (unless (ws-connection-closed? conn)
          (semaphore-post (ws-connection-closed-semaphore conn))
          (define ithread (ws-connection-in-thread conn))
          (define othread (ws-connection-out-thread conn))
          (when ithread
            (kill-thread ithread))
          (when othread
            (kill-thread othread))
          (send-frame (ws-frame #t 0 8 #"")
                      (ws-connection-out-port conn)
                      #:mask?
                      (ws-connection-mask? conn))
          (close-input-port (ws-connection-in-port conn))
          (close-output-port (ws-connection-out-port conn))
          (void))))))

;; Handy syntax rule for EOF checking
(: check-eof (All (X) (-> (U EOF X) X)))
(define (check-eof x)
  (cond
    [(eof-object? x)
      (raise (exn:websocket (format "read-frame: Unexpected end of file!")
                            (current-continuation-marks)))]
    [else
      x]))
;; (mask data key) -> bytes?
;; Masks/Unmasks data from a WebSocket connection.
;;
;; Arguments:
;;  data - data to (un)mask.
;;  key  - masking key.
;;
;; Returns:
;;  (Un)masked data.
(: mask (-> Bytes Bytes Bytes))
(define (mask data key)
  ;; Recursive helper that mutates the byte string
  ;; to produce the unmasked version.
  (: mask-helper (-> Bytes Bytes Nonnegative-Integer Bytes))
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
(: read-frame (-> Input-Port Websocket-Frame))
(define (read-frame port)
  (with-handlers
    ([exn:fail:network? (lambda ([exn : exn])
                          (raise (exn:websocket (exn-message exn)
                                                (current-continuation-marks))))])
    ;; Read the framing byte.
    (define framing-byte (check-eof (read-byte port)))

    ;; Extract bit information
    (define final?
      (bitwise-bit-set? framing-byte 7))
    (define rsv-field
      (bitwise-bit-field framing-byte 4 7))
    (define opcode
      (bitwise-bit-field framing-byte 0 4))

    ;; Read first byte of length and masking information
    (define length/mask-byte (check-eof (read-byte port)))

    (define masked?
      (bitwise-bit-set? length/mask-byte 7))
    (define length
      (bitwise-bit-field length/mask-byte 0 7))

    ;; Interpret the length field
    (cond
      ;; Read the next two bytes, and interpret it as
      ;; a big-endian integer
      [(equal? length 126)
       (define length-bs (check-eof (read-bytes 2 port)))
       (set! length (integer-bytes->integer length-bs #f #t))]
      [(equal? length 127)
       (define length-bs (check-eof (read-bytes 8 port)))
       (set! length (integer-bytes->integer length-bs #f #t))]
      [else void])

    ;; Read the mask if it's set.
    (define masking
      (if masked?
          (let ([mask-bs (check-eof (read-bytes 4 port))])
            mask-bs)
          #"\0\0\0\0"))

    ;; Read data, and unmask - note XOR is symmetric.
    (define data
      (if masked? (mask (check-eof (read-bytes length port)) masking)
          (check-eof (read-bytes length port))))
    ;; Make sure that we actually got the right amount of data!
    (unless (equal? (bytes-length data) length)
      (raise (exn:websocket (format "read-frame: Unexpected end of frame!")
                            (current-continuation-marks))))

    ;; Return the frame.
    (ws-frame final? rsv-field opcode data)))

;; (send-frame frame port #:mask) -> void?
;; Sends a WebSocket frame along the port.
;;
;; Arguments:
;;  frame - WebSocket frame to send.
;;  port  - port to send it on.
;; Optional Arguments:
;;  mask - mask data or not.
;;  fragment-tail? Frame is not the head of a sequence of fragmented frames.
;;
;; Returns:
;;  Nothing.
(: send-frame (->* (Websocket-Frame Output-Port) (#:mask? Boolean #:fragment-tail? Boolean) Void))
(define (send-frame frame port
                    #:mask? [mask? #f]
                    #:fragment-tail? [fragment-tail? #f])
  ;; Construct the framing byte
  (define framing-byte
    (bitwise-ior (if fragment-tail? 0 (ws-frame-opcode frame))
                 (arithmetic-shift (ws-frame-rsv frame) 4)
                 (if (ws-frame-final? frame) 128 0)))

  ;; Grab data (note : we do not handle string/bytes here; this is up to the client to deal with).
  (define data (ws-frame-data frame))
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
    (if mask? (integer->integer-bytes (random 4294967087) 4 #f #t) #"\0\0\0\0"))

  ;; Mask data
  (when mask?
    (set! data (mask data masking)))

  ;; Write everything out - making sure that any network errors
  ;; get reported correctly.
  (with-handlers
    ([exn:fail:network? (lambda ([exn : exn])
                          (raise (exn:websocket (exn-message exn)
                                                (current-continuation-marks))))])
    (write-byte framing-byte port)
    (write-bytes length-bstr port)
    (when mask? (write-bytes masking port))
    (write-bytes data port)
    (flush-output port)))

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
(: in-thread (-> Websocket-Connection Thread))
(define (in-thread conn)
  (define port (ws-connection-in-port conn))
  (define channel (ws-connection-in-chan conn))
  (define control (ws-connection-control conn))

  (thread
   (lambda ()
     ;; Internal state for dealing with fragmented frames.
     (let loop ([fragmented-read-end #{#f :: (U False Input-Port)}]
                [fragmented-write-end #{#f :: (U False Output-Port)}]
                [fragmented-opcode #{#f :: (U False Integer)}]
                [fragmented-rsv #{#f :: (U False Integer)}]
                [fragmented? #{#f :: Boolean}])
       (define sync-result (sync port))
       (with-handlers
         ;; Got an exception - report it,
         ;; and then quit immediately.
         ;; This will be an unclean shutdown in all cases.
         ([exn:websocket?
           (lambda ([exn : exn])
             (control conn exn)
             (async-channel-put channel exn))])
         (define frame (read-frame port))
         (cond
           ;; Case 0 - Control frame.
           ;; It is the responsibility of control to close the port and
           ;; kill in/out threads.
           [(> (ws-frame-opcode frame) 7)
             (control conn frame)
             (loop fragmented-read-end fragmented-write-end fragmented-opcode fragmented-rsv fragmented?)]
           ;; Case 1 - first and only (unfragmented) frame.
           [(and (not fragmented?)
                 (ws-frame-final? frame))
             (async-channel-put channel frame)
             (loop fragmented-read-end fragmented-write-end fragmented-opcode fragmented-rsv fragmented?)]
           ;; Case 2 - start of a fragmented frame.
           [(and (not fragmented?)
                 (not (ws-frame-final? frame)))
             (define-values (l-fragmented-read-end l-fragmented-write-end) (make-pipe))
             (write-bytes (ws-frame-data frame) l-fragmented-write-end)
             (loop l-fragmented-read-end l-fragmented-write-end (ws-frame-opcode frame) (ws-frame-rsv frame) #t)]
           ;; Case 3 - continuation frame.
           [(and fragmented?
                 (not (ws-frame-final? frame))
                 (equal? 0 (ws-frame-opcode frame)))
             (assert fragmented-write-end output-port?)
             (write-bytes (ws-frame-data frame) fragmented-write-end)
             (loop fragmented-read-end fragmented-write-end fragmented-opcode fragmented-rsv fragmented?)]
           ;; Case 4 - final frame.
           [(and fragmented?
                 (ws-frame-final? frame)
                 (equal? 0 (ws-frame-opcode frame)))
             ;; Flush and close the port
             (assert fragmented-write-end output-port?)
             (write-bytes (ws-frame-data frame) fragmented-write-end)
             (close-output-port fragmented-write-end)
             ;; Write the entire frame to the port
             (assert fragmented-read-end input-port?)
             (assert fragmented-rsv integer?)
             (assert fragmented-opcode integer?)
             (async-channel-put channel (ws-frame #t fragmented-rsv fragmented-opcode (port->bytes fragmented-read-end)))
             (loop #f #f #f #f #f)]
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
(: out-thread (-> Websocket-Connection Thread))
(define (out-thread conn)
  (define port (ws-connection-out-port conn))
  (define channel (ws-connection-out-chan conn))
  (define report-channel (ws-connection-in-chan conn))
  (define mask? (ws-connection-mask? conn))
  (define control (ws-connection-control conn))

  (thread
   (lambda ()
     (let loop ([fragmented? #{#f :: Boolean}])
       (with-handlers
         ;; Got an exception - report it,
         ;; and then quit immediately.
         ;; This will be an unclean shutdown in all cases.
         ([exn:websocket?
           (lambda ([exn : exn])
             (control conn exn)
             (async-channel-put report-channel exn))])
         (define alrm (alarm-evt (+ (ws-connection-last-ping conn) 30000)))
         (define sync-result (sync channel alrm))
         (cond
           ;; Ping alarm went off. Send ping, reset alarm, repeat.
           [(eq? sync-result alrm)
            (send-frame (ws-frame #t 0 9 #"") port #:mask? mask?)
            (set-ws-connection-last-ping! conn (current-inexact-milliseconds))
            (loop fragmented?)]
           [else
            ;; Send the frame, repeat
            (assert sync-result ws-frame?)
            (send-frame sync-result port #:mask? mask? #:fragment-tail? fragmented?)
            (loop (not (ws-frame-final? sync-result)))]))))))
