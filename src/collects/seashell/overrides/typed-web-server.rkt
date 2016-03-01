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
(require typed/web-server/http)
(provide (all-from-out typed/web-server/http))

(provide Dispatcher)
(define-type Dispatcher (-> Connection Request Void))

(require/typed/provide web-server/http/response
                       [print-headers (-> Output-Port (Listof Header) Void)])
(require/typed/provide web-server/http/request
                       [read-headers (-> Input-Port (Listof Header))])
(require/typed/provide web-server/http/request-structs
                       [request-bindings/raw (-> Request (Listof Binding))])
(require/typed/provide web-server/private/timer
                       [#:opaque Timer-Manager timer-manager?]
                       [#:struct timer ([tm : Timer-Manager]
                                        [evt : (Evtof Any)]
                                        [expire-seconds : Number]
                                        [action : (-> Void)])
                                        #:type-name Timer]
                       [cancel-timer! (-> Timer Void)])
(provide Timer)
(require/typed/provide web-server/private/connection-manager
                       [#:struct connection ([id : Integer]
                                             [timer : Timer]
                                             [i-port : Input-Port]
                                             [o-port : Output-Port]
                                             [custodian : Custodian]
                                             [close? : Boolean])
                        #:type-name Connection])
(provide Connection)
(require/typed/provide web-server/dispatchers/dispatch
                       [next-dispatcher (-> Nothing)])
(require/typed web-server/private/dispatch-server-sig
               [#:signature dispatch-server-connect^
                ([port->real-ports : (-> Input-Port Output-Port
                                         (Values Input-Port Output-Port))])])
(define-type Dispatch-Server-Connect@ (Unit (import) (export dispatch-server-connect^)))
(provide dispatch-server-connect^ Dispatch-Server-Connect@)

(require/typed/provide web-server/web-server
               [serve (->* (#:dispatch Dispatcher)
                           (#:confirmation-channel (Async-Channelof (U exn Integer))
                            #:connection-close? Boolean
                            #:dispatch-server-connect@ Any ;; Can't generate Dispatch-Server-Connect@ right now...
                            #:port Integer
                            #:listen-ip (U String False)
                            #:max-waiting Integer
                            #:initial-connection-timeout Integer)
                           (-> Void))])
