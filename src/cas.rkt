;; Seashell
;; Copyright (C) 2012-2013 Jennifer Wong, Marc Burns
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
;;
;; Authors: Jennifer Wong, Marc Burns
;;
;; Racket CAS 2.0 authentication module
(module cas racket
  (require racket/async-channel
           net/uri-codec
           net/url-connect
           web-server/servlet
           xml
           openssl)
  
  (provide initiate-cas-auth
           validate-cas-ticket/proxyfw
           validate-cas-ticket
           gen-proxy-ticket)
  
  ;; just like call/input-url, but verify certificate
  (define (call/input-url/verify url connect handle)
    (let ((ssl-context (ssl-make-client-context)))
      (ssl-load-default-verify-sources! ssl-context)
      (ssl-set-verify! ssl-context #t)
      (ssl-set-verify-hostname! ssl-context #t)
      (ssl-set-ciphers! ssl-context "DEFAULT:!aNULL:!eNULL:!LOW:!EXPORT:!SSLv2")
      (ssl-seal-context! ssl-context)
      (parameterize
          ([current-https-protocol ssl-context])
        (call/input-url url connect handle))))
  
  ;; string string -> #f or (string/utf-8 . bytes)
  ;; Captures the current continuation and redirects the user's browser
  ;; to the CAS login page. If the browser returns from the authentication
  ;; process, execution will continue and this function will return a pair of
  ;; the generated service URL and the token. Execution does not continue
  ;; on failure.
  ;;
  ;; Arguments:
  ;;  - An absolute url U such that if V is the result of a call to send/suspend,
  ;;    then a GET request to (string-append U V) will cause the continuation
  ;;    captured by send/suspend to be invoked.
  ;;  - The CAS provider root URL.
  (define (initiate-cas-auth server-url provider-url)
    (define url-receiver #f)
    (define finish-sema (make-semaphore 1))
    (define ticket-resp
      (send/suspend
       (lambda(url)
         (set! url-receiver (form-urlencoded-encode
                             (string-append server-url url)))
         (redirect-to
          (string-append
           provider-url "/login?service="
           url-receiver)))))
    (clear-continuation-table!)
    (define bdg (force (request-bindings/raw-promise ticket-resp)))
    (cond
      [(not (semaphore-try-wait? finish-sema))
       (send/back
        (response/xexpr "Invalid service."))]
      [(not (and (list? bdg)
                 (binding:form? (bindings-assq #"ticket" bdg))))
       (send/back
        (response/xexpr "Invalid response."))]
      [else
       (cons url-receiver
             (binding:form-value (bindings-assq #"ticket" bdg)))]))
  
  ;; string string string bytes -> #f or (string string bytes bytes)
  ;; Validates a service ticket and requests a proxy ticket granting ticket.
  ;; If the ticket is valid and the proxy ticket granting ticket was granted
  ;; then a list containing the following is returned:
  ;;  - Net ID of the authenticated user.
  ;;  - URL used for the proxy callback request.
  ;;  - Proxy ticket granting ticket IOU.
  ;;  - Proxy ticket granting ticket.
  ;; Otherwise, #f is returned.
  ;;
  ;; Arguments:
  ;;  - An absolute url U such that if V is the result of a call to send/suspend,
  ;;    then a GET request to (string-append U V) will cause the continuation
  ;;    captured by send/suspend to be invoked.
  ;;  - The CAS provider root URL.
  ;;  - The service URL for which the ticket to be validated was issued.
  ;;  - The ticket to be validated.
  (define (validate-cas-ticket/proxyfw server-url provider-url service ticket)
    (define url-pgt #f)
    (define pgt-req-ch (make-async-channel))
    (define finish-sema (make-semaphore 1))
    (send/suspend/dispatch
     (lambda(embed/url)
       (set!
        url-pgt
        (form-urlencoded-encode
         (string-append server-url
                        (embed/url (lambda(req) (async-channel-put pgt-req-ch (force (request-bindings/raw-promise req)))
                                     (send/back (response/xexpr "OK")))))))
       (redirect-to (embed/url identity))))
    (when (not (semaphore-try-wait? finish-sema))
      (send/back (response/xexpr "Duplicate request.")))
    (define resp-xml
      (call/input-url/verify
       (string->url
        (string-append
         provider-url "/proxyValidate?service="
         service "&ticket=" (bytes->string/utf-8 ticket)
         "&pgtUrl=" url-pgt))
       get-pure-port
       (compose xml->xexpr document-element read-xml)))
    (define pgt-bdg
      (let loop ((bdg empty))
        (if (empty? bdg)
            (loop (sync/timeout 10 pgt-req-ch))
            bdg)))
    (clear-continuation-table!)
    (match
        resp-xml
      [`(cas:serviceResponse
         ((xmlns:cas "http://www.yale.edu/tp/cas"))
         ,_ ...
         (cas:authenticationSuccess
          ()
          ,@(list-no-order
             `(cas:user () ,user)
             `(cas:proxyGrantingTicket () ,serv-pgt-iou)
             _ ...))
         ,_ ...)
       (if (and (binding? (bindings-assq #"pgtIou" pgt-bdg))
                (binding? (bindings-assq #"pgtId" pgt-bdg))
                (equal? (binding:form-value (bindings-assq #"pgtIou" pgt-bdg))
                        (string->bytes/utf-8 serv-pgt-iou)))
           (list user
                 url-pgt
                 (binding:form-value (bindings-assq #"pgtIou" pgt-bdg))
                 (binding:form-value (bindings-assq #"pgtId"  pgt-bdg)))
           #f)]
      [else #f]))
  
  ;; string string bytes -> #f or string
  ;; Validates a service or proxy ticket. Returns the Net ID of the authenticated
  ;; user if the ticket was valid, or #f otherwise.
  ;;
  ;; Arguments:
  ;;  - The CAS provider root URL.
  ;;  - The service URL for which the ticket to be validated was issued.
  ;;  - The ticket to be validated.
  (define (validate-cas-ticket provider-url service ticket)
    (define resp-xml
      (call/input-url/verify
       (string->url
        (string-append
         provider-url "/proxyValidate?service="
         service "&ticket=" (bytes->string/utf-8 ticket)))
       get-pure-port
       (compose xml->xexpr document-element read-xml)))
    (match
        resp-xml
      [`(cas:serviceResponse
         ((xmlns:cas "http://www.yale.edu/tp/cas"))
         ,_ ...
         (cas:authenticationSuccess
          ()
          ,@(list-no-order
             `(cas:user () ,user)
             _ ...))
         ,_ ...)
       user]
      [else #f]))
  
  ;; string string bytes -> #f or bytes
  ;; Issues a proxy ticket. Returns the proxy ticket on success or #f
  ;; otherwise.
  ;;
  ;; Arguments:
  ;;  - The CAS provider root URL.
  ;;  - The service name or URL for which the proxy ticket should be issued.
  ;;  - A proxy ticket granting ticket.
  (define (gen-proxy-ticket provider-url service pgt)
    (define resp-xml
      (call/input-url/verify
       (string->url
        (string-append
         provider-url "/proxy?targetService="
         (form-urlencoded-encode service)
         "&pgt=" (bytes->string/utf-8 pgt)))
       get-pure-port
       (compose xml->xexpr document-element read-xml)))
    (match
        resp-xml
      [`(cas:serviceResponse
         ((xmlns:cas "http://www.yale.edu/tp/cas"))
         ,_ ...
         (cas:proxySuccess
          ()
          ,@(list-no-order
             `(cas:proxyTicket () ,proxy-ticket)
             _ ...))
         ,_ ...)
       (string->bytes/utf-8 proxy-ticket)]
      [else #f])))
