;; Racket CAS 2.0 authentication module
;; (C) 2013 Marc Burns, Jennifer Wong
(module cas racket
  (require racket/port
           racket/async-channel
           net/base64
           net/uri-codec
           net/url
           web-server/servlet
           xml
           xml/xexpr)

  (provide initiate-cas-auth
           validate-cas-token/proxyfw)

  ;; -> (string/utf-8 . bytes) or #f
  (define (initiate-cas-auth server-url provider-url)
    (define url-receiver #f)
    (define finish-sema (make-semaphore 1))
    (define token-resp
      (send/suspend
        (lambda(url)
          (set! url-receiver (form-urlencoded-encode
                               (string-append server-url url)))
          (redirect-to
            (string-append
              provider-url "/login?service="
              url-receiver)))))
    (clear-continuation-table!)
    (define bdg (force (request-bindings/raw-promise token-resp)))
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

  ;; -> #f or (string string bytes bytes) = (netid proxy-service pgtId pgtIou
  (define (validate-cas-token/proxyfw server-url provider-url service ticket)
    (define url-pgt #f)
    (define pgt-req-ch (make-async-channel))
    (define finish-sema (make-semaphore 1))
    (printf "dispatch..~n")
    (send/suspend/dispatch
     (lambda(embed/url)
      (set!
        url-pgt
        (form-urlencoded-encode
          (string-append server-url
                         (embed/url (lambda(req) (async-channel-put pgt-req-ch req)
                                                 (send/back (response/xexpr "OK")))))))
      (redirect-to (embed/url identity))))
    (printf "<~n")
    (when (not (semaphore-try-wait? finish-sema))
      (send/back (response/xexpr "Duplicate request.")))
    (printf "k for user, validating service~n")
    (define resp-xml
     (call/input-url
       (string->url
         (string-append
           provider-url "/serviceValidate?service="
           service "&ticket=" (bytes->string/utf-8 ticket)
           "&pgtUrl=" url-pgt))
         get-pure-port
         (compose xml->xexpr document-element read-xml)))
    (printf "got response, waiting on callback~n")
    (define pgt-req (async-channel-get pgt-req-ch))
    (printf "got callback response!~n")

    (printf "~nValidation result:~n")
    (pretty-print resp-xml)
    (printf "~nProxy callback bindings:~n")
    (pretty-print (force (request-bindings/raw-promise pgt-req)))
    (printf "~nProxy request:~n")
    (pretty-print pgt-req)
    (printf "~n")

    (match
      resp-xml
      [`(cas:serviceResponse
          ((xmlns:cas "http://www.yale.edu/tp/cas"))
          ,_ ...
          (cas:authenticationSuccess () ,_ ... (cas:user () ,user) ,_ ...)
          ,_ ...)
        user]
      [else #f])))
