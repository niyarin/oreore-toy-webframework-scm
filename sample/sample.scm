(include "../src/otw.scm")
(include "../src/otw-render.scm")
(include "../src/toy-server.scm")

(import 
  (scheme base)
  (scheme write)
  (niyarin toy-server)
  (niyarin otw)
  (niyarin otw render))

(otw-let-env env otw-generate-uri
   
   (define (cadr-assv key als)
     (cond 
       ((not (list? als)) (error "ERROR:not alist"))
       ((assv key als)
        => cadr)
       (else (error "ERROR" key als))))

   (define (index-page param)
        (let loop ((uname "")
                   (password "")
                   (error-message ""))
          (let* ((loginpage-data
                 (call/cc 
                   (lambda (to-login-cont)
                     (otw-render-view
                       `(html
                          (body
                            (div (@ (style "color:#ff0000"))
                              ,error-message)
                            (form (@ (action ,(otw-generate-uri to-login-cont))
                                     (method "post"))
                              (input (@ (type "text") (name "uname") (value ,uname)))
                              (input (@ (type "password") (name "password")))
                              (input (@ (type "submit") (value "Submit"))))))
                       env)
                     )))
                 (content (cadr-assv 'content loginpage-data)))

               (cond
                 ((not 
                    (otw-validation-not-null?
                    content
                    '(uname password)))
                   (loop (cadr-assv 'uname content)
                        (cadr-assv 'password content)
                        "username or password is empty"))
                 (else 
                   (otw-render-view
                     `(html
                        (body
                          "OK"))
                     param
                   ))))))

   (toy-server-run 
     "8080"
     '(mode develop)
     `(dispatcher 
         ,(otw-dispatcher index-page env))))
