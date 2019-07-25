(include "./sxml.scm")
(include "./otw.scm")

(define-library (niyarin otw render)
   (import 
     (scheme base) 
     (scheme write)
     (scheme cxr) 
     (niyarin sxml) 
     (niyarin otw))

   (export otw-render-html otw-render-view);
   (begin
     (define (otw-render-html str out-port)
       (let ((bv (string->utf8 str)))
          (write-bytevector (string->utf8 "HTTP/1.1 200 OK\r\n") out-port)
          (write-bytevector (string->utf8 "Content-Type: text/html\r\n") out-port)
          (write-bytevector (string->utf8 "Content-Length: ") out-port)
          (write-bytevector (string->utf8
            (number->string
             (bytevector-length bv))) out-port)
          (write-bytevector (string->utf8 "\r\n\r\n") out-port)
          (write-bytevector bv out-port)))

     (define (otw-render-view sxml env)
       (let ((out-port (otw-env-ref-port env))
             (exit-page (otw-env-ref-exit env)))
          (otw-render-html (sxml->xml-string sxml) out-port)
          (exit-page)))

    ))
