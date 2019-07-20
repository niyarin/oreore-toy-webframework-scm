(include "./sxml.scm")

(define-library (niyarin otw render)
   (import (scheme base) (scheme write))
   (export otw-render-html);
   (begin
     (define (otw-render-html str out-port)
       (let ((bv (string->utf8 str)))
          (write-bytevector (string->utf8 "HTTP/1.1 200 OK\r\n") out-port)
          (write-bytevector (string->utf8 "Content-Type: text/plain\r\n") out-port)
          (write-bytevector (string->utf8 "Content-Length: ") out-port)
          (write-bytevector (string->utf8
            (number->string
             (bytevector-length bv))) out-port)
          (write-bytevector (string->utf8 "\r\n\r\n") out-port)
          (write-bytevector bv out-port)))
    ))
