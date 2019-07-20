(define-library (niyarin toy-server)
   (import (scheme base)
           (scheme regex)
           (scheme write)
           (scheme charset)
           (srfi 18);multi thread
           (srfi 106);socket
           )
   (export toy-server-run toy-server-library-test)
   (begin

     (define (toy-server-ok-develop e)
       (display "ERROR")(display e)(newline)
       )

     (define (toy-server-500-release e)
       (display "ERROR")(display e)(newline)
       )

     (define (toy-server-decompose-query-parameter query-string)
       (let loop ((index 0)
                  (sindex 0)
                  (state #t)
                  (left #f)
                  (res '()))
         (cond
           ((= (string-length query-string) index) 
            (cons 
              (cons
                left
                (substring query-string sindex index))
              res))

           ((and state 
                 (char=? 
                      (string-ref query-string index) 
                      #\=))
            (loop 
              (+ index 1) 
              (+ index 1) 
              #f
              (substring query-string sindex index)
              res))
           ((and (not state)
                 (char=?
                   (string-ref query-string index)
                   #\&))
            (loop
              (+ index 1)
              (+ index 1)
              #t
              left
              (cons
                 (cons 
                   left
                   (substring query-string sindex index))
                 res)))
           (else 
             (loop
               (+ index 1)
               sindex
               state
               left
               res)))))

     (define GET-MATCH-PATTERN
         `(: "GET" (+ space)
             (-> path 
                 (+ 
                   ,(char-set-adjoin 
                      char-set:letter+digit
                      #\- #\_ #\, #\.  #\!  #\/ #\' #\( #\) #\*)
                   ))
             (? "?" (-> param (* any))) (+ space)
             (* any)))

     (define (toy-server-get line)
       (let ((matches
                (regexp-matches
                  GET-MATCH-PATTERN
                  line)
                ))
        (cond 
           (matches
             (list
               (regexp-match-submatch matches 'path)
               (cond 
                 ((regexp-match-submatch matches 'param) => 
                     (lambda (x) 
                        (toy-server-decompose-query-parameter x)))
                 (else '()))))
           (else #f)
         )))

     (define (toy-server-read-first in-port)
       (let ((output-port (open-output-string)))
         (let loop ()
           (let ((c (read-u8 in-port)))
             (if (= c 13)
               (get-output-string output-port)
               (begin
                 (write-char (integer->char c) output-port)
                 (loop)))))))

     (define (toy-server-get-http-method http-first-line)
       (letrec-syntax
         ((cls-case 
            (syntax-rules ( else)
              ((_ nums input (else res) dusts ... )
               res)

              ((_ nums input)
               #f)

              ((_ (n1 ...) input (method-char-ls res) next ... )
               (cls-case "AUX" (n1 ...) (n1 ...) input method-char-ls res () next ...))
              ((_ "AUX" nums-org nums-using input () res tests next ...)
               (if (and . tests) 
                 res
                 (cls-case nums-org input next ...)))

              ((_ "AUX" nums-org (n1 n2 ...) input(c1 c2 ...) res (tests ...)  next ...)
               (cls-case 
                 "AUX"
                 nums-org
                 (n2 ...)
                 input
                 (c2 ... ) 
                 res 
                 ((char=? c1 (string-ref input n1)) tests ...)
                 next ... ))
              )))

         (cls-case
           (0 1 2 3 4 5)
           http-first-line
           ((#\G #\E #\T) 'GET)
           ((#\P #\O #\S #\T) 'POST)
           (else (error "UNDEFINED HTTP METHOD :" http-first-line))
           )))

     ;;
     (define (toy-server-listen socket . opt)
         (let  ((in-port (socket-input-port socket))
                (out-port (socket-input-port socket))
                (mode 
                  (cond 
                    ((assv 'mode opt) =>
                        cadr)
                    (else 'release)))
                (dispatcher
                  (cond
                     ((assv 'dispatcher opt) => 
                        cadr)
                     (else 
                       (lambda x 
                         (display x)(newline)
                         x)))))
           (guard 
             (e 
               ((eq? mode 'develop)
                (toy-server-500-develop e))
               (else
                (toy-server-500-release e)))
             (let* ((first-line
                      (toy-server-read-first in-port))
                    (http-method
                      (toy-server-get-http-method first-line)))
               (display first-line)(newline)
               (case http-method
                 ((GET) 
                  (dispatcher 'GET (toy-server-get first-line)))
                 (else
                   (error "ERROR")))))))

     ;;starting server function 
     (define (toy-server-run port)
       (begin
          (display "Serving HTTP on 0.0.0.0 port ") 
          (display port) 
          (newline))
       (let ((server-socket (make-server-socket port)))
          (let loop ()
            (let ((socket (socket-accept server-socket)))
              (thread-start! 
                (make-thread
                  (lambda ()
                    (toy-server-listen
                      socket))))
              (loop)))))

     (define (toy-server-library-test)
         (display (toy-server-get-http-method "GET /index?abc=123&def=456"))(newline)
         (display (toy-server-get-http-method "POST /index HTTP/1.1"))(newline)
       )
     ))
