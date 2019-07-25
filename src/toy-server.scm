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

     (define (toy-server-500-develop e)
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
              (list
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
              (string->symbol (substring query-string sindex index))
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
                 (list 
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

    (define (otw-generate-first-line-pattern mode)
      `(: ,mode (+ space)
          (-> path 
              (+ 
                ,(char-set-adjoin 
                   char-set:letter+digit
                   #\- #\_ #\, #\.  #\!  #\/ #\' #\( #\) #\*)
                ))
          (? "?" (-> param (* any))) (+ space)
          (* any)))

     (define GET-MATCH-PATTERN
         (otw-generate-first-line-pattern "GET"))

    (define POST-MATCH-PATTERN
         (otw-generate-first-line-pattern "POST"))

     (define (toy-server-match-first-line pattern line)
       (let ((matches
                (regexp-matches
                  pattern
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
           (else #f))))

     (define (toy-server-read-first in-port)
       (let ((output-port (open-output-string)))
         (let loop ()
           (let ((c (read-u8 in-port)))
             (if (= c 13)
               (begin
                  (read-u8 in-port)
                  (get-output-string output-port))
               (begin
                 (write-char (integer->char c) output-port)
                 (loop)))))))

     (define (toy-server-match-contents-line line)
       (regexp-matches 
         '(: "Content-Length: " (-> length (+ numeric)))
          line))

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
     (define (toy-server-listen socket opt)
         (let  ((in-port (socket-input-port socket))
                (out-port (socket-output-port socket))
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

               (case http-method
                 ((GET) 
                   (dispatcher out-port 'GET (toy-server-match-first-line GET-MATCH-PATTERN first-line))
                   (close-port in-port)
                   (close-port out-port)
                   (socket-shutdown socket *shut-rdwr*)
                   (socket-close socket))
                 ((POST)
                  (let ((uri (toy-server-match-first-line POST-MATCH-PATTERN first-line)))
                     (let* ((len
                              (let loop ((line (toy-server-read-first in-port))
                                         (res 0))
                                (cond 
                                  ((zero? (string-length line)) res)
                                  ((toy-server-match-contents-line line)
                                   => 
                                   (lambda (matcher)
                                      (loop (toy-server-read-first in-port)
                                            (string->number 
                                              (regexp-match-submatch 
                                                matcher
                                                'length))
                                            )))
                                  (else (loop (toy-server-read-first in-port) res)))))
                            (content
                              (let ((out-str-port (open-output-string)))
                                 (let loop ((i 0))
                                   (if (= i len)
                                      (get-output-string out-str-port)
                                      (begin
                                         (write-char 
                                           (integer->char (read-u8  in-port))
                                           out-str-port)
                                         (loop (+ i 1) ))))))
                            
                            (content-decomposed 
                              (toy-server-decompose-query-parameter content)))
                       (begin;body
                         (dispatcher out-port 'POST uri `(content ,content-decomposed))
                         (close-port in-port)
                         (close-port out-port)
                         (socket-shutdown socket *shut-rdwr*)
                         (socket-close socket)
                       ))))
                 (else
                   (error "ERROR:Undefined http method")))))))

     ;;starting server function 
     (define (toy-server-run port . opt)
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
                      socket opt))))
              (loop)))))

     (define (toy-server-library-test)
         (display (toy-server-get-http-method "GET /index?abc=123&def=456"))(newline)
         (display (toy-server-get-http-method "POST /index HTTP/1.1"))(newline)
         (display 
           (regexp-match-submatch
              (toy-server-match-contents-line "Content-Length: 18")
              'length
              ))(newline))
     ))


;(import (scheme base)(niyarin toy-server))
;(toy-server-library-test)
