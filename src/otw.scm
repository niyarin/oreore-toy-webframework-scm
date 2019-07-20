(include "./otw-render.scm")

(define-library (niyarin otw) 
   (import 
     (scheme base)
     (scheme write)
     (scheme list)
     (niyarin otw render))

   (export otw-let-env otw-dispatcher otw-generate-uri otw-library-test)

   (begin
      (define (otw-create-env)
        (cons 0 '()))

      (define (otw-dispatcher index-page)
        (lambda (out-port method url-qparam . opt)
           (case method
             ((GET)
               (let ((als (cadr url-qparam))
                     (uri (car url-qparam)))
                 (display uri)(display als)(newline)
                 (cond
                   ((assv 'contid als)
                    => (lambda (apair)
                         (cadr apair)
                         ))
                   (else 
                     (index-page 
                       (cons 
                         (list 'port out-port)
                          (cons (list 'url uri) als)))
                     ))
                  ;(otw-render-html "<html><body>HELLO WORLD</body><html>" out-port)
              ))
             (else
               ))))

      (define (otw-proc-generate-url! env cont opt)
         (let ((id (+ (car env) 1))
               (als (cdr env))
               (url-params
                 (cond ((assv 'url-params opt) => cadr)
                       (else '())))
               (base-url 
                 (cond ((assv 'base-url opt) => cadr)
                       (else "../"))))

           (set-car! env id)
           (set-cdr! 
             (cons 
               id
               cont)
             als)
           (string-append 
             base-url "?contid=" (number->string id)
             (fold 
               (lambda (p res)
                 (string-append 
                   res
                   "&"
                   (car p)
                   "="
                   (cadr p)))
               ""
               url-params))))

      (define-syntax otw-let-env
        (syntax-rules ()
            ((_ env-name generate-uri bodies ... )
             (let ((env-name (otw-create-env)))
               (let ((generate-uri 
                       (lambda (cont . opt)
                         (otw-proc-generate-url!
                           env-name
                           cont
                           opt))))
                  bodies ...)))))

     (define (otw-library-test)
       (let ((tmp-env (otw-create-env)))
          (display 
            (otw-proc-generate-url! tmp-env 'test '()))(newline)
          (display
            (otw-proc-generate-url! 
              tmp-env 
              'test2 
              '((base-url "/aaaaa/bbbbb/ccccc.dddd")
                (url-params (("qqqq" "wwww")("eeee" "rrrr"))))))
          (newline)
          ))
      ))


;(import (scheme base)(niyarin otw))
;(otw-library-test)
