(include "./otw-render.scm")

(define-library (niyarin otw) 
   (import 
     (scheme base)
     (scheme write)
     (scheme list)
     (niyarin otw render))

   (export otw-let-env otw-dispatcher otw-generate-uri otw-library-test otw-validation-not-null?)

   (begin
      (define (otw-create-env)
        (cons 0 '()))

      (define (otw-dispatcher index-page env)
        (lambda (out-port method url-qparam . opt)
          (call/cc 
            (lambda (exit)
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
                          (list 
                            (list 'port out-port)
                            (list 'url uri) 
                            (list 'exit exit)
                            (list 'param als))))
                        )))
                ((POST)
                 (let ((als (cadr url-qparam))
                       (uri (car url-qparam)))
                   (cond 
                     ((assv 'contid als)
                      =>  (lambda (apair)
                            (let* ((contid (string->number (cadr apair)))
                                   (cont-apair (assv contid (cdr env))))
                               ((cdr cont-apair)
                                (list
                                   (list 'port out-port)
                                   (list 'url uri) 
                                   (list 'exit exit)
                                   (list
                                      'content
                                         (cadr (assv 'content opt)))
                                   (list 'param als))))))
                                      
                     (else 
                       ;TODO:まだ
                       (index-page
                         (cons 
                           (list 'port out-port)
                           (cons (list 'url uri) als)))))))
                (else
                  ))))))

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
             env
             (cons
                (cons 
                  id
                  cont)
                als))
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

     (define (otw-validation-not-null? alist keys)
       (display alist)(newline)
       (fold
         (lambda (key res)
           (cond 
             ((not res) #f)
             ((assv key alist)
              => (lambda (apair)
                   (zero?
                     (string-length 
                        (cadr apair)))))
             (else 
               #f)))
         #t
         keys))

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
