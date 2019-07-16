;using Riastradh's shift/reset

(define-library (niyarin shift-reset)
   (import (scheme base))
   (export niyarin-shift-reset/shift
           niyarin-shift-reset/reset)

   (begin

     (define (*meta-continuation* v)
       (error "No reset" v))

     (define-syntax niyarin-shift-reset/shift
         (syntax-rules ()
            ((_ var body)
               (call/cc
                 (lambda (k)
                   (let ((res
                           (let ((var
                                   (lambda (v) 
                                     (niyarin-shift-reset/reset (k v)))))
                             body)))
                     (*meta-continuation* res)))))))

     (define-syntax niyarin-shift-reset/reset
         (syntax-rules ()
            ((_ body)
             (let ((mc *meta-continuation*))
               (call/cc
                 (lambda (k)
                   (set! *meta-continuation*
                         (lambda (v)
                           (set! *meta-continuation* mc)
                           (k v)))
                   (let ((res body))
                     (*meta-continuation* res))))))))
     ))
   
