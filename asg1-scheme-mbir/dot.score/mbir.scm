#!/afs/cats.ucsc.edu/courses/cse112-wm/usr/racket/bin/mzscheme -qr
;; $Id: mbir.scm,v 1.9 2021-01-12 11:57:59-08 - - $
;;
;; Tony Shao    toshao
;; Brendan Teo  bteo1
;; NAME
;;    mbir.scm filename.mbir
;;
;; SYNOPSIS
;;    mbir.scm - mini basic interper
;;
;; DESCRIPTION
;;    The file mentioned in argv[1] is read and assumed to be an mbir
;;    program, which is the executed.  Currently it is only printed.
;;

(define *DEBUG* #f)
(define *STDIN* (current-input-port))
(define *STDOUT* (current-output-port))
(define *STDERR* (current-error-port))
(define *ARG-LIST* (vector->list (current-command-line-arguments)))

(define *stmt-table*     (make-hash))
(define *function-table* (make-hash))

;;simplify
(for-each
    (lambda (pair)
        (hash-set! *function-table* (car pair) (cadr pair)))
    `(
        (+ , +) 
           (- , -) 
           (* , *)
           (/ , /)
           (% , (lambda (x y) (- (+ x 0.0) 
            (* (truncate (/ (+ x 0.0) (+ y 0.0))) (+ y 0.0)))))
           (^ , expt)
           ;; relop
           (= , equal?)
           (< , <)
           (> , >)
           (!= , (lambda (x y) (not (equal? x y))))
           (>= , >=)
           (<= , <=)
           ;; math functions 
           (exp , exp)
           (ceil , ceiling)
           (floor , floor)
           (sqrt , sqrt)
           (abs , abs)
           (acos , acos)
           (asin , asin)
           (atan , atan)
           (cos , cos)
           (round , round)
           (sin , sin)
           (tan , tan)
           (trunc , truncate)
           (log , log)
           (log10 , (lambda (x) (/ (log (+ x 0.0)) (log 10.0))))
           (log2 , (lambda (x) (/ (log (+ x 0.0)) (log 2.0))))
      )
)

(define *array-table*    (make-hash))

(define *label-table*    (make-hash))

(define *var-table*      (make-hash))
  
(for-each (lambda (pair) (hash-set! *var-table* (car pair) (cadr pair)))
   `(
        (e    ,(exp 1.0))
        (eof  , 0.0)
        (nan  ,(/ 0.0 0.0))
        (pi   ,(acos -1.0))
    )
)

(define NAN (/ 0.0 0.0))

(define *RUN-FILE*
    (let-values
        (((dirname basename dir?)
            (split-path (find-system-path 'run-file))))
        (path->string basename)))

(define (die list)
    (for-each (lambda (item) (fprintf *STDERR* "~a " item)) list)
    (fprintf *STDERR* "~n")
    (when (not *DEBUG*) (exit 1)))

(define (dump . args)
    (when *DEBUG*
        (printf "DEBUG:")
        (for-each (lambda (arg) (printf " ~s" arg)) args)
        (printf "~n")))

(define (usage-exit)
    (die `("Usage: " ,*RUN-FILE* " [-d] filename")))

(define (line-number line)
    (car line))

(define (line-label line)
    (let ((tail (cdr line)))
         (and (not (null? tail))
              (symbol? (car tail))
              (car tail))))

(define (line-stmt  line)
    (let ((tail (cdr line)))
         (cond ((null? tail) #f)
               ((pair? (car tail)) (car tail))
               ((null? (cdr tail)) #f)
               (else (cadr tail)))))

(define (not-implemented function args . nl)
    (printf "(NOT-IMPLEMENTED: ~s ~s)" function args)
    (when (not (null? nl)) (printf "~n")))

;;simplify
(define (eval-expr expr)
    (cond 
    ((number? expr) (+ expr 0.0))
    ;; if variable return the variable's value
    ((symbol? expr) (hash-ref *var-table* expr 0.0))
    ;; if a pair, then either an array or function
    ((pair? expr)
    ;; if function return the function's val applied to arg(s)
    (if (hash-has-key? *function-table* (car expr))
        (+ (apply (hash-ref *function-table* (car expr)) 
               (map eval-expr (cdr expr))) 0.0)
        ;; else if variable, return vector else return error
        (if (hash-has-key? *var-table* (car expr))
            ;; return vector subscript value
            (vector-ref (hash-ref *var-table* (car expr)) 
              (- (exact-round(eval-expr (cadr expr))) 1))
                (die '(Invalid expression)))))))

;;simplify
(define (interp-dim args continuation)
    (printf "interp-dim: expr: ~a ~n" (cadr (car args)))
    (printf "interp-dim: expr: ~a ~n" (caddr (car args)))
    (hash-set! *var-table* (caddr (car args)) 
    (make-vector (abs (exact-round (eval-expr (cadr (car args)))))))
    (interp-program continuation))

;;simplify
(define (interp-let args continuation)
    ; (printf "interp-let: expr: ~a ~n" args)
    (if (pair? (car args))
        (begin
            (printf "interp-let: got in case if pair? (car expr) ~n")
            (vector-set! (hash-ref *var-table*
                (caar expr)) (- (args-expr (cadar args)) 1) 
                 (eval-expr (cadr args))))
    (begin
        ;(printf "interp-let: in case else ~n")
        (let ((result (eval-expr (cadr args))))
           (hash-set! *var-table* (car args) result)
           ;(printf "interp-let: result: ~a, 
           ;(car expr): ~a ~n" result (car expr))
           ;(printf "interp-let: result: ~a, 
           ;(cadr expr): ~a ~n" result (cadr expr))
        )))
    (interp-program continuation)
    )


(define (interp-goto args continuation)
    ; (printf "interp-goto: args: ~a ~n" args)
    ; (printf "hash-table ~a ~n" *label-table*)
    (if (hash-has-key? *label-table* (car args))
      (interp-program (hash-ref *label-table* (car args)))
      (die '("Error: jump to undeclared label.")))
    )


(define (interp-if args continuation)
    ; (printf "interp-if: args: ~a ~n" args)
    (if ((hash-ref *function-table* (car(car args)))
            (eval-expr (cadr (car args))) 
                (eval-expr (caddr (car args))))
        (interp-goto (cdr args) continuation)
        (interp-program continuation)
    )
)

(define (interp-print args continuation)
    (define (print item)
        (if (string? item)
            (printf "~a" item)
            (printf " ~a" (eval-expr item))))
    (for-each print args)
    (printf "~n");
    (interp-program continuation))

(define (interp-input args continuation)
    ; (printf "interp-input: args: ~a ~n" (car args))
    (if (not (null? args))
        (begin
        (let ((object (read)))
            (cond 
                [(eof-object? object) 
                (begin
                    (hash-set! *var-table* eof 1.0)
                    (hash-set! *var-table* (car args) NAN)
                    (exit 1))]
                [(number? object)
                    (hash-set! 
                        *var-table* (car args) (eval-expr object))]
                [(pair? object) (
                    (when (and (hash-has-key? *var-table* 
                        (car args)) 
                            (<= (- (eval-expr (cadr (car args))) 1)
                        (vector-length (car (hash-ref *var-table* 
                         (car args))))))
                    ;; set vector index to new value
                    (begin 
                        (vector-set! (hash-ref *var-table* 
                        (car (car args))) 
                        (- (eval-expr (cdr (car args))) 1) x))))]
                [else 
                    (begin
                    (printf "Error: invalid expression.~n")
                    (hash-set! *var-table* (car args) NAN)
                    (exit 1))] ))
        (interp-input (cdr args) continuation))
        (interp-program continuation)
        )
    ; (printf "var-table ~a ~n" *var-table*)
    )

(for-each (lambda (fn) (hash-set! *stmt-table* (car fn) (cadr fn)))
   `(
        (dim   ,interp-dim)
        (let   ,interp-let)
        (goto  ,interp-goto)
        (if    ,interp-if)
        (print ,interp-print)
        (input ,interp-input)
    ))

(define (interp-program program)
    (when (not (null? program))
          (let ((line (line-stmt (car program)))
                    (continuation (cdr program)))
               (if line
                   (let ((func (hash-ref *stmt-table* (car line) #f)))
                        (func (cdr line) continuation))
                   (interp-program continuation)))))

(define (scan-for-labels program)
        (when (not (null? program))
                (when (not (equal? (length (car program)) 1))
                        (when (symbol? (cadr (car program)))
                                (hash-set! *label-table* 
                                    (cadr(car program)) program)
                        )   
                )   
                (scan-for-labels (cdr program) )
        )   
)

(define (readlist filename)
    (let ((inputfile (open-input-file filename)))
         (if (not (input-port? inputfile))
             (die `(,*RUN-FILE* ": " ,filename ": open failed"))
             (let ((program (read inputfile)))
                  (close-input-port inputfile)
                         program))))

(define (dump-program filename program)
    (define (dump-line line)
        (dump (line-number line) (line-label line) (line-stmt line)))
    (dump *RUN-FILE* *DEBUG* filename)
    (dump program)
    (for-each (lambda (line) (dump-line line)) program))

(define (main arglist)
    (cond ((null? arglist)
                (usage-exit))
          ((string=? (car arglist) "-d")
                (set! *DEBUG* #t)
                (printf "~a: ~s~n" *RUN-FILE* *ARG-LIST*)
                (main (cdr arglist)))
          ((not (null? (cdr  arglist)))
                (usage-exit))
          (else (let* ((mbprogfile (car arglist))
                       (program (readlist mbprogfile)))
                (begin (when *DEBUG* (dump-program mbprogfile program))
                       (scan-for-labels program)
                       (interp-program program))))))
(main *ARG-LIST*)