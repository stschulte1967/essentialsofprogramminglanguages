(define in-S?
  (lambda (n)
    (if (zero? n) #t
      (if (>= (- n 3) 0)
        (in-S? (- n 3))
	#f))))

(define list-length
  (lambda (lst)
    (if (null? lst)
      0
      (+ 1 (list-length (cdr lst))))))

(define nth-element
  (lambda (lst n)
    (if (null? lst)
      (error "Error: lst is null")
      (if (zero? n)
        (car lst)
        (nth-element (cdr lst) (- n 1))))))

(define remove-first
  (lambda (e lst)
    (if (null? lst)
      '()
      (if (eqv? (car lst) e)
	(cdr lst)
        (cons (car lst) (remove-first e (cdr lst)))))))

(define remove
  (lambda (e lst)
    (if (null? lst)
      '()
      (if (eqv? (car lst) e)
        (remove e (cdr lst))
        (cons (car lst) (remove e (cdr lst)))))))

(define occurs-free?
  (lambda (var expr)
    (cond 
      ((symbol? expr) (eqv? var expr))
      ((eqv? (car expr) 'lambda)
        (and
          (not (eqv? var (car (cadr expr))))
          (occurs-free? var (caddr expr))))
      (else
        (or
          (occurs-free? var (car expr))
          (occurs-free? var (cadr expr)))))))

(define subst-in-s-expr
  (lambda (new old sexpr)
    (if (symbol? sexpr)
	(if (eqv? sexpr old) new sexpr)
        (subst new old sexpr))))

(define subst
  (lambda (new old slist)
    (if (null? slist) 
      '()
      (cons
        (subst-in-s-expr new old (car slist))
        (subst new old (cdr slist))))))
