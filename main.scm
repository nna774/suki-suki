(define (return v)
  (cons '() v))

(define (getEnv l)
  (if l
      (car l)
      #f))
(define (getVal l)
  (if l
      (cdr l)
      #f))

(define (set _l v)
  (cons v '()))
(define (get l)
  (cons (getEnv l) (getEnv l)))

(define (bind v f)
  (if v
      (f (car v) (cdr v))
      #f))
(define >>= bind)

(define (bind_ v f)
  (if v
      (f (car v))
      #f))
(define >> bind_)

(define (<or> ms)
  (if (null? ms)
      (lambda (_) #f)
      (lambda (l)
        (let* ((m (car ms)) (v (m l)))
          (if v
              v
              ((<or> (cdr ms)) l)
              )
          )
        )
      ))

(define (then f g)
  (lambda (l) (>>= (f l) g)))
(define (then_ f g)
  (lambda (l) (>> (f l) g)))

(define (eval x)
  x)

(define (readChar l c)
  (if (char=? (car l) c)
      (set l (cdr l))
      #f))

(define (readQuote l)
  (readChar l #\'))

(define (readOpen l)
  (readChar l #\())

(define (readClose l)
  (readChar l #\)))

(define (readNil l)
  (if ((then_ readQuote
              (then_ readOpen readClose)) l)
      (cons
       (cdddr l)
       '()
       )
      #f
      ))

(define (toInt xs)
  (define (toIntImp xs res)
    (if (null? xs)
        res
        (toIntImp (cdr xs) (+ (* res 10) (digit->integer (car xs))))))
  (toIntImp xs 0))

(define (readInt l)
  (let* ((v (takeWhile l char-numeric?)) (n (cdr v)))
    (if (null? n)
        #f
        (cons (car v) (toInt n))
        )))

(define (readSExp l)
  ((<or>
    (list
     readInt
     readNil
     (then_ readQuote readSExp)
     (then_ readOpen readClose)
     ))l))

(define (takeWhile l pred)
  (define (takeWhileImp l pred res)
    (if (pred (car l))
        (takeWhileImp (cdr l) pred (cons
			            (cdr l)
			            (cons (car l)
				          (cdr res))))
        res
        ))
  (let ((res (takeWhileImp l pred (cons l '()))))
    (cons
     (car res) ; rest
     (reverse (cdr res)) ; got
     )))

(define (identifierChar? c)
  (if (char-alphabetic? c)
      #t
      (memq c (list #\! #\? ;; ...
	            ))))

;; (define (readSpaces l)
;;   (let ((c (car list)))
;;     (if (memq c '(#\space #\newline))
;;         (readSpaces ol)
;;         (cons c l)
;;         )))

(define (readAll port)
  (define (readAllImp p res)
    (let ((c (read-char p)))
      (if (eof-object? c)
          res
          (readAllImp p (cons c res))
          )
      ))
  (reverse (readAllImp port '())))

(define (main args)
  (let ((stdin (readAll (standard-input-port))))
    (>>
     (set '() stdin)
     (lambda (x) (print (getVal (readSExp x))))
     )))

;; (define (main z)
;;   (print (takeWhile '(1 2 3 4 5 6 7 8 9 0) (lambda (x) (< x 5))))
;;   )
