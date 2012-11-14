
					;实例2.1.1
(define (add-rat x y)
  (make-rat (+ (* (number x) (denom y))
               (* (number y) (denom x)))
            (* (denom x) (denom y))))

(define (sub-rat x y)
  (make-rat (- (* (number x) (denom y))
               (* (number y) (denom x)))
            (* (denom x) (denom y))))

(define (mul-rat x y)
  (make-rat (* (number x) (number y))
            (* (denom x) (denom y))))

(define (div-rat x y)
  (make-rat (* (number x) (denom y))
            (* (denom x) (number y))))

(define (equal-rat? x y)
  (= (* (number x) (denom y))
     (* (number y) (denom x))))

(define (make-rat n d) (cons n d))
(define (number x)(car x))
(define (denom x) (cdr x))
(define (print-rat x)
  (newline)
  (display (number x))
  (display "/")
  (display (denom x)))

(define (make-rat n d)
  (let ((g (gcd n d)))
    (cons (/ n g) (/ d g))))
(define (make-rat n d)
  (let ((g (gcd n d)))
    (cons (/ n g) (/ d g))))
					;E 2.1
(define (make-rat n d)
  (let ((g (gcd n d)))
    (if (< (* n d) 0)
        (cons (- 0 (abs (/ n g))) (abs (/ d g)))
        (cons (abs (/ n g)) (abs (/ d g))))))




(define a (make-rat 3 -10))
(define b (make-rat 1 10))
(print-rat (add-rat a b))
(add-rat a b)

					;E 2.2

(define (make-segment p0 p1)
  (cons p0 p1))

(define (start-segment seg)
  (car seg))
(define (end-segment seg)
  (cdr seg))

(define (make-point x y)
  (cons x y))
(define (x-point p)
  (car p))
(define (y-point p)
  (cdr p))

(define (midpoint-segment seg)
  (let ((p0 (start-segment seg))
        (p1 (end-segment seg)))
    (make-point (/ (+ (x-point p0) (x-point p1)) 2)
                (/ (+ (y-point p0) (y-point p1)) 2))))

(midpoint-segment (make-segment (make-point 1 5) (make-point 5 13)))

					;E 2.3
					;定义获得长宽的方法，计算
					;方法，可以是4线段，两点等等表示方法。

					;E 2.4
					;Church Encode 的典型

					;E 2.5
(define (cons a b)
  (* (expt 2 a)
     (expt 3 b)))
(define (car z)
  (if (= 0 (remainder z 2))
      (+ 1 (car (/ z 2)))
      0))
(define (cdr z)
  (if (= 0 (remainder z 3))
      (+ 1 (cdr (/ z 3)))
      0))
(cdr (cons 3 5))

					;2.6 
					;邱奇数


					;E 2.7
(define (upper-bound x)
  (car x))
(define (lower-bound x)
  (cdr x))

					;E 2.8
(define (sub-interval a b)
  (make-interval (
                  (- (lower-bound a) (upper-bound b))
                  (- (uppper-bound a) (lower-bound b)))))





					;List Ops 

(define (list-ref items n)
  (if (= n 0)
      (car items)
      (list-ref (cdr items) (- n 1))))

(define (length items)
  (if (null? items)
      0
      (+ 1 (length (cdr items)))))

(define (append l1 l2)
  (if (null? l1)
      l2
      (cons (car l1) (append (cdr l1) l2))))

(append '(1 2 3) '(2 9 10))

(cdr '(1))
(car '(1))
					;E 2.17 改用if出问题，为什么？
(define (last-pair lst)
  (cond ((null? lst)
	 (error "list empty -- LAST-PAIR"))
	((null? (cdr lst))
	 lst)
	(else
	 (last-pair (cdr lst)))))
					;E 2.18
(define (reverse l)
  (if (null? l)
      '()
      (append (reverse (cdr l)) (list (car l)))))

					;E 2.19


					;E 2.20 参考来源是用filter处理的，感觉作弊呢。
(define (iter z first newlist)
  (if (null? z)
      newlist
      (if (same first (car z))
	  (iter (cdr z) first (append newlist (list(car z))))
	  (iter (cdr z) first newlist)
	  ))
  )

(define (same a b)
  (or (and (even? a) (even? b))
      (and (odd? a) (odd? b))
      ))

(define (same-parity .z)
  (iter (cdr z) (car z) '()))

					;E 2.21
(define (square-list items)
  (if (null? items)
      '()
      (cons (square (car items))
	    (square-list (cdr items)))))
(define (square-list items)
  (map (lambda (x) (* x x)) items))

					;E 2.22
					;按照顺序求值然后插入的结果
					;cons的更换顺序无效，把前面的list当做一个单元处理了。

					;E 2.23
(define (for-each func li)
  (if (null? li)
      '()
      (begin
	(func (car li))
	(for-each func (cdr li)))
      )
  )

;E 2.24
(1 (2 (3 4)))
;E 2.25
d d a d a
a a
d a d a dadada...
;E 2.26
(1 2 3 4 5 6)
((1 2 3) 4 5 6)
((1 2 3) (4 5 6))
;E 2.27

;二叉树版本
(define (deep-reverse l)
  (cond ((null? l) )
	((not (pair? l)) l)
	(else (list (deep-reverse (cadr l))
		      (deep-reverse (car l))
	       ))))
;多叉树
(define (eli-deep-reverse lst) 
  (cond ((null? lst) nil) 
	((pair? (car lst)) 
	 (append 
	  (eli-deep-reverse (cdr lst)) 
	  (list (eli-deep-reverse (car lst))))) 
	(else 
	 (append 
	  (eli-deep-reverse (cdr lst)) 
	  (list (car lst)))))) 

;E 2.28
(define (count-leaves x)
  (cond ((null? x) 0)
	((not (pair? x)) 1)
	(else (+ (count-leaves (car x))
		 (count-leaves (cdr x))))))

(define (fringe x)
  (cond ((null? x) )
	((not (pair? x)) x)
	(else (append
		(fringe (car x))
		(fringe (cdr x))))))
(define (eli-deep-reverse lst) 
  (cond ((null? lst) nil) 
	((pair? (car lst)) 
	 (append 
	  (list (eli-deep-reverse (car lis)))
	  (eli-deep-reverse (cdr lst)))) 
	(else 
	 (append 
	  (list (car lst))
	  (eli-deep-reverse (cdr lst))))))
