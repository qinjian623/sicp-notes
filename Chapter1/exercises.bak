;E1.1
;跳过
;E1.2
(/ (+ 5 4 (- 2 (- 3 (+ 6 (/ 4 5))))) (* 3 (- 6 2) (- 2 7)))
;E1.3
;UGLY
(define (sum-of-two-larger-from-three a b c)
  (if (> a b)
      (if (> b c)
	  (+ a b)
	  (+ a c)
	  )
      (if (> a c)
	  (+ a b)
	  (+ b c)
	  )
      )
  )
;E1.4
过程与数据的统一的结果,函数式编程的重要特征.
;E1.5
应用序可跳出,正则序无限,待确定;ERROR
;注意正则序是在完全展开后再开始求值.应用序会尽快求值么?

;牛顿平方根
(define (sqrt x)
  (sqrt-iter 1.0 x))

(define (sqrt-iter guess x)
  (if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x)
		 x)))

(define (improve guess x)
  (average guess (/ x guess))
(define (average a b)
  (/ (+ a b) 2))

(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.0001))

(define (square x)
  (* x x))

;E1.6
出现的情况是无限递归,与自己猜测一直.
new-if 在判断条件后,依然在自己的作用域内部执行.造成递归情况下的不能跳出.;这一解释不足以说明
但是按说也存在最后运算结束后的跳出?
;E1.7

;E1.8

;E1.9
1.
(+ 4 5)
(inc (+ 3 5))
(inc (inc (+ 2 5)))
......
递归
2.
(+ 4 5)
(+ 3 6)
(+ 2 7)
(+ 1 8)
......
迭代
;E1.10
(A 1 10):
(A 0 (A 1 9))
(A 0 (A 0 (A 1 8) ))
(A 0 (A 0 (A 0 (A 1 7))))
......
(A 0 ...... (A 0 (A 1 1))...)
2**10

(A 2 4):
(A 1 (A 2 3))
(A 1 (A 1 (A 2 2)))
(A 1 (A 1 (A 1 (A 2 1))))

2**16

(A 3 3):
(A 2 (A 3 2))
(A 2 (A 2 (A 3 1)))
(A 2 4)
2**16

f
;n*2
g
;2**n
h
;2**2**2**2...

;实例:换零钱方式的统计

(define (count-change amount)
  (cc amount 5))
(define (cc amount kinds-of-coins)
  (cond ((= amount 0) 1)
	((or (< amount 0) (= kinds-of-coins 0)) 0)
	(else (+ (cc amount
		     (- kinds-of-coins 1))
		 (cc (- amount
			(first-denomination kinds-of-coins))
		     kinds-of-coins)))))
(define (first-denomination kinds-of-coins)
  (cond ((= kinds-of-coins 1) 1)
	((= kinds-of-coins 2) 5)
	((= kinds-of-coins 3) 10)
	((= kinds-of-coins 4) 25)
	((= kinds-of-coins 5) 50)))

;E1.11
(define (f n)
  (if (< n 3)
      n
      (+ (f (- n 1)) 
	 (* 2 (f (- n 2))) 
	 (* 3 (f (- n 3))))))

(define (f n)
  (f-iter 2 1 0 n))

(define (f-iter a b c n)
  (if (= 0 n)
      c
      (f-iter (+ a (* 2 b) (* 3 c))
	      a
	      b
	      (- n 1))))
;E1.12
(define (pascal x y)
  (if (or (= x 0) (= x y)
      1
      (+ (pascal (- x 1) (- y 1) )
	 (pascal x (- y 1))))))
;E1.13

;E1.14
;E1.15
(define (cube x) (* x x x))
(define (p x) (- (* 3 x)(* 4 (cube x))))
(define (sine angle)
  (if (not (> (abs angle) 0.1))
      angle
      (p (sine (/ angle 3.0)))))

;E1.16

(define (fast-expt b n)
  (cond ((= n 0) 1)
	((even? n) (square (fast-expt b (/ n 2))))
	(else (* b (fast-expt b (- n 1))))))
(define (even? n)
  (= (remainder n 2) 0))

;迭代
(define (expt b n)
  (expt-iter b n 1))
(define (expt-iter b n product)
  (cond ((= n 0)
	 product)
	((even? n)
	 (expt-iter (* b b) (/ n 2) product
	 ))
	((odd? n)
	 (expt-iter b
		    (- n 1)
		    (* b product)))))

;E1.17

(define (double x)
  (+ x x))
(define (halve x)
  (if (even? x)
      (/ x 2)
      ()))
;normal
(define (* a b)
  (if (= b 0)
      0
      (+ a (* a (- b 1)))))
;log
(define (fast-* a b)
  (cond ((= b 1) a)
	((even? b) 
	 (fast-* (double a) (halve b)))
	 ;参考的一份答案来源中,doule操作在fast-*外部,感觉这里的是不小心写成了迭代的方法.但是,注意,在下面的分支中,依然还是递归模式.如果一直在这个路径上执行,那么就成为完全迭代.
	(else 
	 (+ a (fast-* a (- b 1))))))
;E1.18

(define (* a b)
  (*-iter a b 0))

(define (*-iter a b r)
  (cond ((= b 1)
	 r)
	((even? b)
	 (*-iter (double a) (halve b) r)
	 )
	((odd? b)
	 (*-iter a
		 (- b 1)
		 (+ a r)
	 ))))
;E1.19
;这里引入了一个代换过程,fib的代换过程是其的一个特例.
;该过程有良好的性质,可以方便类似log级别方法的使用.
(define (fib n)
  (fib-iter 1 0 0 1 n))
(define (fib-iter a b p q count)
  (cond ((= count 0) b)
	((even? count)
	 (fib-iter a
		   b
		   (+ (* p p) (* q q))
		   (+ (* q q) (* 2 p q))
		   (/ count 2)))
	(else (fib-iter (+ (* b q) (* a q) (* a p))
			(+ (* b q) (* a q))
			p
			q
			(- count 1)))))

;E1.20
(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))
;正则序
(gcd 206 40)
(gcd 40 (remainder 206 40))
(gcd (remainder 206 40 ) (remainder 40 (remainder 206 40)))
(gcd (remainder 40 (remainder 206 40)) (remainder (remainder 206 40) (remainder 40 (remainder 206 40))))
......
;增长应该是1,3,6,10次,共20次.
;应用序
(gcd 206 40)
(gcd 40 6)
;4次,参考答案的来源结果是5次,这里应该是4次.看它的运算是有一步多余的.
;E1.21
(define (smallest-divisor n)
  (find-divisor n 2))
(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
	((divides? test-divisor n) test-divisor)
	(else (find-divisor n (+ test-divisor 1)))))

(define (divides? a b)
  (= (remainder b a) 0))
(define (prime? n)
  (= n (smallest-divisor n)))

;E1.22
(define (timed-prime-test n)
;  (newline)
;  (display n)
;  (display (runtime))
  (start-prime-test n (runtime)))
(define (start-prime-test n start-time)
  (if (prime? n)
      (report-prime (- (runtime) start-time))
      (= 1 2)))
(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time)
  (= 1 1))
  

(define (search-for-primes a b)
  (cond ((= b 0) 0)
      (else
       (if (timed-prime-test a)
	   (search-for-primes(+ a 1) (- b 1))
	   (search-for-primes(+ a 1) b)))))

;根据参考答案来源,runtime新版为秒,改为real-time-clock
(define (runtime )
  (real-time-clock))
;简单计算了,貌似比n**1/2的性能差一点.
;E1.23

(define (next n)
  (if (= n 2)
      3
      (+ n 2)
      )
  )

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
	((divides? test-divisor n) test-divisor)
	(else (find-divisor n (next test-divisor)))))
;增长上没有达到1/2的优化量.


;E1.24

(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))

(define (fast-prime? n times)
  (cond ((= times 0) true)
	((fermat-test n)(fast-prime? n (- times 1)))
	(else false)))

(define (expmod base exp m)
  (cond ((= exp 0) 1)
	((even? exp)
	 (remainder (square (expmod base (/ exp 2) m))
		    m))
	(else
	 (remainder (* base (expmod base (- exp 1) m))
		    m))))
(define (start-prime-test n start-time)
  (if (fast-prime? n 100)
      (report-prime (- (runtime) start-time))
      (= 1 2)))

;E1.25

