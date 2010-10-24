(/
 (+ 5
    4
    (- 2
       (- 3
          (+ 6
             4/5))))
 (* 3
    (- 6 2)
    (- 2 7)))

(def a 3)
(def b 4)
(= a b)
(if (and (> b a)
         (> b
            (* a b)))
  b
  a)

(cond (= a 4) 6
      (= b 4) (+ 6 7 a)
      :else -1)

(+ 2 (if (> )))

(defn square
  [i]
  (* i i))

(defn sum
  [xs]
  (reduce + xs))

(defn sum-of-squares
  [xs]
  (sum (map square xs)))

(defn sum-of-2-max
  [& xs]
  (sum-of-squares (take 2 (sort > xs))))

(defn sumofsquares [& a]
  (sort > a)
  (reduce +
          (map
           (fn [i]
             (* i i))
           (rest (sort a)))))

(sumofsquares 5 4 6)

((if true - +) 1 2)

(defn good-enough?
  [guess x]
  (< (Math/abs
      (- (square guess)
         x))
     0.001))

;;Exercise 1.6
;;It's to do with the application of operands (applicative order),
;;not with how new-if returns results of the expressions. The example (new-if (= 2 3) 0 5
;;works because the operands are atoms. With compound expressions, the
;;interpreter has to expand the combination (irrespective of which
;;expression it returns the result of) causing it to enter an
;;infinite loop.


;;Exercise 1.7
;;
;;It doesn't work with small numbers because the margin of error is
;;only 0.001. With large numbers, there isn't enough precision for the
;;good enough test to ever pass (and thus would iterate forever). e.g:
;; 
;; sqrt 2,000,000,000,000
;;
;;it stops refining answer after iteration 26, which, is still
;;outside of an acceptable range for good-enough?: there isn't enough
;;precision to continue to refine, causing an infinite loop.
;; 
;; Rewrite good-enough + collect change between interations

(defn good-enough?
  [guess prev-guess]
  (< (Math/abs (/ (- prev-guess
                     guess)
                  guess))
     0.0001))

(defn average
  [x y]
  (/ (+ x
        y)
     2))

(defn improve
  [guess x]
  (average guess
           (/ x
              guess)))

(defn sqrt-iter
  [guess x prev-guess]
  (if (good-enough? guess
                    prev-guess)
    guess
    (recur (improve guess x) x guess)))

(defn sqrt
  [x]
  (sqrt-iter 1.0 x 0))


;;Exercise 1.8
;;Use Newton's method to find cube roots
(defn improve-cube
  [guess x]
  (/ (+ (/ x (square guess))
        (* 2 guess))
     3))

(defn cuberoot-iter
  [guess x prev-guess]
  (if (good-enough? guess
                    prev-guess)
    guess
    (recur (improve-cube guess x) x guess)))

(defn cuberoot
  [x]
  (cuberoot-iter 1.0 x 0))

;;Exercise 1.9
;;Each procedure adds 2 numbers together, using the substitution model
;;illustrate the process generated. Are the processes iterative or
;;recursive?

(defn plus-a
  [a b]
  (if (= a 0)
    b
    (inc (plus-a (dec a) b))))

;;e.g. (plus-a 3 2) :
;;
;;(inc (plus-a 2 2))
;;(inc (inc (plus-a 1 2)))
;;(inc (inc (inc plus-a 0 2)))
;;(inc (inc (inc 2)))
;;(inc (inc 3))
;;(inc 4)
;;5

;; this recursive procedure generates a recursive process


(defn plus-b
  [a b n]
  (if (= a 0)
    b
    (recur (dec a) (inc b))))

;;e.g. (plus-b 3 2)
;;
;;(plus-b 3 2)
;;(plus-b 2 3)
;;(plus-b 1 4)
;;(plus-b 0 5)
;;5

;; this recursive procedure generates an iterative process.

;;Example 1.10
;;Ackermann's function:
;;http://en.wikipedia.org/wiki/Ackermann_function
;;

(defn ackermann
  [x y]
  (cond (= y 0) 0
        (= x 0) (* 2 y)
        (= y 1) 2
        :else (ackermann (- x 1)
                         (ackermann x (- y 1)))))

(defn ackermann-f
  [n]
  (ackermann 0 n))

;; (ackermann-f 3)
;; 6
;; (ackermann-f 5)
;; 10
;;
;; behaviour is 2n

(defn ackermann-g
  [n]
  (ackermann 1 n))

;; (ackermann-g 3)
;; 8
;; (ackermann-g 5)
;; 32
;;
;; behaviour is 2^n
;;
;;(ackermann 1 3)
;;(ackermann 0 (ackermann 1 2))
;;(ackermann 0 (ackermann 0 (ackermann 1 1)))
;;(ackermann 0 (ackermann 0 2))
;;(ackermann 0 4)
;;8

(defn ackermann-h
  [n]
  (ackermann 2 n))

;;(ackermann-h 0)
;;0
;;(ackermann-h 1)
;;2
;;(ackermann-h 2)
;;4
;;(ackermann-h 3)
;;16
;;(ackermann-h 4)
;;65536
;;(ackermann-h 5)
;;java.lang.StackOverflowError
;;
;;(ackermann-h 3)
;;(ackermann 2 3)
;;(ackermann 1 (ackermann 2 2))
;;(ackermann 1 (ackermann 1 (ackermann 2 1)))
;;(ackermann 1 (ackermann 1 2))
;;(ackermann 1 (ackermann 0 (ackermann 1 1)))
;;(ackermann 1 (ackermann 0 2))
;;(ackermann 1 4)
;;(ackermann 0 (ackermann 1 3))
;;(ackermann 0 (ackermann 0 (ackermann 1 2)))
;;(ackermann 0 (ackermann 0 (ackermann 0 (ackermann 1 1))))
;;(ackermann 0 (ackermann 0 (ackermann 0 2)))
;;(ackermann 0 (ackermann 0 4))
;;(ackermann 0 8)
;;16
;;phew!
;;
;;http://en.wikipedia.org/wiki/Tetration
;;2^^n
;;2^(2^2)
;;2^2 for n = 2
;;2^(2^2) for n = 3
;;2^(2^(2^2)) for n = 4


;;1.2.2 Tree Recursion

;; tree-recursive fibonacci
(defn fib-recur
  [n]
  (cond (= n 0) 0
        (= n 1) 1
        :else (+ (fib-recur (- n 1))
                 (fib-recur (- n 2)))))

;;linear iteration fibonacci
(defn fib-iter
  ([n]
     (fib-iter 1 0 n))
  ([a b count]
     (if (= count 0)
       b
       (recur (+ a b)
              a
              (- count 1)))))



;;Example 1.11
;;A function f is defined by the rule that f(n) = n if n<3 and
;;f(n) = f(n - 1) + 2f(n - 2) + 3f(n - 3) if n> 3. Write a procedure
;;that computes f by means of a recursive process.
;;Write a procedure that computes f by means of an iterative process.

(defn f
  [n]
  (if (< n 3)
    n
    (+ (f (- n 1))
       (* 2
          (f (- n 2)))
       (* 3
          (f (- n 3))))))

;;;a = f-3
;;;b = f-2
;;;c = f-1
(defn f-iter
  ([n]
     (if (n < 3)
       n
       (f-iter 0 1 2 n)))
  ([a b c count]
     (if (= 0 count)
       c
       (recur b
              c
              (+ c
                 (* 2 b)
                 (* 3 a))
              (dec count)))))
