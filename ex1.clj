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


;;;Counting Change
(defn first-denomination
  [kinds-of-coins]
  (cond (= kinds-of-coins 1) 1
        (= kinds-of-coins 2) 5
        (= kinds-of-coins 3) 10
        (= kinds-of-coins 4) 25
        (= kinds-of-coins 5) 50))

(defn count-change
  ([amount] (count-change amount 5))
  ([amount kinds-of-coins]
     (cond (= amount 0) 1
           (or (< amount 0) (= kinds-of-coins 0)) 0
           :else (+ (count-change amount
                                  (dec kinds-of-coins))
                    (count-change (- amount
                                     (first-denomination kinds-of-coins))
                                  kinds-of-coins)))))


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
     (if (< n 3)
       n
       (f-iter 0 1 2 n)))
  ([a b c count]
     (if (= 2 count)
       c
       (recur b
              c
              (+ c
                 (* 2 b)
                 (* 3 a))
              (dec count)))))

;; The key idea here is to keep as much state variables as we have
;; recursive calls, and use them as a queue, with the first presenting
;; the most recent computation, and so on
;; http://eli.thegreenplace.net/2007/06/28/sicp-section-122/

;;;Example 1.12: Pascal's Triangle
;;;Write a procedure that computes parts of Pascal's Triangle

(defn pascals-triangle
  [row col]
  (cond (= row 1) 1
        (or (= col 1) (= row col)) 1
        :else (+ (pascals-triangle (dec row)
                                   col)
                 (pascals-triangle (dec row)
                                   (dec col)))))



;; Example 1.15
(defn cube
  [x]
  (* x x x))
(defn p
  [x]
  (- (* 3 x)
     (* 4
        (cube x))))
(defn sine
  [angle]
  (if (not (> (Math/abs angle) 0.1))
    angle
    (p (sine (/ angle
                3.0)))))

;; a)
;; How many times is procedure p applied when (sine 12.15) is evaled?
;; (sine 12.15)
;; |
;; (sine 4.05)
;; |
;; (sine 1.35)
;; |
;; (sine 0.45)
;; |
;; (sine 0.15)
;; |
;; (sine 0.05) => angle
;;
;; contracts back, so from in to out:
;; (p 0.05)
;; |
;; (p 0.1495)
;; |
;; (p 0.4351345505)
;; |
;; (p 0.9758465331678772)
;; |
;; (p -0.7895631144708228) => -0.39980345741334

;; b) What is the order of growth in space and number of steps (as a
;; function of a) used by the process generated by the sine procedure
;; when (sine a) is evaluated?
;;
;; this is controlled by the rate at which angle is reduced below
;; 3.0. given its divided by 3, it would appear to be O(log n)?



;;; Exponentiation

(defn expt
  [b n]
  (if (= 0 n)
    1
    (* b (expt b
               (dec n)))))

(defn expt-iter
  ([b n]
     (expt-iter b n 1))
  ([b counter product]
     (if (= counter 0)
       product
       (recur b
              (dec counter)
              (* b product)))))

(defn square
  [n]
  (* n n))

(defn fast-expt
  [b n]
  (cond (= n 0) 1
        (even? n) (square (fast-expt b
                                     (/ n 2)))
        :else (* b
                 (fast-expt b
                            (dec n)))))

;; Exercise 1.16.  Design a procedure that evolves an iterative
;; exponentiation process that uses successive squaring and uses a
;; logarithmic number of steps, as does fast-expt. (Hint: Using the
;; observation that (b^n/2)^2 = (b^2)^n/2, keep, along with the exponent n
;; and the base b, an additional state variable a, and define the
;; state transformation in such a way that the product a bn is
;; unchanged from state to state. At the beginning of the process a is
;; taken to be 1, and the answer is given by the value of a at the end
;; of the process. In general, the technique of defining an invariant
;; quantity that remains unchanged from state to state is a powerful
;; way to think about the design of iterative algorithms.)

(defn fast-expt-iter
  ([b n] (fast-expt-iter b n 1))
  ([b n a]
     (cond (= n 0) a
           (even? n) (recur (square b)
                            (/ n 2)
                            a)
           :else (recur b
                        (dec n)
                        (* a b)))))



;; Exercise 1.17.  The exponentiation algorithms in this section are
;; based on performing exponentiation by means of repeated
;; multiplication. In a similar way, one can perform integer
;; multiplication by means of repeated addition. The following
;; multiplication procedure (in which it is assumed that our language
;; can only add, not multiply) is analogous to theexpt procedure:

(defn mult
  [a b]
  (if (= b 0)
    0
    (+ a
       (mult a
             (dec b)))))

;; This algorithm takes a number of steps that is linear in b. Now
;; suppose we include, together with addition, operations double,
;; which doubles an integer, and halve, which divides an (even)
;; integer by 2. Using these, design a multiplication procedure
;; analogous to fast-expt that uses a logarithmic number of steps.

(defn dbl
  [n]
  (* n 2))

(defn halve
  [n]
  (/ n 2))

(defn fast-mult
  [a b]
  (cond (= b 0) 0
        (even? b) (fast-mult (dbl a)
                             (halve b))
        :else (+ a
                 (fast-mult a
                            (- b 1)))))

;; Unsurprisingly this performs worse than the clojure.core/* fn.
;; user=> (time (fast-mult 100000000000000000000000000000000 20))
;; "Elapsed time: 0.149 msecs"
;; user=> (time (* 100000000000000000000000000000000 20))
;; "Elapsed time: 0.071 msecs"


;; Exercise 1.18.  Using the results of exercises 1.16 and 1.17,
;; devise a procedure that generates an iterative process for
;; multiplying two integers in terms of adding, doubling, and halving
;; and uses a logarithmic number of steps.

(defn fast-mult-iter
  ([a b] (fast-mult-iter a b 0))
  ([a b c]
     (cond (= b 0) c
           (even? b) (recur (dbl a)
                            (halve b)
                            c)
           :else (recur a
                        (dec b)
                        (+ a c)))))

;; for example, 9 and 5
;; (fast-mult-iter 9 5 0)
;; (fast-mult-iter 9 4 9)
;; (fast-mult-iter 18 2 9)
;; (fast-mult-iter 36 1 9)
;; (fast-mult-iter 36 0 45)



;; Exercise 1.19.   There is a clever algorithm for computing the
;; Fibonacci numbers in a logarithmic number of steps. Recall the
;; transformation of the state variables a and b in the fib-iter
;; process of section 1.2.2: a  a + b and b  a. Call this
;; transformation T, and observe that applying T over and over again n
;; times, starting with 1 and 0, produces the pair Fib(n + 1) and
;; Fib(n). In other words, the Fibonacci numbers are produced by
;; applying Tn, the nth power of the transformation T, starting with
;; the pair (1,0). Now consider T to be the special case of p = 0 and
;; q = 1 in a family of transformations Tpq, where Tpq transforms the
;; pair (a,b) according to a = bq + aq + ap and b = bp + aq. Show that
;; if we apply such a transformation Tpq twice, the effect is the same
;; as using a single transformation Tp'q' of the same form, and
;; compute p' and q' in terms of p and q. This gives us an explicit
;; way to square these transformations, and thus we can compute Tn
;; using successive squaring, as in the fast-expt procedure. Put this
;; all together to complete the following procedure, which runs in a
;; logarithmic number of steps:

(defn fib-log-iter
  ([n] (fib-log-iter 1 0 0 1 n))
  ([a b p q count]
     (cond (= count 0) b
           (even? count) (recur a
                                b
                                p' ; compute this
                                q' ; compute this
                                (/ count 2))
           :else (recur (+ (* b q) (* a q) (* a p))
                        (+ (* b p) (* a q))
                        p
                        q
                        (dec count)))))




;; Greatest Common Divisors

;; Euclid
(defn gcd
  [a b]
  (if (= b 0)
    a
    (recur b (mod a b))))
