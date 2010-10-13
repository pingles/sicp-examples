(defn multiple-of?
  [divisors n]
  (some #(= 0 %)
        (map #(mod n %) divisors)))

(reduce +
        (filter #(multiple-of? '(3 5) %)
                (range 1 1000)))
