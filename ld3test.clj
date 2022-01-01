;; 1. Find the last element of a list.
(defn my-last [xs]
 (if (empty? (rest xs))
   (first xs)
   (my-last (rest xs))))


(assert (= :d
           (my-last '(:a :b :c :d))))

;; 2. Find the N-th element of a list.
(defn get-nth [xs n]
    (if (empty? xs)
    nil
    (if (= n 0)
        (first xs)
        (get-nth (rest xs) (- n 1)))))

(assert (= :c
           (get-nth '(:a :b :c :d) 2)))

;; 3. Find the length of a list
(defn my-length [xs]
   (if (empty? (rest xs))
    1
    (+ 1(my-length (rest xs)))))

(assert (= 4
           (my-length '(:a :b :c :d))))

;; 4. Reverse a list.
(defn my-reverse [xs]
  (if (empty? (rest xs))
  xs
  (cons (my-last xs) (my-reverse (drop-last xs) ))))

(assert (= '(:d :c :b :a)
           (my-reverse '(:a :b :c :d))))

;; 5. Find out whether a list is a palindrome.
(defn is-palindrome? [xs]
  (if (empty? (rest xs))
  	true
	(if (= (my-last xs) (first xs))
		(is-palindrome? (drop-last(rest xs)))
		false
		)))


(assert (= true
           (is-palindrome? '(:a :b :c :b :a))))

;; 6. Duplicate the elements of a list.
(defn duplicate [xs]
  (if (empty? (rest xs))
  (cons (first xs) xs)
  (cons (first xs) (cons (first xs) (duplicate (rest xs) )))))


(assert (= '(:a :a :b :b :c :c)
           (duplicate '(:a :b :c))))

;; 7. Eliminate consecutive duplicates of a list.
(defn compress [xs]
  (if (empty? (rest xs))
	  xs
	  (if (= (first xs) (first(rest xs)))
	  (compress (rest xs))
	  (cons (first xs) (compress (rest xs))))))

(assert (= '(:a :b :c)
           (compress '(:a :a :b :b :c :c))))

;; 8. Remove the N-th element of a list
(defn remove-at [xs n]
(if (= 0 n)
(remove-at (rest xs) (- n 1))
(if (empty? (rest xs))
  xs
  (cons (first xs) (remove-at (rest xs) (- n 1 ))))))

(assert (= '(:a :b :d :e)
           (remove-at '(:a :b :c :d :e) 2)))

;; 9. Insert a new element at the N-th position of a list.
(defn insert-at [x xs n]
(if (= 0 n)
(cons x (cons (first xs) (insert-at x (rest xs) (- n 1))))
(if (empty? (rest xs))
  xs
  (cons (first xs) (insert-at x (rest xs) (- n 1 ))))))

(assert (= '(:a :b :x :c :d)
           (insert-at :x '(:a :b :c :d) 2)))

;; 10. Create a list containing all integers within a given range.
(defn my-range [a b]
  (if (= a (+ 1 b))
  	nil
	(cons a (my-range (+ 1 a) b))))

(assert (= '(3 4 5 6 7)
           (my-range 3 7)))

;; 11. Concatenate two lists
(defn my-concat [xs ys]
	(if (empty?  xs)
		ys
		(cons (first xs) (my-concat (rest xs) ys))))

(assert (= '(:a :b :c :d :e :f)
           (my-concat '(:a :b :c) '(:d :e :f))))

;; 12. Split a list into two parts; the length of the first part is given.
(defn my-drop [xs n]
  (if (= 0 n)
  	(if (empty? (rest xs))
		xs
		(cons (first xs) (my-drop (rest xs) n)))
	(my-drop (rest xs) (- n 1))))

(assert (= '(:d :e)
           (my-drop '(:a :b :c :d :e) 3)))

;; 13. Split a list into two parts; the length of the first part is given.
(defn my-take [xs n]
  (if (not= 0 n)
  	(if (empty? (rest xs))
		xs
		(cons (first xs) (my-take (rest xs) (- n 1))))))

(assert (= '(:a :b :c)
           (my-take '(:a :b :c :d :e) 3)))

;; 14. Implement the filtering function
(defn my-filter [p xs]
 (cond (empty? xs) nil
       (p (first xs)) (cons (first xs)
                            (my-filter p (rest xs)))
       :else (my-filter p (rest xs))))

(assert (= '(1 3 5)
           (my-filter odd? '(1 2 3 4 5))))

;; 15. Implement the mapping function
(defn my-map [f xs]
  (cond (empty? xs) nil
  :else (cons (f (first xs)) (my-map f (rest xs)))))


(assert (= '(2 3 4 5 6)
           (my-map inc '(1 2 3 4 5))))

;; 16. Implement the reducing function
(defn my-reduce [f acc xs]
 (cond (empty? xs) acc
  :else  (f(first xs)(my-reduce f acc (rest xs)))))

(assert (= 15
           (my-reduce + 0 '(1 2 3 4 5))))


;; 17. Implement the flattening function

(defn my-flatten [xs]
  (if (empty? xs)
    nil
    (if (not (sequential? (first xs)))
      (cons (first xs) (my-flatten (rest xs)))
      (my-concat (my-flatten (first xs)) (my-flatten (rest xs))))))

(assert (= '(:a :b :c :d :e)
           (my-flatten '(:a (:b (:c :d) :e)))))
