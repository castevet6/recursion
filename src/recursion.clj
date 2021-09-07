(ns recursion)

(defn product [coll]
  (if (empty? coll) 1 (* (first coll) (product (rest coll)))))

(defn singleton? [coll]
  (if (== (count coll) 1) true false))

(defn my-last [coll]
  (cond
    (empty? coll) nil
    (singleton? coll) (first coll)
    :else (my-last (rest coll))))

(defn max-element [a-seq]
  (cond
    (empty? a-seq) nil
    (singleton? a-seq) (first a-seq)
    :else (max (first a-seq) (max-element (rest a-seq)))))

(defn seq-max [seq-1 seq-2]
  (let [len1 (count seq-1)
        len2 (count seq-2)]
    (if (> len1 len2) seq-1 seq-2)))

(defn longest-sequence [a-seq]
  (cond
    (empty? a-seq) nil
    (singleton? a-seq) (first a-seq)
    :else (seq-max (first a-seq) (longest-sequence (rest a-seq)))))

(defn my-filter [pred? a-seq]
  (cond
    (empty? a-seq) a-seq
    :else (if (pred? (first a-seq))
            (cons (first a-seq) (my-filter pred? (rest a-seq)))
            (my-filter pred? (rest a-seq)))))

(defn sequence-contains? [elem a-seq]
  (cond
    (empty? a-seq) false
    (= (first a-seq) elem) true
    :else (sequence-contains? elem (rest a-seq))))

(defn my-take-while [pred? a-seq]
  (cond
    (empty? a-seq) a-seq
    :else (if (pred? (first a-seq))
            (cons (first a-seq) (my-take-while pred? (rest a-seq)))
            '())))

(defn my-drop-while [pred? a-seq]
  (cond
    (empty? a-seq) a-seq
    :else
      (if (pred? (first a-seq))
        (my-drop-while pred? (rest a-seq))
        a-seq)))


(defn seq= [a-seq b-seq]
  (cond
    (and (empty? a-seq) (empty? b-seq)) true
    (and (empty? a-seq) (not (empty? b-seq))) false
    (and (not (empty? a-seq)) (empty? b-seq)) false
    :else
      (if (= (first a-seq) (first b-seq))
        (seq= (rest a-seq) (rest b-seq))
        false)))

(defn my-map [f seq-1 seq-2]
  (cond
    (or (empty? seq-1) (empty? seq-2)) '()
    :else
      (cons (f (first seq-1 ) (first seq-2)) (my-map f (rest seq-1) (rest seq-2)))))

(defn power [n k]
  (if (zero? k)
    1
    (* n (power n (dec k)))))

(defn fib [n]
  (cond
    (= n 0) 0
    (= n 1) 1
    :else (+ (fib (- n 1)) (fib (- n 2)))))

(defn my-repeat [how-many-times what-to-repeat]
  (if (< how-many-times 1)
    '()
    (cons what-to-repeat (my-repeat (dec how-many-times) what-to-repeat))))

(defn my-range [up-to]
  (if (< up-to 1)
    '()
    (cons (dec up-to) (my-range (dec up-to)))))

(defn tails [a-seq]
  (if (empty? a-seq)
    [()]
    (cons (seq a-seq) (tails (rest a-seq)))))

(defn inits [a-seq]
  (let [tails (tails (reverse a-seq))]
    (reverse (map reverse tails))))

(defn rotations [a-seq]
  (let [rev-inits (reverse (inits a-seq))
        rev-tails (reverse (tails a-seq))]
    (if (empty? a-seq)
      '(())
      (rest (map concat rev-tails rev-inits)))))

(defn my-frequencies-helper [freqs a-seq]
  (let [cur (first a-seq)]
    (if (empty? a-seq)
      freqs
      (if (contains? freqs cur)
        (my-frequencies-helper (assoc freqs cur (inc (get freqs cur))) (rest a-seq))
        (my-frequencies-helper (assoc freqs cur 1) (rest a-seq))))))

(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))

(defn un-frequencies [a-map]
  (if (empty? a-map)
    []
    (let [[elem num] (first a-map)]
      (concat
        (repeat num elem)
        (un-frequencies (rest a-map))))))

(defn my-take [n coll]
  (cond
    (empty? coll) '()
    (zero? n) '()
    :else
      (cons
        (first coll)
        (my-take (dec n) (rest coll)))))

(defn my-drop [n coll]
  (cond
    (empty? coll) '()
    (zero? n) (seq coll)
    :else (my-drop (dec n) (rest coll))))

(defn halve [a-seq]
  (let [seq-len (count a-seq)
        divisor (int (/ seq-len 2))]
    (vector (my-take divisor a-seq) (my-drop divisor a-seq))))

(defn seq-merge [a-seq b-seq]
  (if (and (empty? a-seq) (empty? b-seq))
    '()
    (let [cur-a (first a-seq)
          cur-b (first b-seq)]
      (cond
        (empty? cur-a) (cons cur-b (seq-merge a-seq (rest b-seq)))
        (empty? cur-b) (cons cur-a (seq-merge (rest a-seq) b-seq))
        (< cur-a cur-b) (cons cur-a (seq-merge (rest a-seq) b-seq))
        :else (cons cur-b (seq-merge a-seq (rest b-seq)))))))

(defn merge-sort [a-seq]
  [:-])

(defn split-into-monotonics [a-seq]
  [:-])

(defn permutations [a-set]
  [:-])

(defn powerset [a-set]
  [:-])

