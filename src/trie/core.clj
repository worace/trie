(ns trie.core)

(defn trie
  ([] (trie false))
  ([word?] {:children {} :word? word? :selections 0}))

(defn insert [t [char & rest]]
  (let [node (or (get-in t [:children char]) (trie))]
    (if (empty? rest)
      (assoc-in t [:children char] (assoc node :word? true))
      (assoc-in t [:children char] (insert node rest)))))

(defn word [char-path t]
  {:word (apply str char-path)
   :selections (:selections t)})

(defn words
  ([t] (words t []))
  ([t path]
   (concat
    (if (:word? t) [(word path t)] [])
    (mapcat (fn [[char child]]
              (words child (conj path char)))
            (:children t)))))

(defn insert-words
  ([words] (insert-words (trie) words))
  ([t words] (reduce insert t words)))

(defn retrieve
  [t word]
  (loop [t t
         curr (first word)
         tail (rest word)]
    (if (empty? tail)
      (get-in t [:children curr])
      (recur (get-in t [:children curr]) (first tail) (rest tail)))))

(defn suggest [t substring]
  (->> substring
       (retrieve t)
       (words)
       (sort-by (juxt (comp - :selections) :word))
       (map :word)
       (map (partial str substring))))

(defn dict-words []
  (clojure.string/split (slurp "/usr/share/dict/words") #"\n"))

(def tcount (comp count words))

(defn rec-tcount
  ([t] (rec-tcount t 0))
  ([t n] (if (:word? t)
           (inc (reduce + (map rec-tcount (vals (:children t)))))
           (reduce + (map rec-tcount (vals (:children t)))))))

(defn key-path [word]
  (mapcat vector (repeat :children) word))

(defn select
  [t substring selection]
  (if-let [subtrie (retrieve t selection)]
    (assoc-in t (key-path selection)
              (update-in subtrie [:selections] inc))
    t))
