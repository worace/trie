(ns trie.core)

(defn trie
  ([] (trie false))
  ([word?] {:children {} :word? word?}))

(defn insert [t [char & rest]]
  (let [node (or (get-in t [:children char]) (trie))]
    (if (empty? rest)
      (assoc-in t [:children char] (assoc node :word? true))
      (assoc-in t [:children char] (insert node rest)))))

(defn words
  ([t] (map #(apply str %) (words t [])))
  ([t path]
  (concat
   (if (:word? t) [path] [])
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

(defn suggest [t word]
  (map (partial str word) (words (retrieve t word))))
