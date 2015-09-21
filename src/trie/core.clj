(ns trie.core)

(defn alphabet []
  (map char (range 97 123)))

(defn trie
  ([] (trie false))
  ([word?] {:children {} :word? word?}))

(defn insert [t [char & rest]]
  (if (empty? rest)
    (assoc-in t [:children char] (assoc (trie) :word? true))
    (assoc-in t [:children char] (insert (trie) rest))))
