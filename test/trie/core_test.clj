(ns trie.core-test
  (:require [clojure.test :refer :all]
            [trie.core :refer :all]))

(deftest test-trie
  (testing "generates new map"
    (is (= #{:children :word?} (into #{} (keys (trie)))))))

(deftest test-word-trie
  (testing "sets word val to true"
    (is (:word? (trie true)))))

(deftest test-inserting-a-word
  (testing "inserts simple word"
    (let [t (insert (trie) "a")]
      (is (get-in t [:children \a]))
      (is (:word? (get-in t [:children \a]))))))

(deftest test-inserting-longer-word
  (testing "marks end node as word?"
    (let [t (insert (trie) "as")]
      (is (get-in t [:children \a :children \s]))
      (is (not (get-in t [:children \a :word?])))
      (is (get-in t [:children \a :children \s :word?])))))

(deftest test-inserting-additional-words
  (testing "leaves existing words intact"
    (let [t (insert (insert (trie) "a") "as")
          t2 (insert t "ask")]
      (is (get-in t [:children \a :children \s]))
      (is (get-in t [:children \a :word?]))
      (is (get-in t [:children \a :children \s :word?]))
      (is (get-in t2 [:children \a :word?]))
      (is (get-in t2 [:children \a :children \s :word?]))
      (is (get-in t2 [:children \a :children \s :children \k :word?])))))

(deftest test-inserting-shorter-second-word
  (testing "marks intermediate node as word?"
    (let [t (insert (insert (trie) "ask") "as")
          t2 (insert t "a")]
      (is (get-in t [:children \a :children \s :children \k :word?]))
      (is (get-in t [:children \a :children \s :word?]))
      (is (get-in t2 [:children \a :word?])))))

(deftest test-words
  (testing "retrieves all words out of the tree"
    (let [t (insert (trie) "a")]
      (is (= ["a"] (words t)))
      (is (= ["a" "as"] (words (insert t "as")))))))

(deftest test-insert-words
  (testing "takes trie and coll of words and inserts all, returning the trie"
    (let [t (insert-words (trie) ["a" "as" "ask"])]
      (is (:word? (get-in t [:children \a :children \s :children \k]))))))

(defn dict-words []
  (clojure.string/split (slurp "/usr/share/dict/words") #"\n"))

(deftest test-read-dict
  (let [w (dict-words)]
    (is (= 235886 (count (dict-words))))
    (is (= "A" (first (dict-words))))
    (is (= "Zyzzogeton" (last (dict-words))))))

(deftest test-retrieving-word
  (testing "retrieves sub-tree from path created by word's characters"
    (let [t (insert-words ["a" "as" "ask" "asp" "ass"])
          sub (retrieve t "as")]
      (is (= #{\k \p \s} (into #{} (keys (:children sub))))))))

(deftest test-suggestions
  (testing "suggests children of provided text string"
    (let [t (insert-words ["ask" "asking" "asked" "askew"])]
      (is (= ["ask" "asking" "asked" "askew"] (suggest t "as"))))))

(deftest test-big-test
  (testing "reads lots of words"
    (let [t (insert-words (dict-words))]
      (is (= 235886 (count (words t)))))))
