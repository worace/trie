(ns trie.core-test
  (:require [clojure.test :refer :all]
            [trie.core :refer :all]))

(deftest test-alphabet
  (is (= 26 (count (alphabet))))
  (is (= \a (first (alphabet))))
  (is (= \z (last (alphabet)))))

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
