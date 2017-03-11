(ns triboard.algo-test
  (:require
    [cljs.test :as test]
    [triboard.utils.algo :as algo])
  (:require-macros
    [cljs.test :refer (is are deftest testing)]
    ))

(deftest test-pick-n-of-each
  (let [pick-n-of-each (comp vec algo/pick-n-of-each)]
    (testing "Varying number of each group"
      (are
        [expected n] (= expected (pick-n-of-each n (range) [:a :b]))
        [] 0
        [[0 :a] [1 :b]] 1
        [[0 :a] [1 :a] [2 :b] [3 :b]] 2
        ))
    (testing "Varying number of groups"
      (are
        [expected groups] (= expected (pick-n-of-each 2 (range) groups))
        [] []
        [[0 :a] [1 :a]] [:a]
        ))
    (testing "No elements to choose from"
      (is
        (= [] (pick-n-of-each 1 [] [:a]))
        ))))
