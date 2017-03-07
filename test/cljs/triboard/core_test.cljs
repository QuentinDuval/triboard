(ns ^:figwheel-always triboard.core-test
  (:require-macros
    [cljs.test :refer (is are deftest testing)]
    [clojure.test.check.clojure-test :refer [defspec]]
    )
  (:require
    [cljs.spec :as s]
    [cljs.spec.test :as stest :include-macros true]
    ;;[cljs.spec.impl.gen :as sgen]
    [cljs.test :as test]
    [clojure.test.check :as tc]
    [clojure.test.check.generators :as gen]
    [clojure.test.check.properties :as prop :include-macros true]
    [triboard.logic.constants :as cst]
    [triboard.logic.board :as board]
    [triboard.logic.game :as game]
    [triboard.logic.transition :as transition]
    [triboard.logic.player :as player]
    [triboard.logic.scores :as scores]
    [triboard.logic.turn :as turn]
    [triboard.utils.algo :as algo]
    ))


;; ----------------------------------------------------------------------------
;; Utils
;; ----------------------------------------------------------------------------

(def max-score (- (* board/width board/height) cst/init-block-count))

(defn play-moves
  [init-game moves]
  (reduce game/play-at init-game moves))

(defn current-score
  [game]
  (:scores (game/current-turn game)))

(defn sum-player-scores
  [score]
  (transduce (map second) + score))

(defn check-spec-test
  [spec-res]
  (let [res (stest/summarize-results spec-res)]
    (= (:total res) (:check-passed res))))


;; ----------------------------------------------------------------------------
;; Generators
;; ----------------------------------------------------------------------------

(def coord-gen (s/gen ::board/coord))

(def initial-empty-cell-count
  (- (* board/width board/height) (* 4 cst/init-block-count)))

(def game-gen
  (gen/fmap
    (fn [coord] (play-moves (game/new-game) coord))
    (gen/vector coord-gen 0 initial-empty-cell-count)))



;; ----------------------------------------------------------------------------
;; BOARD
;; ----------------------------------------------------------------------------

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


;; ----------------------------------------------------------------------------
;; SCORE
;; ----------------------------------------------------------------------------

(deftest score-todo-test
  (testing "trivial assumption"
    (is (= 1 1))))


;; ----------------------------------------------------------------------------
;; Property based tests
;; ----------------------------------------------------------------------------

#_(
    (deftest board-test
      (is (check-spec-test (stest/check `board/empty-cells)))
      (is (check-spec-test (stest/check `board/to-seq))))

    (deftest scores-test
      (is (check-spec-test (stest/check `scores/update-scores))))

    (deftest move-test
      (is (check-spec-test (stest/check `move/all-available-moves)))
      (is (check-spec-test (stest/check `move/apply-conversion))))
    )

(defn valid-game-transition?
  [old-game new-game move]
  (let [old-scores (current-score old-game)
        new-scores (current-score new-game)]
    (and
      (= (sum-player-scores old-scores) (- (sum-player-scores new-scores) 1))
      )))

(defn game-move-properties
  [old-game]
  (prop/for-all [coord coord-gen]
    (let [new-game (game/play-at old-game coord)]
      (or
        (= old-game new-game)                               ;; TODO - test contained in empty cells
        (valid-game-transition? old-game new-game coord)
        ))))

#_(defn game-undo-properties
    [old-game]
    (prop/for-all [coord coord-gen]
      (let [new-game (game/play-at old-game coord)]
        (or
          (= old-game new-game)
          (= old-game (game/undo-player-move new-game (fn [_] false))) ;; TODO
          ))))

(defspec try-move-from-valid-game 100
  (prop/for-all [g game-gen] (game-move-properties g)))

#_(defspec try-undo-from-valid-game 100
    (prop/for-all [g game-gen] (game-undo-properties g)))


;; ----------------------------------------------------------------------------
;; Running the tests
;; ----------------------------------------------------------------------------

;; To run full instrumented tests
;; (require '[cljs.spec.test])
;; (cljs.spec.test/instrument)
;; (cljs.spec.test/unstrument)

;; (test/run-tests)
