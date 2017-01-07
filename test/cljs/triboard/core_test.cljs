(ns triboard.core-test
  (:require-macros
    [cljs.test :refer (is deftest testing)]
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
    [triboard.logic.move :as move]
    [triboard.logic.scores :as scores]
    [triboard.logic.turn :as turn]
    ))


;; ----------------------------------------------------------------------------
;; Utils
;; ----------------------------------------------------------------------------

(defn play-moves
  [init-game moves]
  (reduce game/play-move init-game moves))

(defn current-score
  [game]
  (turn/get-scores (game/current-turn game)))

(defn sum-player-scores
  [score]
  (transduce (map second) + score))

(defn check-spec-test
  [spec-res]
  (let [res (stest/summarize-results spec-res)]
    (= (:total res) (:check-passed res))))


;; ----------------------------------------------------------------------------
;; Example based tests
;; ----------------------------------------------------------------------------

(deftest example-passing-test
  (is (= 1 1)))


;; ----------------------------------------------------------------------------
;; Generators
;; ----------------------------------------------------------------------------

(def coord-gen (s/gen ::board/coord))

(def initial-empty-cell-count
  (- (* cst/board-width cst/board-height) (* 4 cst/init-block-count)))

(def game-gen
  (gen/fmap
    (fn [coord] (play-moves (game/new-game) coord))
    (gen/vector coord-gen 0 initial-empty-cell-count)))


;; ----------------------------------------------------------------------------
;; Property based tests
;; ----------------------------------------------------------------------------

#_(
    (deftest board-test
      (is (check-spec-test (stest/check `board/empty-cells)))
      (is (check-spec-test (stest/check `board/to-iterable))))

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
    (let [new-game (game/play-move old-game coord)]
      (or
        (= old-game new-game)                               ;; TODO - test contained in empty cells
        (valid-game-transition? old-game new-game coord)
        ))))

(defn game-undo-properties
  [old-game]
  (prop/for-all [coord coord-gen]
    (let [new-game (game/play-move old-game coord)]
      (or
        (= old-game new-game)
        (= old-game (game/undo-player-move new-game (fn [_] false)))
        ))))

(defspec try-move-from-valid-game 100
  (prop/for-all [g game-gen] (game-move-properties g)))

(defspec try-undo-from-valid-game 100
  (prop/for-all [g game-gen] (game-undo-properties g)))


;; ----------------------------------------------------------------------------
;; Running the tests
;; ----------------------------------------------------------------------------

;; To run full instrumented tests
;; (require '[cljs.spec.test])
;; (cljs.spec.test/instrument)
;; (cljs.spec.test/unstrument)

(test/run-tests)
