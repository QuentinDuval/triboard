(ns triboard.core-test
  (:require-macros
    [cljs.test :refer (is deftest testing)]
    [clojure.test.check.clojure-test :refer [defspec]]
    )
  (:require
    [cljs.test :as test]
    [clojure.test.check :as tc]
    [clojure.test.check.generators :as gen]
    [clojure.test.check.properties :as prop :include-macros true]
    [triboard.logic.constants :as cst]
    [triboard.logic.game :as game]
    ))


;; ----------------------------------------------------------------------------
;; Utils
;; ----------------------------------------------------------------------------

(defn play-moves
  [init-game moves]
  (reduce game/play-move init-game moves))


;; ----------------------------------------------------------------------------
;; Example based tests
;; ----------------------------------------------------------------------------

(deftest example-passing-test
  (is (= 1 1)))


;; ----------------------------------------------------------------------------
;; Generators
;; ----------------------------------------------------------------------------

(def move-gen
  (gen/elements cst/all-positions))

(def initial-empty-cell-count
  (- (* cst/board-width cst/board-height) (* 4 cst/init-block-count)))

(def game-gen
  (gen/fmap
    #(play-moves (game/new-game) %)
    (gen/vector move-gen 0 initial-empty-cell-count)
    ))


;; ----------------------------------------------------------------------------
;; Property based tests
;; ----------------------------------------------------------------------------

(defn game-undo-properties
  [old-game]
  (prop/for-all [move move-gen]
    (let [new-game (game/play-move old-game move)]
      (or
        (= old-game new-game)
        (= old-game (game/undo-player-move new-game (fn [_] false)))
        ))))

(defspec try-undo-from-valid-game 100
  (prop/for-all [g game-gen] (game-undo-properties g)))


;; ----------------------------------------------------------------------------
;; Running the tests
;; ----------------------------------------------------------------------------

(test/run-tests)
