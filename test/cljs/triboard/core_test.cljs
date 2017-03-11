(ns ^:figwheel-always triboard.core-test
  (:require-macros
    [cljs.test :refer (is are deftest testing)]
    [clojure.test.check.clojure-test :refer [defspec]]
    )
  (:require
    [cljs.spec :as s]
    [cljs.spec.test :as stest :include-macros true]
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


;; ----------------------------------------------------------------------------
;; Generators
;; ----------------------------------------------------------------------------

(def coordinate-gen
  "Generating random coordinates inside the board"
  (s/gen ::board/coord))

(def max-number-of-turn-by-game
  "The maximum number of moves that can be played in a game"
  (- (* board/width board/height) (* 4 cst/init-block-count)))

(defn valid-coordinate-gen
  "Generate a valid coordinate that can be played at by the current player"
  [game]
  (let [transitions (turn/transitions (game/current-turn game))]
    (if (empty? transitions)
      (gen/return nil)
      (gen/elements (keys transitions)))))

(def game-gen
  (gen/fmap
    (fn [coord] (play-moves (game/new-game) coord))
    (gen/vector coordinate-gen 0 max-number-of-turn-by-game)))

(defn play-n-moves-gen
  "Play `n` random moves on the initial `game` provided as parameter"
  [game n]
  (if (zero? n)
    (gen/return game)
    (gen/bind
      (valid-coordinate-gen game)
      (fn play-coord [coord]
        (if (nil? coord)
          (gen/return game)
          (play-n-moves-gen (game/play-at game coord) (dec n)))
        ))
    ))

(def valid-game-gen
  "Generate a valid game, where each move is valid"
  (gen/bind
    (gen/elements (range (inc max-number-of-turn-by-game)))
    #(play-n-moves-gen (game/new-game) %)))


;; ----------------------------------------------------------------------------
;; Property based tests
;; ----------------------------------------------------------------------------

(defn valid-game-transition?
  [old-game new-game move]
  (let [old-scores (current-score old-game)
        new-scores (current-score new-game)]
    (and
      (= (sum-player-scores old-scores) (- (sum-player-scores new-scores) 1))
      )))

(defn game-move-properties
  [old-game]
  (prop/for-all [coord coordinate-gen]
    (let [new-game (game/play-at old-game coord)]
      (or
        (= old-game new-game)                               ;; TODO - test contained in empty cells
        (valid-game-transition? old-game new-game coord)
        ))))

#_(defn game-undo-properties
    [old-game]
    (prop/for-all [coord coordinate-gen]
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
