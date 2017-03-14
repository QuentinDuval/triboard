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

(defn repeat-m
  [f input n]
  (if (pos? n)
    (recur f (gen/bind input f) (dec n))
    input))


;; ----------------------------------------------------------------------------
;; Generators
;; ----------------------------------------------------------------------------

(def coordinate-gen
  "Generating random coordinates inside the board"
  (s/gen ::board/coord))

(def max-number-of-turn-by-game
  "The maximum number of moves that can be played in a game"
  (- (* board/width board/height) (* 4 cst/init-block-count)))

(def game-gen
  (gen/fmap
    (fn [coord] (play-moves (game/new-game) coord))
    (gen/vector coordinate-gen 0 max-number-of-turn-by-game)))

(defn next-turn-gen
  "Generator for a valid next turn from a previous valid turn"
  [turn]
  (gen/fmap
    #(turn/next-turn turn %)
    (gen/elements (vals (turn/transitions turn)))))

;; (gen/sample (next-turn-gen (turn/new-init-turn)) 1)

(defn valid-coordinate-gen
  "Generate a valid coordinate that can be played at by the current player"
  [game]
  (let [transitions (turn/transitions (game/current-turn game))]
    (if (empty? transitions)
      (gen/return nil)
      (gen/elements (keys transitions)))))

(defn next-game-gen
  "Generate a game from the current game, by selecting a random transition"
  [game]
  (gen/fmap
    (fn [coord]
      (if (nil? coord) game (game/play-at game coord)))
    (valid-coordinate-gen game)))

(defn play-n-moves-gen
  "Play `n` random moves on the initial `game` provided as parameter"
  [game n]
  (repeat-m next-game-gen (gen/return game) n))

(def valid-game-gen
  "Generate a valid game, where each move is valid"
  (gen/bind
    #_(gen/elements (range (inc max-number-of-turn-by-game)))
    (gen/such-that #(< % 30) gen/int)                       ;; 30 before stack overflow
    #(play-n-moves-gen (game/new-game) %)))

(def board-gen
  (s/gen ::board/board))


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


#_(
    (s/def ::left ::bad-binary-tree)
    (s/def ::right ::bad-binary-tree)
    (s/def ::bad-binary-tree
      (s/cat
        :value any?
        :children (s/keys :opt [::left ::right])))

    (s/def ::branches #{:left :right})

    (s/def ::binary-tree
      (s/or
        :leaf int?
        :node (s/map-of ::branches ::binary-tree)))

    (s/def ::binary-tree-2
      (s/cat
        :value int?
        :children (s/map-of #{:left :right} (s/nilable ::binary-tree-2))))

    (s/def ::all-scores
      (s/map-of #{:blue :red :green} int?))

    (s/def ::blue int?)
    (s/def ::red int?)
    (s/def ::green int?)

    (s/def ::all-scores-2
      (s/keys :req-un [::blue ::red ::green]))
    )