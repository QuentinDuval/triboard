(ns triboard.ai.ai
  (:require
    [cljs.spec :as s :include-macros true]
    [triboard.ai.scoring :as scoring]
    [triboard.logic.game :as game]
    [triboard.logic.player :as player]
    [triboard.logic.scores :as scores]
    [triboard.logic.turn :as turn]
    ))


;; -----------------------------------------
;; Private
;; -----------------------------------------

(defn- make-ai
  [board player]
  {:player player
   :other-players (remove #{player} player/all)
   :cell-weights (scoring/get-weighting board)
   })

(defn- score-move
  "Compute the strength of a move, based on the converted cells"
  [{:keys [player cell-weights]} transition]
  (reduce                                                   ;; TODO - Factor in move
    #(scores/update-scores-with %1 %2 cell-weights)
    scores/null-scores
    transition))

;; -----------------------------------------
;; Attempts game - tree (later minimax decoupled)
;; -----------------------------------------

(defn build-game-tree
  [game depth]
  (if (= 0 depth)
    game
    (let [transitions (turn/player-transitions (game/current-turn game))
          next-games (map #(game/play-move game (first %)) transitions)]
      (map vector
        transitions
        (map #(build-game-tree % (dec depth)) next-games)))
    ))

(defn test-build-game-tree
  []
  (let [init-game (game/new-game)]
    (build-game-tree init-game 1)))

;; -----------------------------------------
;; Attempts minimax
;; -----------------------------------------

(defn leaf-score
  [ai game]
  (:scores (game/current-turn game)))

(defn tree-score
  [ai game depth]
  (if (= 0 depth)
    (leaf-score ai game)
    (let [turn (game/current-turn game)
          transitions (turn/player-transitions turn)
          next-games (map #(game/play-move game (first %)) transitions)
          min-max (if (= (:player ai) (:player turn)) max min)]
      (apply min-max (map #(tree-score ai % (dec depth)) next-games)))
    ))

(defn test-score-tree
  []
  (let [init-game (game/new-game)
        turn (game/current-turn init-game)
        ai (make-ai (:board turn) (:player turn))]
    (tree-score ai init-game 2)))

;; -----------------------------------------

(defn- worst-possible-score-from
  [{:keys [player other-players] :as ai} moves]
  (transduce
    (comp
      (mapcat #(get moves %))
      (map #(score-move ai (second %)))
      (map #(get % player)))
    min
    other-players))

(defn- move-best-outcome
  [ai game [coord transition]]
  (let [new-game (game/play-move game coord)
        move-diff (get (score-move ai transition) (:player ai))
        new-moves (:transitions (game/current-turn new-game))
        next-diff (worst-possible-score-from ai new-moves)]
    {:scoring (+ move-diff next-diff)
     :game new-game}))

(defn- max-by [key-fn coll] (apply max-key key-fn coll))


;; -----------------------------------------
;; Public API
;; -----------------------------------------

(s/fdef play-best-move
  :args (s/tuple ::game/game)
  :ret ::game/game)

(defn- play-best-move
  "[SIMPLISTIC] Play the best move for a player based on:
   * The immediate gain
   * The worse immediate lost afterwards"
  [game]
  (let [turn (game/current-turn game)
        ai (make-ai (:board turn) (:player turn))]
    (:game
      (max-by :scoring
        (map
          #(move-best-outcome ai game %)
          (turn/player-transitions turn)))
      )))
