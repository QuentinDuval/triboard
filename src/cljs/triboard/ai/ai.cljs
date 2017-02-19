(ns triboard.ai.ai
  (:require
    [cljs.spec :as s :include-macros true]
    [triboard.ai.scores :as scores]
    [triboard.logic.board :as board]
    [triboard.logic.game :as game]
    [triboard.logic.move :as move]
    [triboard.logic.player :as player]
    ))


;; -----------------------------------------
;; Private
;; -----------------------------------------

(defn- make-ai
  [board player]
  {:player player
   :other-players (remove #{player} player/players)
   :cell-weights (scores/get-weights-by-cell board)
   })

(defn- score-move
  "Compute the strength of a move, based on the converted cells"
  [{:keys [player cell-weights]} [point conversions]]
  (reduce
    #(scores/update-score-diff cell-weights %1 %2)
    scores/null-score-diff
    (conj conversions (move/empty-cell-conversion player point))
    ))

(defn- worst-possible-score-from
  [{:keys [player other-players] :as ai} moves]
  (transduce
    (comp
      (mapcat #(get moves %))
      (map #(score-move ai %))
      (map #(get % player)))
    min
    other-players))

(defn- move-best-outcome
  [ai game [coord converted :as move]]
  (let [new-game (game/play-move game coord)
        move-diff (get (score-move ai move) (:player ai))
        new-moves (:moves (game/current-state new-game))
        next-diff (worst-possible-score-from ai new-moves)]
    (+ move-diff next-diff)))

(defn- max-by
  [key-fn coll]
  (apply max-key (memoize key-fn) coll))


;; -----------------------------------------
;; Public API
;; -----------------------------------------

(s/fdef best-move
  :args (s/tuple ::game ::player/player)
  :ret ::board/coord)

(defn best-move
  "[SIMPLISTIC] Return the best move for a player based on:
   * The immediate gain
   * The worse immediate lost afterwards"
  [game player]
  (let [game-state (game/current-state game)
        ai (make-ai (:board game-state) player)]
    (first
      (max-by #(move-best-outcome ai game %)
        (get-in game-state [:moves player]))
      )))
