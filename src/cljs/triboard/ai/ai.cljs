(ns triboard.ai.ai
  (:require
    [cljs.spec :as s :include-macros true]
    [triboard.ai.scores :as scores]
    [triboard.logic.board :as board]
    [triboard.logic.constants :as cst]
    [triboard.logic.game :as game]
    [triboard.logic.move :as move]
    [triboard.logic.turn :as turn]
    ))


;; -----------------------------------------
;; Private
;; -----------------------------------------

(defn- make-ai
  [game-state player]
  {:player player
   :other-players (remove #{player} cst/players)
   :cell-weights (scores/get-weights-by-cell (:board game-state))
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
  [{:keys [player other-players] :as ai} game-state]
  (transduce
    (comp
      (mapcat #(get (:moves game-state) %))
      (map #(score-move ai %))
      (map #(get % player)))
    min
    other-players))

(defn- move-best-outcome
  [ai turn [coord converted :as move]]
  (let [new-turn (turn/play-move turn coord)                ;; TODO - replace by game
        move-diff (get (score-move ai move) (:player ai))
        next-diff (worst-possible-score-from ai new-turn)]
    (+ move-diff next-diff)))

(defn- max-by
  [key-fn coll]
  (apply max-key (memoize key-fn) coll))


;; -----------------------------------------
;; Public API
;; -----------------------------------------

(s/fdef best-move
  :args (s/tuple ::game/game-state ::cst/player)            ;; TODO - take a full game
  :ret ::board/coord)

(defn best-move
  "[SIMPLISTIC] Return the best move for a player based on:
   * The immediate gain
   * The worse immediate lost afterwards"
  [game-state player]
  (let [ai (make-ai game-state player)]
    (first
      (max-by #(move-best-outcome ai game-state %)
        (get (:moves game-state) player))                   ;; TODO - Game
      )))
