(ns triboard.ai.ai
  (:require
    [triboard.ai.scores :as scores]
    [triboard.logic.constants :as cst]
    [triboard.logic.move :as move]
    [triboard.logic.turn :as turn]
    [triboard.utils :as utils]
    ))


;; -----------------------------------------
;; Private
;; -----------------------------------------

(defn- move-strength
  "Compute the strength of a move, based on the converted cells"
  [get-cells-strength player [point converted]]
  {:pre [(move/conversions? converted)]}
  (reduce
    #(scores/update-delta get-cells-strength %1 %2)
    scores/null-delta
    (conj converted
      (move/empty-cell-conversion player point))
    ))

(defn- worst-immediate-loss ;; TODO - It should consider what the other player could win
  "Return the next worse lost turn move for 'looser' if 'player' plays"
  {:pre [(player? player) (player? looser)]}
  [cells-strength turn player looser]
  (let [all-moves (turn/get-moves-of turn player)]
    (transduce
      (map #(get (move-strength cells-strength player %) looser))
      min
      all-moves)))


;; -----------------------------------------
;; Public API
;; -----------------------------------------

(defn best-move
  "[SIMPLISTIC] Return the best move for a player based on:
   * The immediate gain
   * The worse immediate lost afterwards"
  {:pre [(player? player)]}
  [turn player]
  (let [cells-strength (scores/compute-cells-strength (turn/get-board turn))
        moves (turn/get-moves-of turn player)
        others (remove #{player} cst/players)]
    (first
      (utils/fast-max-key
        (fn [[m converted :as move]]
          (let [new-turn (turn/play-move turn m)
                new-diff (get (move-strength cells-strength player move) player)
                next-diff (map #(worst-immediate-loss cells-strength new-turn % player) others)]
            (+ new-diff (apply min next-diff))))
        moves))
    ))
