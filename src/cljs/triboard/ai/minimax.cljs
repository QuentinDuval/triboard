(ns triboard.ai.minimax
  (:require [triboard.logic.turn :as turn]))


(defprotocol AIStrategy
  (eval-turn [this turn] "Heuristic to score the value of a turn")
  (maximizing? [this turn] "Whether the turn should minimize or maximize the transitions"))


(defn minimax-step
  "One stage of the minimax algorithm:
   * Apply the maximizing or mininizing step to all transitions of the turn
   * Evaluate the lower level using the on-transition function"
  [ai turn on-transition
   & {:keys [max-fn min-fn]
      :or {max-fn max, min-fn min}}]
  (apply
    (if (maximizing? ai turn) max-fn min-fn)
    (map
      (fn [[coord transition]] (on-transition coord transition))
      (turn/transitions turn))))

(defn minimax-step-by
  "One stage of the minimax algorithm, with custom comparison functions"
  [key-fn ai turn on-transition]
  (minimax-step
    ai turn on-transition
    :min-fn (partial min-key key-fn) ;; Lambda does not work here
    :max-fn (partial max-key key-fn)))

(defn minimax
  "Implements the minimax recursion:
   * Call the leaf node evaluation if the depth is zero
   * Otherwise goes one level deeper"
  [ai turn depth]
  (if (zero? depth)
    (eval-turn ai turn)
    (minimax-step ai turn
      (fn [_ transition]
        (minimax ai
          (turn/next-turn turn transition)
          (dec depth))))
    ))
