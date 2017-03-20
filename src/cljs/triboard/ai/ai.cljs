(ns triboard.ai.ai
  (:require
    [cljs.spec :as s :include-macros true]
    [triboard.logic.board :as board]
    [triboard.logic.game :as game]
    [triboard.logic.scores :as scores]
    [triboard.logic.turn :as turn]
    ))


;; -----------------------------------------
;; Private
;; -----------------------------------------

(def eval-counter (atom 0))

(defprotocol AIStrategy
  (eval-score [this scores] "Heuristic to score the value of a turn")
  (maximizing? [this turn] "Tell whether we are in min or max step"))

(defn- make-ai
  [player]
  (reify AIStrategy
    (eval-score [_ scores] (get scores player))
    (maximizing? [_ turn] (= (:player turn) player))
    ))

(defn- make-cheating-ai
  [player]
  (reify AIStrategy
    (eval-score [_ scores] (+ (:red scores) (:green scores)))
    (maximizing? [_ turn] (not= (:player turn) :blue))
    ))

;; -----------------------------------------

(defn- minimax-step
  "One stage of the minimax algorithm:
   * Apply the maximizing or mininizing step to all transitions of the turn
   * Evaluate the lower level using the on-transition function"
  [ai turn on-transition
   & {:keys [max-fn min-fn]
      :or {max-fn max, min-fn min}}]
  (swap! eval-counter inc)
  (apply
    (if (maximizing? ai turn) max-fn min-fn)
    (map
      (fn [[coord transition]] (on-transition coord transition))
      (turn/transitions turn))))

(defn- minimax-step-by
  [key-fn ai turn on-transition]
  (minimax-step
    ai turn on-transition
    :min-fn (partial min-key key-fn) ;; Lambda does not work here
    :max-fn (partial max-key key-fn)))


;; -----------------------------------------

(defn- leaf-score
  "Evaluate the score of a leaf turn by looking at its transition
   In effect, it will look the score one level after"
  [ai {:keys [scores] :as turn}]
  (minimax-step ai turn
    (fn [_ transition]
      ;; TODO - Plug other ways to score (like counting available transitions)
      (eval-score ai (scores/update-scores scores transition))
      )))

(defn- tree-score
  "Implements the minimax recursion:
   * Call the leaf node evaluation if the depth is zero
   * Otherwise goes one level deeper"
  [ai turn depth]
  (if (zero? depth)
    (leaf-score ai turn)
    (minimax-step ai turn
      (fn [_ transition]
        (tree-score ai
          (turn/next-turn turn transition)
          (dec depth))))
    ))

(defn- best-move
  "The top level of the minimax algorithm
   * Triggers lower level minimax evaluations
   * Keeps the transition that led to the max"
  [ai turn]
  (first
    (minimax-step-by
      second ai turn
      (fn [coord transition]
        (let [new-turn (turn/next-turn turn transition)]
          [coord (tree-score ai new-turn 1)]))
      )))

(defn- focus-human-player?
  "High level AI to decude whether the AI ally against the human player"
  [{:keys [blue red green] :as scores}]
  (let [human-score (* 1.1 blue)]
    (and (< red human-score) (< green human-score))))

;; -----------------------------------------
;; Public API
;; -----------------------------------------

(s/fdef find-best-move
  :args (s/cat :game ::game/game)
  :ret ::board/coord)

(defn find-best-move
  "Find the best available move for the current player"
  [game]
  (reset! eval-counter 0)
  (let [turn (game/current-turn game)
        hard-mode? (focus-human-player? (:scores turn))
        ai ((if hard-mode? make-cheating-ai make-ai) (:player turn))
        coord (time (best-move ai turn))]  ;; TODO - Remove time + find a way to correlate with moves + sort then and take best
    (js/console.log @eval-counter)
    coord))


;; -----------------------------------------
;; TESTS
;; -----------------------------------------

(defn benchmark []
  (let [g (game/new-game)]
    (time (dotimes [i 10]
            (time (find-best-move g))
            ))))
