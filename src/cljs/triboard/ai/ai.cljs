(ns triboard.ai.ai
  (:require
    [cljs.spec :as s :include-macros true]
    [triboard.ai.minimax :as ai-algo]
    [triboard.logic.board :as board]
    [triboard.logic.game :as game]
    [triboard.logic.scores :as scores]
    [triboard.logic.turn :as turn]))


;; -----------------------------------------
;; Private
;; -----------------------------------------

(defn- make-ai
  [player]
  (reify ai-algo/AIStrategy
    (eval-score [_ scores] (get scores player))
    (maximizing? [_ turn] (= (:player turn) player))
    ))

(defn- make-cheating-ai
  [player]
  (reify ai-algo/AIStrategy
    (eval-score [_ scores] (+ (:red scores) (:green scores)))
    (maximizing? [_ turn] (not= (:player turn) :blue))
    ))


;; -----------------------------------------

(defn- leaf-score
  "Evaluate the score of a leaf turn by looking at its transition
   In effect, it will look the score one level after"
  [ai {:keys [scores] :as turn}]
  (ai-algo/minimax-step ai turn
    (fn [_ transition]
      (ai-algo/eval-score ai (scores/update-scores scores transition))
      )))

(defn- tree-score
  "Implements the minimax recursion:
   * Call the leaf node evaluation if the depth is zero
   * Otherwise goes one level deeper"
  [ai turn depth]
  (if (zero? depth)
    (leaf-score ai turn)
    (ai-algo/minimax-step ai turn
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
    (ai-algo/minimax-step-by
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
  (let [turn (game/current-turn game)
        hard-mode? (focus-human-player? (:scores turn))
        ai ((if hard-mode? make-cheating-ai make-ai) (:player turn))
        coord (time (best-move ai turn))]  ;; TODO - Remove time + find a way to correlate with moves + sort then and take best
    coord))


;; -----------------------------------------
;; TESTS
;; -----------------------------------------

(defn benchmark []
  (let [g (game/new-game)]
    (time (dotimes [i 10]
            (time (find-best-move g))
            ))))
