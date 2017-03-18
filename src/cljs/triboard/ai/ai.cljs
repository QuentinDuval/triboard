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

(defn- min-max-step
  [ai turn on-transition
   & {:keys [max-fn min-fn]
      :or {max-fn max, min-fn min}}]
  (swap! eval-counter inc)
  (apply
    (if (maximizing? ai turn) max-fn min-fn)
    (map
      (fn [[coord transition]] (on-transition coord transition))
      (turn/transitions turn))))

(defn- min-max-step-by
  [key-fn ai turn on-transition]
  (min-max-step
    ai turn on-transition
    :min-fn (partial min-key key-fn) ;; Lambda does not work here
    :max-fn (partial max-key key-fn)))


;; -----------------------------------------

(defn- leaf-score
  [ai {:keys [scores] :as turn}]
  (min-max-step ai turn
    (fn [_ transition]
      (eval-score ai (scores/update-scores scores transition))
      )))

(defn- tree-score
  [ai turn depth]
  (if (zero? depth)
    (leaf-score ai turn)
    (min-max-step ai turn
      (fn [_ transition]
        (tree-score ai
          (turn/next-turn turn transition)
          (dec depth))))
    ))

(defn- best-move
  [ai turn]
  (first
    (min-max-step-by
      second ai turn
      (fn [coord transition]
        (let [new-turn (turn/next-turn turn transition)]
          [coord (tree-score ai new-turn 1)]))
      )))

(defn- focus-human-player?
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
