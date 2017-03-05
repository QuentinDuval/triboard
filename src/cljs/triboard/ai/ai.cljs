(ns triboard.ai.ai
  (:require
    [cljs.spec :as s :include-macros true]
    [triboard.logic.game :as game]
    [triboard.logic.scores :as scores]
    [triboard.logic.turn :as turn]
    ))


;; -----------------------------------------
;; Private
;; -----------------------------------------

(defprotocol AIStrategy
  (optimized-score [this scores] "Extract data from the score to compare")
  (maximizing-turn? [this turn] "Indicates whether we are in min or max"))

(defn- make-ai
  [player]
  (reify AIStrategy
    (optimized-score [_ scores] (get scores player))
    (maximizing-turn? [_ turn] (= (:player turn) player))
    ))

(defn- make-cheating-ai
  [player]
  (reify AIStrategy
    (optimized-score [_ scores] (+ (:red scores) (:green scores)))
    (maximizing-turn? [_ turn] (not= (:player turn) :blue))
    ))

;; -----------------------------------------

(defn- min-max-step-by
  [compare-key ai turn on-transition]
  (apply
    (if (maximizing-turn? ai turn) max-key min-key)
    compare-key
    (map
      (fn [[coord transition]] (on-transition coord transition))
      (turn/player-transitions turn))))

(defn- min-max-step
  [ai turn on-transition]
  (min-max-step-by identity ai turn on-transition))         ;; TODO - avoid projection

;; -----------------------------------------

(defn- leaf-score
  [ai {:keys [scores] :as turn}]
  (min-max-step ai turn
    (fn look-ahead [_ transition]
      (optimized-score ai (scores/update-scores scores transition))
      )))

(defn- tree-score
  [ai turn depth]
  (if (= 0 depth)
    (leaf-score ai turn)
    (min-max-step ai turn
      (fn look-lower [_ transition]
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

;; -----------------------------------------
;; Public API
;; -----------------------------------------

(s/fdef play-best-move
  :args (s/tuple ::game/game)
  :ret ::game/game)

(defn- is-player-first?
  [{:keys [blue red green] :as scores}]
  (and (< red blue) (< green blue)))

(defn- play-best-move
  [game]
  (let [turn (game/current-turn game)
        hard-mode? (is-player-first? (:scores turn))
        ai ((if hard-mode? make-cheating-ai make-ai) (:player turn))]
    (game/play-at game
      (time (best-move ai turn)) ;; TODO - Remove time
      )))


;; -----------------------------------------
;; TESTS
;; -----------------------------------------

(defn benchmark []
  (let [g (game/new-game)]
    (time (dotimes [i 10]
            (time (play-best-move g))
            ))))
