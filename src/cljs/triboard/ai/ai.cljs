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

(defn- make-ai
  [board player]
  {:player player})

(defn- summarize-score
  [ai scores]
  (get scores (:player ai))) ;; TODO - Blue score + min-max modification for AI Aliance

(defn- maximizing-turn?
  [ai turn]
  (= (:player ai) (:player turn)))

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
  (min-max-step-by identity ai turn on-transition)) ;; TODO - avoid projection

;; -----------------------------------------

(defn- leaf-score
  [ai {:keys [scores] :as turn}]
  (min-max-step ai turn
    (fn look-ahead [_ transition]
      (summarize-score ai (reduce scores/update-scores scores transition))
      )))

(defn tree-score
  [ai turn depth]
  (if (= 0 depth)
    (leaf-score ai turn)
    (min-max-step ai turn
      (fn look-lower [_ transition]
        (tree-score ai
          (turn/apply-transition turn transition)
          (dec depth))))
    ))

;; -----------------------------------------
;; Public API
;; -----------------------------------------

(s/fdef play-best-move
  :args (s/tuple ::game/game)
  :ret ::game/game)

(defn- play-best-move
  [game]
  (let [turn (game/current-turn game)
        ai (make-ai (:board turn) (:player turn))]
    (:game
      (min-max-step-by
        :score ai turn
        (fn [coord _]
          (let [new-game (game/play-move game coord)
                new-turn (game/current-turn new-game)]
            {:game new-game                             ;; TODO - Not sure keeping game in memory is wise
             :score (tree-score ai new-turn 1)})))
      )))


;; -----------------------------------------
;; TESTS
;; -----------------------------------------

(defn benchmark []
  (time (dotimes [i 10]
          (time (play-best-move (game/new-game)))
          )))
