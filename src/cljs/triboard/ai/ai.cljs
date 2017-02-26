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

(defn- max-by
  [key-fn coll]
  (apply max-key key-fn coll))

(defn- min-max-step
  [ai turn recur-fn]
  (apply
    (if (= (:player ai) (:player turn)) max min)
    (map
      (fn [[_ transition]] (recur-fn transition))
      (turn/player-transitions turn))))

;; -----------------------------------------

(defn- leaf-score
  [ai turn]
  (min-max-step ai turn
    (fn look-ahead [transition]
      (get
        (reduce scores/update-scores (:scores turn) transition)
        (:player ai)) ;; TODO - Blue score + min-max modification for AI Aliance
      )))

(defn tree-score
  [ai turn depth]
  (if (= 0 depth)
    (leaf-score ai turn)
    (min-max-step ai turn
      (fn look-lower [transition]
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
      (max-by :scoring
        (map
          (fn [[coord _]]
            (let [new-game (game/play-move game coord)
                  new-turn (game/current-turn new-game)]
              {:game new-game
               :scoring (tree-score ai new-turn 1)}))
          (turn/player-transitions turn)))
      )))


;; -----------------------------------------
;; TESTS
;; -----------------------------------------

(defn test-on-game
  [g]
  (let [turn (game/current-turn g)
        ai (make-ai (:board turn) (:player turn))]
    (tree-score ai (game/current-turn g) 2)))

(defn test-score-tree [] (test-on-game (game/new-game)))

(defn benchmark []
  (time (dotimes [i 10]
          (time (test-score-tree))
          )))
