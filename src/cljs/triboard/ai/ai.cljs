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

(defn- maximizing-turn?
  [ai turn]
  (= (:player ai) (:player turn)))

(defn- min-max-step
  [ai turn on-transition]
  (apply
    (if (maximizing-turn? ai turn) max min)
    (map
      (fn [[_ transition]] (on-transition transition))
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

(defn benchmark []
  (time (dotimes [i 10]
          (time (play-best-move (game/new-game)))
          )))
