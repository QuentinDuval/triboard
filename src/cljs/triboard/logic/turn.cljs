(ns triboard.logic.turn
  (:require
    [cljs.spec :as s :include-macros true]
    [triboard.logic.board :as board]
    [triboard.logic.constants :as cst]
    [triboard.logic.transition :as transition]
    [triboard.logic.player :as player]
    [triboard.logic.scores :as scores]))


;; -----------------------------------------
;; Private
;; -----------------------------------------

(defn- who-can-play
  "Given the current player and the transitions for the next turn,
   returns the next player that has at least one transition available"
  [player transitions]
  (filter #(contains? transitions %) (player/next-three player)))

(defn- with-next-player
  "Complete the turn to compute to find the next player than can act
   * Requires to look-ahead for the next possible transitions
   * To save computation, keep this look-ahead available"
  [{:keys [board player] :as turn}]
  (let [transitions (transition/all-transitions board)
        next-player (first (who-can-play player transitions))]
    (-> turn
      (assoc :transitions (get transitions next-player))
      (assoc :player next-player)
      )))


;; -----------------------------------------
;; Public API
;; -----------------------------------------

(s/def ::turn
  (s/keys :req-un
    [::board/board
     ::player/player
     ::scores/scores]))

(s/fdef next-turn
  :args (s/cat :turn ::turn :transition ::transition/transition)
  :ret ::turn)

(s/fdef transitions
  :args (s/cat :turn ::turn)
  :ret (s/map-of ::transition/destination ::transition/transition))

;; -----------------------------------------

(defn new-init-turn []
  (with-next-player
    {:board (board/new-board)
     :player (rand-nth player/all)
     :transitions {}
     :scores (scores/initial-scores cst/init-block-count)}))

(defn next-turn
  "Apply the transtion to the current turn, yielding a new turn"
  [turn transition]
  (-> turn
    (update :board transition/apply-transition transition)
    (update :scores scores/update-scores transition)
    (with-next-player)))

(defn transitions
  "Return the transitions available for the next player"
  [turn]
  (:transitions turn))
