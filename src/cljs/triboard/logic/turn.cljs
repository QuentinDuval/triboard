(ns triboard.logic.turn
  (:require
    [cljs.spec :as s :include-macros true]
    [triboard.logic.board :as board]
    [triboard.logic.transition :as transition]
    [triboard.logic.player :as player]
    [triboard.logic.scores :as scores]
    ))


;; -----------------------------------------
;; Private
;; -----------------------------------------

(defn- with-next-player
  "Complete the turn to compute to find the next player than can act
   * Requires to look-ahead for the next possible transitions
   * To save computation, keep this look-ahead available"
  [{:keys [board player] :as turn}]
  (let [transitions (transition/all-transitions board)
        who-can-play (filter #(get transitions %) (player/next-three player))]
    (-> turn
      (assoc :transitions transitions)
      (assoc :player (first who-can-play))
      )))


;; -----------------------------------------
;; Public API
;; -----------------------------------------

(s/def ::turn
  (s/keys :req-un
    [::board/board
     ::player/player
     ::transition/transitions
     ::scores/scores]))

(defn new-init-turn []
  (with-next-player
    {:board (board/new-board)
     :player (rand-nth player/all)
     :transitions {}
     :scores scores/initial-scores}))

(s/fdef play-transition
  :args (s/cat :turn ::board/coord)
  :ret ::turn)

(defn next-turn
  "Apply the transtion to the current turn, yielding a new turn"
  [turn transition]
  (-> turn
    (update :board transition/apply-transition transition)
    (update :scores scores/update-scores transition)
    with-next-player))

(defn player-transitions
  "Return the transitions available for the next player"
  [turn]
  (get-in turn [:transitions (:player turn)]))
