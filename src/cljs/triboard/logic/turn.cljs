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

(defn apply-transition
  "Apply the transtion to the current turn, yielding a new turn"
  [turn transition]
  (let [new-board (transition/apply-transition (:board turn) transition)
        new-scores (reduce scores/update-scores (:scores turn) transition)]
    (-> turn
      (assoc :board new-board)
      (assoc :scores new-scores)
      (with-next-player)
      )))

(defn play-move
  "On player playing the move [x y] - update all the game state accordingly"
  [{:keys [player board] :as turn} coord]
  (if-let [transitions (get-in turn [:transitions player coord])]
    (apply-transition turn transitions)))

(defn player-transitions
  "Return the transitions available for the next player"
  [turn]
  (get-in turn [:transitions (:player turn)]))
