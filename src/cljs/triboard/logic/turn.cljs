(ns triboard.logic.turn
  (:require
    [cljs.spec :as s :include-macros true]
    [triboard.logic.board :as board]
    [triboard.logic.move :as move]
    [triboard.logic.player :as player]
    [triboard.logic.scores :as scores]
    ))


;; -----------------------------------------
;; Private
;; -----------------------------------------

(defn- with-available-moves
  "Add the available moves on the board"
  [{:keys [board] :as turn}]
  (assoc turn :moves (move/available-transitions board)))

(defn- with-next-player
  "Find the next player to act - dismiss those that cannot play any move"
  [{:keys [moves player] :as turn}]
  (let [who-can-play (filter #(get moves %) (player/next-three player))]
    (assoc turn :player (first who-can-play))
    ))

(defn- apply-moves
  [turn moves]
  (let [new-board (-> moves :board)
        new-scores (reduce scores/update-scores (:scores turn) (:transition moves))] ;; TODO - Factor in move
    (-> turn
      (assoc :board new-board)
      (assoc :scores new-scores)
      )))


;; -----------------------------------------
;; Public API
;; -----------------------------------------

(s/def ::turn
  (s/keys :req-un
    [::board/board
     ::player/player
     ::scores/scores]))

(defn new-init-turn []
  (-> {:board (board/new-board)
       :player (rand-nth player/all)
       :moves {}
       :scores scores/initial-scores}
    with-available-moves
    with-next-player))

(s/fdef play-move
  :args (s/cat :turn ::board/coord)
  :ret ::turn)

(defn play-move
  "On player playing the move [x y] - update all the game state accordingly"
  [{:keys [player board] :as turn} point]
  (if-let [moves (get-in turn [:moves player point])]
    (-> turn
      (apply-moves moves)
      (with-available-moves)
      (with-next-player))))

#_(defn complete-move
  [turn]
  (-> turn (with-available-moves) (with-next-player)))

;; TODO - Available moves from the current position
;; TODO - To be used inside the game

#_(defn available-turns
  "Get the available turns from the current turn, by player and positions"
  [turn]
  (move/map-game-tree
    #(apply-moves turn %)
    (move/available-transitions (:board turn))))
