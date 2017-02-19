(ns triboard.logic.turn
  (:require
    [cljs.spec :as s :include-macros true]
    [triboard.logic.constants :as cst]
    [triboard.logic.board :as board]
    [triboard.logic.move :as move]
    [triboard.logic.scores :as scores]
    ))


;; -----------------------------------------
;; Private
;; -----------------------------------------

(defn- with-available-moves
  "Add the available moves on the board"
  [{:keys [board] :as turn}]
  (assoc turn :moves (move/all-available-moves board)))

(defn- next-player
  [player]
  (case player
    :blue :red
    :red :green
    :green :blue))

(defn- next-3-players
  [player]
  (take 3 (iterate next-player (next-player player))))

(defn- with-next-player
  "Find the next player to act - dismiss those that cannot play any move"
  [{:keys [moves player] :as turn}]
  (let [who-can-play (filter #(get moves %) (next-3-players player))]
    (assoc turn :player (first who-can-play))
    ))

(defn- apply-moves
  [turn moves]
  (let [new-board (reduce move/apply-conversion (:board turn) moves)
        new-scores (reduce scores/update-scores (:scores turn) moves)]
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
     ::cst/player
     ::move/available-moves
     ::scores/scores]))

(defn new-init-turn []
  (-> {:board (board/new-board)
       :player (rand-nth cst/players)
       :moves {}
       :scores scores/initial-scores}
    with-available-moves
    with-next-player))

(defn get-player [turn] (:player turn))
(defn get-board [turn] (:board turn))
(defn get-scores [turn] (:scores turn))

(defn get-moves-of
  "Access the available moves for the provided player, by coordinates"
  [turn player]
  (get (get-moves turn) player))

(s/fdef play-move
  :args (s/cat :turn ::turn :point ::board/coord)
  :ret ::turn)

(defn play-move
  "On player playing the move [x y] - update all the game state accordingly"
  [{:keys [player board] :as turn} point]
  (if-let [moves (get (get-moves-of turn player) point)]
    (-> turn
      (apply-moves (conj moves (move/empty-cell-conversion player point)))
      (with-available-moves)
      (with-next-player))
    turn))
