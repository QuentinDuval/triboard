(ns triboard.logic.turn
  (:require
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

(defn- with-next-player
  "Find the next player to act - dismiss those that cannot play any move"
  [{:keys [moves player] :as game}]
  (let [nexts (take 3 (iterate next-player (next-player player)))]
    (assoc game :player
                (some #(if (get moves %) % false) nexts))
    ))

(defn- apply-move
  [turn move]
  {:pre [(move/move? move)]}
  (-> turn
    (update :board move/apply-move move)
    (update :scores scores/update-scores move)
    ))


;; -----------------------------------------
;; Public API
;; -----------------------------------------

(defn new-init-turn []
  (-> {:board (board/new-board)
       :player (rand-nth cst/players)
       :moves {}
       :scores scores/initial-scores}
    with-available-moves))

(defn current-player [turn] (:player turn))
(defn get-board [turn] (:board turn))
(defn get-scores [turn] (:scores turn))
(defn game-over? [turn] (nil? (current-player turn)))

(defn get-move-at
  "Access the available moves for the provided player at the provided point"
  [turn player point]
  (get-in turn [:moves player point]))

(defn play-move
  "On player playing the move [x y] - update all the game state accordingly"
  [{:keys [player board] :as turn} point]
  {:pre [(board/board? board)]}
  (if-let [moves (get-move-at turn player point)]
    (->
      (reduce apply-move turn moves)
      (apply-move (move/empty-cell-move player point))
      (with-available-moves)
      (with-next-player))
    turn))
