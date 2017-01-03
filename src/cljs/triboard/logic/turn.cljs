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
  (let [nexts (take 3 (iterate next-player (next-player player)))
        next-valid (some #(if (get moves %) % false) nexts)]
    (assoc game :player next-valid)
    ))

(defn- apply-moves
  [turn moves]
  {:pre [(move/moves? moves)]}
  (let [new-board (reduce move/apply-move (:board turn) moves)
        new-scores (reduce scores/update-scores (:scores turn) moves)]
    (-> turn
      (assoc :board new-board)
      (assoc :scores new-scores)
      )))


;; -----------------------------------------
;; Public API
;; -----------------------------------------

(defn new-init-turn []
  (-> {:board (board/new-board)
       :player (rand-nth cst/players)
       :moves {}
       :scores scores/initial-scores}
    with-available-moves))

(defn get-player [turn] (:player turn))
(defn get-board [turn] (:board turn))
(defn get-scores [turn] (:scores turn))
(defn get-moves [turn] (:moves turn))
(defn game-over? [turn] (nil? (get-player turn)))

(defn get-move-at
  "Access the available moves for the provided player at the provided point"
  [turn player point]
  (get-in (get-moves turn) [player point]))

(defn play-move
  "On player playing the move [x y] - update all the game state accordingly"
  [{:keys [player board] :as turn} point]
  {:pre [(board/board? board)]}
  (if-let [moves (get-move-at turn player point)]
    (-> turn
      (apply-moves (conj moves (move/empty-cell-move player point)))
      (with-available-moves)
      (with-next-player))
    turn))
