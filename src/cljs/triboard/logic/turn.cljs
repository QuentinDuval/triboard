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
  [{:keys [board] :as game}]
  (assoc game :moves (move/all-available-moves board)))

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
  [game {:keys [winner looser taken] :as move}]
  {:pre [(move/move? move)]}
  (-> game
    (update-in [:board] move/apply-move move)
    (update-in [:scores winner] + (count taken))
    (update-in [:scores looser] - (count taken))
    ))


;; -----------------------------------------
;; Public API
;; -----------------------------------------

(defn get-move-at
  "Access the available moves for the provided player at the provided point"
  [game player point]
  (get-in game [:moves player point]))

(defn play-move
  "On player playing the move [x y] - update all the game state accordingly"
  [{:keys [player board] :as game} point]
  {:pre [(board/board? board)]}
  (if-let [moves (get-move-at game player point)]
    (->
      (reduce apply-move game moves)
      (apply-move (move/empty-cell-move player point))
      (with-available-moves)
      (with-next-player))
    game))

(defn new-init-turn []
  (-> {:board (board/new-board)
       :player (rand-nth cst/players)
       :moves {}
       :help false
       :scores scores/initial-scores}
    with-available-moves))
