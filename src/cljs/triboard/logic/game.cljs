(ns triboard.logic.game
  (:require
    [cljs.spec :as s :include-macros true]
    [triboard.logic.board :as board]
    [triboard.logic.player :as player]
    [triboard.logic.turn :as turn]))


(s/def ::game (s/coll-of ::turn/turn))

(s/fdef current-turn
  :args (s/cat :game ::game)
  :ret ::turn/turn)

(s/fdef play-move
  :args (s/cat :game ::game :coord ::board/coord)
  :ret ::game)

(s/fdef undo-player-move
  :args (s/cat :game ::game)
  :ret ::game)

;; ----------------------------------------------

(defn new-game []
  (list (turn/new-init-turn)))

(defn current-turn
  "Return the current state of the game:
   * Board (cells and owners)
   * Player score
   * Next player to play"
  [game]
  (first game))

(defn play-at
  "Play a move, adding a new turn into the game"
  [game coord]
  (let [curr-turn (current-turn game)]
    (if-let [transition (get (turn/transitions curr-turn) coord)]
      (conj game (turn/next-turn curr-turn transition))
      game)))

(defn undo-player-move
  "Cancel the last player move, getting back to the previous player move"
  [game]
  (let [new-game (drop-while (comp player/is-ai? :player) (drop 1 game))]
    (if (empty? new-game)
      (take-last 1 game)
      new-game)))

(defn restart-game
  "Restart the game, back to the first turn of the game"
  [game]
  (take-last 1 game))
