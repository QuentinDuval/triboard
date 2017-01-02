(ns triboard.logic.game
  (:require
    [triboard.logic.turn :as turn]
    ))


(defn new-game
  "Create a brand new game"
  []
  (list (turn/new-init-turn)))

(defn current-turn
  "Get the current turn available for a move"
  [game-turns]
  (first game-turns))

(defn play-move
  "Play a move, adding a new turn into the game"
  [game-turns coord]
  (conj game-turns
    (turn/play-move (current-turn game-turns) coord)))

(defn undo-player-move
  "Cancel the last player move, getting back to the previous player move"
  [old-turns is-ai?]
  (let [new-turns (drop-while #(is-ai? (:player %)) (drop 1 old-turns))]
    (if (empty? new-turns)
      (take-last 1 old-turns)
      new-turns)))
