(ns triboard.logic.game
  (:require
    [cljs.spec :as s :include-macros true]
    [triboard.logic.board :as board]
    [triboard.logic.turn :as turn]
    ))


(s/def ::game #(every? (partial s/valid? ::turn/turn) %))
(s/fdef current-turn :args (s/tuple ::game) :ret ::turn/turn)
(s/fdef play-move :args (s/tuple ::game ::board/coord) :ret ::game)
(s/fdef undo-player-move :args (s/tuple ::game fn?) :ret ::game)

(defn new-game []
  (list (turn/new-init-turn)))

(defn current-turn
  "Get the current turn available for a move"
  [game]
  (first game))

(defn play-move
  "Play a move, adding a new turn into the game"
  [game coord]
  (let [curr-turn (current-turn game)
        next-turn (turn/play-move curr-turn coord)]
    (if (not= next-turn curr-turn)
      (conj game next-turn)
      game)))

(defn undo-player-move
  "Cancel the last player move, getting back to the previous player move"
  [game is-ai?]
  (let [new-game (drop-while #(is-ai? (:player %)) (drop 1 game))]
    (if (empty? new-game)
      (take-last 1 game)
      new-game)))
