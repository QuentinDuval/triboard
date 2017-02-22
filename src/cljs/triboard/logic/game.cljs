(ns triboard.logic.game
  (:require
    [cljs.spec :as s :include-macros true]
    [triboard.logic.board :as board]
    [triboard.logic.move :as move]
    [triboard.logic.player :as player]
    [triboard.logic.scores :as scores]
    [triboard.logic.turn :as turn]
    ))


(s/def ::game-state ::turn/turn)

(s/def ::game #(every? (partial s/valid? ::game-state) %))

(s/fdef current-state :args (s/tuple ::game) :ret ::game-state)
(s/fdef play-move :args (s/tuple ::game ::board/coord) :ret ::game)
(s/fdef undo-player-move :args (s/tuple ::game fn?) :ret ::game)

(defn new-game []
  (list (turn/new-init-turn)))

(defn current-state
  "Return the current state of the game:
   * Board (cells and owners)
   * Player score
   * Next player to play"
  [game]
  (first game))

(defn available-games
  "Get the reachable games from the current position, by player and position"
  [game]
  (move/map-game-tree
    (fn [next-turn] (delay (conj game (turn/complete-move next-turn)))) ;; TODO - create a real new game, and put the thing in cache (delay for each recursive game)
    (turn/available-turns (peek game))))

(defn play-move
  "Play a move, adding a new turn into the game"
  [game coord]
  (let [player (:player (current-state game))]
    (if-let [next-game (deref (get-in (available-games game) [player coord]))]
      next-game
      game)))

(defn undo-player-move
  "Cancel the last player move, getting back to the previous player move"
  [game is-ai?]
  (let [new-game (drop-while #(is-ai? (:player %)) (drop 1 game))]
    (if (empty? new-game)
      (take-last 1 game)
      new-game)))
