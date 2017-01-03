(ns triboard.store
  (:require
    [reagent.core :as reagent]
    [triboard.ai.ai :as ai]
    [triboard.logic.game :as game]
    [triboard.logic.turn :as turn])
  (:require-macros
    [reagent.ratom :refer [reaction]]))


;; -----------------------------------------
;; Private
;; -----------------------------------------

(def is-ai? #{:red :green})

(defonce app-state
  (reagent/atom
    {:game (game/new-game)
     :help false}))

(defn play-game-turn! [move]
  (swap! app-state update :game game/play-move move))


;; -----------------------------------------
;; Public API (reactive consumption)
;; -----------------------------------------

(def current-turn (reaction (game/current-turn (:game @app-state))))
(def current-player (reaction (turn/get-player @current-turn)))
(def ai-player? (reaction (is-ai? @current-player)))

(def suggestions
  (reaction
    (if (and (:help @app-state) (not (is-ai? @current-player)))
      (turn/get-moves-of @current-turn @current-player)
      {})))


;; -----------------------------------------
;; Public API (updates)
;; -----------------------------------------

(defn- send-event!
  [msg]
  (case (first msg)
    :new-game (swap! app-state assoc :game (game/new-game))
    :toggle-help (swap! app-state update :help not)
    :restart (swap! app-state update :game #(take-last 1 %))
    :undo (swap! app-state update :game game/undo-player-move is-ai?)
    :ai-play (play-game-turn! (ai/best-move @current-turn @current-player))
    :player-move (play-game-turn! (second msg))
    ))

