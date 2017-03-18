(ns triboard.store
  (:require
    [cljs.spec :as s]
    [reagent.core :as reagent]
    [triboard.logic.game :as game]
    [triboard.logic.player :as player]
    [triboard.logic.turn :as turn])
  (:require-macros
    [reagent.ratom :refer [reaction]]))


;; -----------------------------------------
;; Private
;; -----------------------------------------

(defonce app-state
  (reagent/atom
    {:game (game/new-game)
     :help false}))


;; -----------------------------------------
;; Public API (reactive consumption)
;; -----------------------------------------

(def game (reaction (:game @app-state)))
(def current-turn (reaction (game/current-turn @game)))
(def current-player (reaction (:player @current-turn)))
(def ai-player? (reaction (player/is-ai? @current-player)))

(def suggestions
  (reaction
    (if (and (:help @app-state) (not @ai-player?))
      #(contains? (turn/transitions @current-turn) %)
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
    :undo (swap! app-state update :game game/undo-player-move)
    :play-at (swap! app-state update :game game/play-at (second msg))
    ))
