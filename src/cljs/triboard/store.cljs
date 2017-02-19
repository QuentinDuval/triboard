(ns triboard.store
  (:require
    [cljs.spec :as s]
    [reagent.core :as reagent]
    [triboard.ai.ai :as ai]
    [triboard.logic.board :as board]
    [triboard.logic.game :as game]
    [triboard.logic.player :as player])
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
(def current-state (reaction (game/current-state @game)))
(def current-player (reaction (:player @current-state)))
(def ai-player? (reaction (player/is-ai? @current-player)))

(def suggestions
  (reaction
    (if (and (:help @app-state) (not @ai-player?))
      (get (:moves @current-state) @current-player)
      {})))


;; -----------------------------------------
;; Public API (updates)
;; -----------------------------------------

(s/def ::game-event
  (s/or
    :no-params (s/tuple #{:new-game :toggle-help :restart :undo :ai-play})
    :player-move (s/tuple #{:player-move} ::board/coord)))

(s/fdef send-event! :args (s/tuple ::game-event))

(defn- send-event!
  [msg]
  (case (first msg)
    :new-game (swap! app-state assoc :game (game/new-game))
    :toggle-help (swap! app-state update :help not)
    :restart (swap! app-state update :game #(take-last 1 %))
    :undo (swap! app-state update :game game/undo-player-move player/is-ai?)
    :ai-play (swap! app-state update :game ai/play-best-move @current-player)
    :player-move (swap! app-state update :game game/play-move (second msg))
    ))
