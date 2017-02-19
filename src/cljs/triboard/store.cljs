(ns triboard.store
  (:require
    [cljs.spec :as s]
    [reagent.core :as reagent]
    [triboard.ai.ai :as ai]
    [triboard.logic.board :as board]
    [triboard.logic.game :as game])
  (:require-macros
    [reagent.ratom :refer [reaction]]))


;; -----------------------------------------
;; Private
;; -----------------------------------------

(def ^:private is-ai? #{:red :green})

(defonce app-state
  (reagent/atom
    {:game (game/new-game)
     :help false}))

(defn- play-game-turn! [move]
  (swap! app-state update :game game/play-move move))


;; -----------------------------------------
;; Public API (reactive consumption)
;; -----------------------------------------

(def game (reaction (:game @app-state)))
(def current-state (reaction (game/current-state @game)))
(def current-player (reaction (:player @current-state)))
(def ai-player? (reaction (is-ai? @current-player)))

(def suggestions
  (reaction
    (if (and (:help @app-state) (not (is-ai? @current-player)))
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
    :undo (swap! app-state update :game game/undo-player-move is-ai?)
    :ai-play (play-game-turn! (ai/best-move @game @current-player))
    :player-move (play-game-turn! (second msg))
    ))
