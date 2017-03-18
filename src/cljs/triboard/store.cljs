(ns triboard.store
  (:require
    [cljs.spec :as s]
    [reagent.core :as reagent]
    [triboard.logic.board :as board]
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
(def ai-player? (reaction (player/is-ai? (:player @current-turn))))

(def suggestions
  (reaction
    (if (and (:help @app-state) (not @ai-player?))
      #(contains? (turn/transitions @current-turn) %)
      (constantly false))))


;; -----------------------------------------
;; Public API (updates)
;; -----------------------------------------

(s/def ::store-event
  (s/or
    :new-game (s/tuple #{:new-game})
    :toggle-help (s/tuple #{:toggle-help})
    :restart (s/tuple #{:restart})
    :undo (s/tuple #{:undo})
    :play-at (s/tuple #{:play-at} ::board/coord)
    ))

(s/fdef send-event!
  :args (s/cat :event ::store-event))

(defn- send-event!
  [msg]
  (case (first msg)
    :new-game (swap! app-state assoc :game (game/new-game))
    :toggle-help (swap! app-state update :help not)
    :restart (swap! app-state update :game game/restart-game)
    :undo (swap! app-state update :game game/undo-player-move)
    :play-at (swap! app-state update :game game/play-at (second msg))
    ))
