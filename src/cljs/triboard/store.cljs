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
(def ai-player? (reaction (player/is-ai? (:player @current-turn))))

(def suggestions
  (reaction
    (if (and (:help @app-state) (not @ai-player?))
      #(contains? (turn/transitions @current-turn) %)
      (constantly false))))


;; -----------------------------------------
;; Public API (updates)
;; -----------------------------------------

(defn swap-game!
  [update-fn & args]
  (apply swap! app-state update :game update-fn args))

(defn toggle-help!
  []
  (swap! app-state update :help not))
