(ns triboard.core
  (:require [reagent.core :as reagent :refer [atom]]))

(enable-console-print!)

(def ^:const board-height 11)
(def ^:const board-width 17)

(defn init-state
  []
  [])

(defonce app-state
  (atom {:text "Hello Chestnut!"}))

(defn greeting []
  [:h1 (:text @app-state)])

(reagent/render [greeting]
  (js/document.getElementById "app"))
