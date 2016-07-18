(ns triboard.core
  (:require [reagent.core :as reagent :refer [atom]]))

(enable-console-print!)

;; -----------------------------------------

(def board-width 16)
(def board-height 11)
(def init-block-count 12) ;; Init blocks for red, blue, green, gray

(def scale-factor 100)

;; -----------------------------------------

(defn new-board
  "Create a new board"
  []
  (vec
    (repeat board-width
      (vec (repeat board-height :empty))
      )))

;; -----------------------------------------

(defonce app-state
  (atom
    {:board (new-board)
     }))

(defn empty-cell
  "Draws an empty cell"
  [x y]
  [:rect {:width 0.9
          :height 0.9
          :x (+ 0.05 x)
          :y (+ 0.05 y)
          :fill "lightgrey"
          }])

(defn greeting []
  [:h1 "Triboard"
   (into
     [:svg#board
      {:view-box (str "0 0 " board-width " " board-height)}]
     (for [x (range board-width)
           y (range board-height)]
       ^{:key [x y]}
       (case (get-in @app-state [:board x y])
         :empty  [empty-cell x y])
       ))
   ])

;; -----------------------------------------

(reagent/render [greeting]
  (js/document.getElementById "app"))
