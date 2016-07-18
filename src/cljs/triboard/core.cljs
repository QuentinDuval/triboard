(ns triboard.core
  (:require [reagent.core :as reagent :refer [atom]]))

(enable-console-print!)

;; -----------------------------------------

(def board-width 16)
(def board-height 11)
(def init-block-count 12) ;; Init blocks for red, blue, green, gray

(def scale-factor 100)

;; -----------------------------------------

(def empty-board
  (let [column (vec (repeat board-height :empty))]
    (vec (repeat board-width column))))

(def all-positions
  (vec
    (for [x (range board-width)
          y (range board-height)]
      [x y])))

(defn draw-slices
  "Draw n * m slices from the collection"
  [n slices positions]
  (mapcat
    (fn [k] (map vector
              (repeat (nth slices k))
              (subvec positions (* k n) (* (inc k) n))
              ))
    (range (count slices))))

(defn init-positions
  "Create random initial positions for the players"
  []
  (draw-slices init-block-count
    [:blue :red :green :gray] (shuffle all-positions)))

(defn new-board
  "Create a new board"
  []
  (reduce (fn [r [color [x y]]]
            (prn [x y])
            (assoc-in r [x y] color))
    empty-board (init-positions)))

;; -----------------------------------------

(defonce app-state
  (atom
    {:board (new-board)
     }))

(defn empty-cell
  "Draws an empty cell"
  [x y color]
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
     (for [[x y] all-positions]
       ^{:key [x y]}
       (case (get-in @app-state [:board x y])
         :empty  [empty-cell x y]
         :blue [empty-cell x y]
         :red [empty-cell x y]
         :green [empty-cell x y]
         :gray [empty-cell x y])
       ))
   ])

;; -----------------------------------------

(reagent/render [greeting]
  (js/document.getElementById "app"))
