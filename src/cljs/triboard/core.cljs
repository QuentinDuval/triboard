(ns triboard.core
  (:require [reagent.core :as reagent :refer [atom]]))

(enable-console-print!)

;; -----------------------------------------

(def board-width 16)
(def board-height 11)
(def init-block-count 12) ;; Init blocks for red, blue, green, gray
(def players [:blue :red :green])

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
    (conj players :gray)
    (shuffle all-positions)))

(defn new-board []
  (reduce (fn [r [color [x y]]]
            (prn [x y])
            (assoc-in r [x y] color))
    empty-board (init-positions)))

(defn new-game []
  {:board (new-board)
   :turn 0
   ;; From the turn you can deduce the number of turn left
   ;; There is an invariant holding: total score = 12 * 3 + turn
   :scores
   {:blue 12
    :red 12
    :green 12}})

;; -----------------------------------------

(defonce app-state (atom (new-game)))

;; -----------------------------------------

(defn rect-cell
  [x y color options]
  [:rect
   (merge
     {:width 0.9
      :height 0.9
      :x (+ 0.05 x)
      :y (+ 0.05 y)
      :fill color}
     options)
   ])

(defn empty-cell
  [x y]
  (rect-cell x y "lightgray"
    {:on-click #(prn [x y])}
    ))

(defn greeting []
  [:h1 "Triboard"
   (into
     [:svg#board
      {:view-box (str "0 0 " board-width " " board-height)}]
     (for [[x y] all-positions]
       ^{:key [x y]}
       (case (get-in @app-state [:board x y])
         :empty  [empty-cell x y]
         :blue [rect-cell x y "blue"]
         :red [rect-cell x y "red"]
         :green [rect-cell x y "green"]
         :gray [rect-cell x y "gray"])
       ))
   ])

;; -----------------------------------------

(reagent/render [greeting]
  (js/document.getElementById "app"))
