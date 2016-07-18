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
    (conj players :wall)
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

(defn range-single-coord
  "Creates a range along one coordinate"
  [x dx max-val]
  (let [end-range (if (neg? dx) 0 max-val)]
    (range x end-range dx)))

(defn range-coord
  "Give all the coordinate in the provided direction"
  [[xi yi] [dx dy]]
  (map vector
    (range-single-coord xi dx board-width)
    (range-single-coord yi dy board-height)))

(defn is-cell-owned?
  "Indicates whether a cell is owned by any player"
  [cell]
  (and (not= cell :empty) (not= cell :wall)))

(defn range-cells ;; TODO - Use the concept of sentinel for the walls?
  "Give all the cells in the provided direction - until you reach an empty / blocked / wall cell"
  [board [x y] [dx dy]]
  (eduction
    (comp
      (map #(get-in board %))
      (take-while is-cell-owned?)) ;; TODO - Use partition ?
    (range-coord [x y] [dx dy])))

;; -----------------------------------------

(defonce app-state (atom (new-game)))
(def board (reagent/cursor app-state [:board]))

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
         :wall [rect-cell x y "gray"])
       ))
   ])

;; -----------------------------------------

(reagent/render [greeting]
  (js/document.getElementById "app"))
