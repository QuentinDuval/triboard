(ns triboard.logic.board
  (:require
    [triboard.logic.constants :as cst]
    ))


;; -----------------------------------------
;; Private
;; -----------------------------------------

(def empty-board
  (let [column (vec (repeat cst/board-height :empty))]
    (vec (repeat cst/board-width column))))


(defn- draw-slices
  "Draw n * m slices from the collection"
  [n slices positions]
  (mapcat
    (fn [k] (map vector
              (repeat (nth slices k))
              (subvec positions (* k n) (* (inc k) n))
              ))
    (range (count slices))))

(defn- init-positions
  "Create random initial positions for the players"
  []
  (draw-slices cst/init-block-count
    (conj cst/players :wall)
    (shuffle cst/all-positions)))


;; -----------------------------------------
;; Public API
;; -----------------------------------------

(defn board?
  "A board is a vector of vector of cells"
  [b]
  (every? #(every? cst/cell? %) b))

(defn coord?
  "A cell is a pair of integer"
  [p]
  (and
    (integer? (first p))
    (integer? (second p))
    (= 2 (count p))))

(defn new-board []
  {:post [(board? %)]}
  (reduce
    (fn [r [color point]] (assoc-in r point color))
    empty-board (init-positions)))

(defn get-cell-at
  [board coord]
  {:pre [(board? board) (coord? coord)]}
  (get-in board coord))
