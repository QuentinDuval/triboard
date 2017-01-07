(ns triboard.logic.board
  (:require
    [cljs.spec :as s :include-macros true]
    [cljs.spec.test :as stest :include-macros true]
    [cljs.spec.impl.gen :as gen]
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
;; Public Types
;; -----------------------------------------

(s/def ::cell cst/cell?)
(s/def ::board (s/every (s/every cst/cell? :count 11) :count 16))
(s/def ::coord (s/tuple integer? integer?))

(s/fdef new-board
  :ret ::board)

(s/fdef to-iterable
  :args (s/cat :board ::board)
  :ret (s/every (s/tuple ::coord cst/cell?)))

(s/fdef empty-cells
  :args (s/cat :board ::board)
  :ret (partial every? #(s/valid? ::coord %))) ;; TODO - Fails with coll-of ::coord or s/every. WHY?

;; TODO - Remove these predicates
(defn board? [b] (s/valid? ::board b))
(defn coord? [c] (s/valid? ::coord c))


;; -----------------------------------------
;; Public API
;; -----------------------------------------

(defn new-board
  "Creates a new board with initial positions of each players"
  []
  (reduce
    (fn [r [color point]] (assoc-in r point color))
    empty-board (init-positions)))

(def get-cell-at get-in)

(defn to-iterable
  "Access to the board as a list of coordinates with corresponding owner"
  [board]
  (for [[x y :as coord] cst/all-positions]
    [coord (get-cell-at board coord)]
    ))

(defn empty-cells
  "Access to the empty cells of the board as a list of coordinates"
  [board]
  (eduction
    (filter #(= (get-cell-at board %) :empty))
    cst/all-positions))

;; (s/check-asserts true)

;; You can test this function by using
;; (empty-cells (first (gen/sample (s/gen ::board) 1)))
;; (stest/check `empty-cells)
;; (s/explain `to-iterable to-iterable)
;; (s/explain `empty-cells empty-cells)
;; (stest/instrument `empty-cells)
