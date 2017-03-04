(ns triboard.logic.board
  (:require
    [cljs.spec :as s :include-macros true]
    [triboard.logic.constants :as cst]
    [triboard.logic.player :as player]
    ))


;; -----------------------------------------
;; Private
;; -----------------------------------------

(def empty-board
  (let [column (vec (repeat cst/board-height :empty))]
    (vec (repeat cst/board-width column))))

(defn pick-n-cells-for-each-player
  "Pick N initial positions for each player and the walls"
  [n positions]
  (let [tiles (conj player/all :wall)]
    (map vector positions (mapcat #(repeat n %) tiles))))


;; -----------------------------------------
;; Public Types
;; -----------------------------------------

(s/def ::coord (set cst/all-positions))
(s/def ::board (s/every (s/every ::player/cell :count 11) :count 16))

(s/fdef new-board
  :ret ::board)

(s/fdef to-iterable
  :args (s/cat :board ::board)
  :ret (s/every (s/tuple ::coord ::player/cell)))

(s/fdef empty-cells
  :args (s/cat :board ::board)
  :ret (partial every? #(s/valid? ::coord %)))              ;; TODO - Fails with coll-of ::coord or s/every. WHY?


;; -----------------------------------------
;; Public API
;; -----------------------------------------

(defn convert-cell
  [board coord owner]
  (assoc-in board coord owner))

(defn new-board
  "Creates a new board with initial positions of each players"
  []
  (let [positions (shuffle cst/all-positions)
        updates (pick-n-cells-for-each-player cst/init-block-count positions)]
    (reduce
      (fn [board [coord owner]] (convert-cell board coord owner))
      empty-board updates)))

(def get-cell-at get-in)

(defn to-iterable  ;; TODO - instance of seq
  "Access to the board as a list of coordinates with corresponding owner"
  [board]
  (for [coord cst/all-positions] [coord (get-cell-at board coord)]))

(defn empty-cells
  "Access to the empty cells of the board as a list of coordinates"
  [board]
  (eduction
    (filter #(= (get-cell-at board %) :empty))
    cst/all-positions))

(defn board->array
  "Converts the board into a JavaScript array (for performance)"
  [board]
  (into-array (map into-array board)))


#_(defn test-array
    []
    (let [n 1000000
          a (into-array (range n))]
      (time (dotimes [i n]
              (aget a n)
              ))
      ))

#_(defn test-vector
    []
    (let [n 1000000
          a (into [] (range n))]
      (time (dotimes [i n]
              (get a n)
              ))
      ))
