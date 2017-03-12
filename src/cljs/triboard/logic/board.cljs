(ns triboard.logic.board
  (:require
    [cljs.spec :as s :include-macros true]
    [triboard.logic.constants :as cst]
    [triboard.logic.player :as player]
    [triboard.utils.algo :as algo]))


;; -----------------------------------------
;; Public Types
;; -----------------------------------------

(def width 16)
(def height 11)

(def coordinates
  (vec (for [x (range width) y (range height)] [x y])))

(s/def ::coord (set coordinates))

(s/def ::board
  (s/every
    (s/every ::player/owner :count height)
    :count width))

(s/fdef new-board
  :ret ::board)

(s/fdef convert-cell
  :args (s/cat :board ::board :coord ::coord :owner ::player/player)
  :ret ::board)

(s/fdef get-owner-at
  :args (s/cat :board ::board :coord ::coord)
  :ret ::player/player)

(s/fdef to-seq
  :args (s/cat :board ::board)
  :ret (s/every (s/tuple ::coord ::player/owner)))


;; -----------------------------------------
;; Public API
;; -----------------------------------------

(def empty-board
  "An empty board of `width` columns times `height` owners"
  (let [empty-column (vec (repeat height :none))]
    (vec (repeat width empty-column))))

(defn convert-cell
  [board coord owner]
  (assoc-in board coord owner))

(defn new-board
  "Creates a new board with initial positions of each players"
  []
  (->>
    (conj player/all :wall)
    (algo/randomly-pick-n-of-each cst/init-block-count coordinates)
    (reduce
      (fn [board [coord owner]] (assoc-in board coord owner))
      empty-board)))

(def get-owner-at get-in)

(defn to-seq
  "Access to the board as a list of coordinates with corresponding owner"
  [board]
  (for [coord coordinates] [coord (get-owner-at board coord)]))

(defn board->array
  "Converts the board into a JavaScript array (for performance)"
  [board]
  (into-array (map into-array board)))
