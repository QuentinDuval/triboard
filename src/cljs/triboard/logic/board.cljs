(ns triboard.logic.board
  (:require
    [cljs.spec :as s :include-macros true]
    [triboard.logic.constants :as cst]
    [triboard.logic.player :as player]
    [triboard.utils.algo :as algo]
    ))


;; -----------------------------------------
;; Private
;; -----------------------------------------

(def width 16)
(def height 11)

(def coordinates
  (for [x (range width) y (range height)] [x y]))

(def empty-board
  (let [empty-column (vec (repeat height :none))]
    (vec (repeat width empty-column))))

(defn pick-n-cells-for-each-player
  "Pick N initial positions for each player and the walls"
  [n positions]
  (algo/zip
    positions
    (mapcat #(repeat n %) (conj player/all :wall))))

#_(deftype Board
    [grid]

    ILookup
    (-lookup
      [board coord not-found] (get-in (.-grid board) coord not-found))
    (-lookup
      [board coord] (-lookup board coord :wall))

    IAssociative
    (-contains-key? [board coord]
      (-lookup board coord nil))
    (-assoc [board coord owner]
      (Board. (assoc-in (.-grid board) coord owner)))

    ISeqable
    (-seq [board]
      (for [coord coordinates]
        [coord (get-in (.-grid board) coord)]))

    Object
    (toString [board]
      (.toString (.-grid board))
      ))


#_(defn test-board-type
    []
    (let [b (Board. empty-board)]
      (println (str b))
      (println (get b [0 0]))
      (println (contains? b [0 0]))
      (println (get (assoc b [0 0] :wall) [0 0]))
      (println (seq b))
      ))


;; -----------------------------------------
;; Public Types
;; -----------------------------------------

(s/def ::coord (set coordinates))
(s/def ::board (s/every (s/every ::player/cell :count 11) :count 16))

(s/fdef new-board
  :ret ::board)

(s/fdef to-seq
  :args (s/cat :board ::board)
  :ret (s/every (s/tuple ::coord ::player/cell)))


;; -----------------------------------------
;; Public API
;; -----------------------------------------

(defn convert-cell                                          ;; TODO -instance of assoc
  [board coord owner]
  (assoc-in board coord owner))

(defn new-board
  "Creates a new board with initial positions of each players"
  []
  (let [positions (shuffle coordinates)
        updates (pick-n-cells-for-each-player cst/init-block-count positions)]
    (reduce
      (fn [board [coord owner]] (convert-cell board coord owner))
      empty-board updates)))

(def get-owner-at get-in)                                   ;; TODO - instance of lookup

(defn to-seq                                                ;; TODO - instance of seq
  "Access to the board as a list of coordinates with corresponding owner"
  [board]
  (for [coord coordinates] [coord (get-owner-at board coord)]))

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
