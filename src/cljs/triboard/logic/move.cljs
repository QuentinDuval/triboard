(ns triboard.logic.move
  (:require
    [cljs.spec :as s :include-macros true]
    [triboard.logic.board :as board]
    [triboard.logic.constants :as cst]
    [triboard.logic.player :as player]
    ))


;; -----------------------------------------
;; Public Types
;; -----------------------------------------

(s/def ::point ::board/coord)
(s/def ::taken (s/coll-of ::board/coord))
(s/def ::winner ::player/player)
(s/def ::looser ::player/playable-cell)
(s/def ::conversion  (s/keys :req-un [::point ::taken ::winner ::looser]))
(s/def ::available-moves
  (s/map-of ::player/player
    (s/map-of ::board/coord
      (s/every ::conversion))))


;; -----------------------------------------
;; Private
;; -----------------------------------------

(defn- ^boolean is-not-convertible?
  [cell]
  (or (nil? cell) (= cell :empty) (= cell :wall)))

(defn- walk-dir
  [[x y] [dx dy]]
  [(+ x dx) (+ y dy)])

(defn- available-cells-by-dir
  "Indicates the convertible cells when clicking at [x y]"  ;; TODO - Refactor & Test
  [board initial-coord direction]
  (loop [coord (walk-dir initial-coord direction)
         looser nil
         taken []]
    (let [cell (board/get-cell-at board coord)]
      (cond
        (is-not-convertible? cell) nil                        ;; No move: reached end and only 1 type of cell
        (and looser (not= looser cell)) {:winner cell         ;; Who wins the cells
                                         :looser looser       ;; Who looses the cells
                                         :point initial-coord ;; The move performed
                                         :taken taken}        ;; The cells taken
        :else (recur
                (walk-dir coord direction)
                cell (conj taken coord)))
      )))

(defn- available-conversions-at
  "Provides the list of moves that can be done from a cell"
  [board point]
  (eduction
    (keep #(available-cells-by-dir board point %))
    cst/directions))


;; -----------------------------------------
;; Public API
;; -----------------------------------------

;; TODO - Refactor to get two things out
;; 1. A new board
;; 2. A list of the converted cells
;; All grouped by point and by player

(defn empty-cell-conversion
  "Create a move to take an empty cell"
  [player point]
  {:point point
   :winner player
   :looser :empty
   :taken [point]})

(s/fdef all-available-moves
  :args (s/tuple ::board/board)
  :ret ::available-moves)

(defn all-available-moves
  "Return all move available on the board, grouped by player and by move"
  [board]
  (dissoc
    (transduce
      (mapcat #(available-conversions-at board %))
      #(update-in %1 [(:winner %2) (:point %2)] conj %2)
      {}
      (board/empty-cells board))
    nil))                                                   ;; TODO - find why we have nil

(s/fdef apply-conversion
  :args (s/tuple ::board/board ::conversion)
  :ret ::board/board)

(defn apply-conversion
  "Apply a move onto the board, yielding a new board"
  [board move]
  (let [updates (map vector (:taken move) (repeat (:winner move)))]
    (board/update-cells board updates)))
