(ns triboard.logic.move
  (:require
    [triboard.logic.board :as board]
    [triboard.logic.constants :as cst]
    ))


;; -----------------------------------------
;; Public Types
;; -----------------------------------------

(defn conversion?
  "A move is the result of player a cell at a given coordinate"
  [m]
  (and
    (board/coord? (:point m))                                ;; TODO - rename => point
    (every? board/coord? (:taken m))
    (cst/player? (:winner m))
    (cst/cell? (:looser m))))

(defn conversions?
  [moves]
  (every? conversion? moves))


;; -----------------------------------------
;; Private
;; -----------------------------------------

(defn- ^boolean is-not-convertible?
  [cell]
  (or (nil? cell) (= cell :empty) (= cell :wall)))

(defn- available-cells-by-dir
  "Indicates the convertible cells when clicking at [x y], and returns:
	{:point [7 5]
   :winner :blue,
   :looser :red,
   :taken [[8 5] [9 5]]}"
  [board [xi yi :as coord] [dx dy]]
  (loop [x (+ xi dx)
         y (+ yi dy)
         looser nil
         taken []]
    (let [cell (board/get-cell-at board [x y])]
      (cond
        (is-not-convertible? cell) nil                      ;; No move: reached end and only 1 type of cell
        (and looser (not= looser cell)) {:winner cell       ;; Who wins the cells
                                         :looser looser     ;; Who looses the cells
                                         :point coord       ;; The move performed
                                         :taken taken}      ;; The cells taken
        :else (recur
                (+ x dx) (+ y dy)
                cell (conj taken [x y])))
      )))

(defn- available-conversions-at
  "Provides the list of moves that can be done from a cell"
  [board point]
  {:pre [(board/board? board) (board/coord? point)]
   :post [(conversions? %)]}
  (eduction
    (keep #(available-cells-by-dir board point %))
    cst/directions))


;; -----------------------------------------
;; Public API
;; -----------------------------------------

(defn empty-cell-conversion
  "Create a move to take an empty cell"
  [player point]
  {:point point
   :winner player
   :looser :empty
   :taken [point]})

(defn all-available-moves
  "Return all move available on the board, grouped by player and by move
   Example:
    {:blue
      {[0 5]
       ({:point [0 5],
         :winner :blue,
         :looser :red,
         :taken [[0 4]]})}}"
  [board]
  {:pre [(board/board? board)]}
  (transduce
    (mapcat #(available-conversions-at board %))
    #(update-in %1 [(:winner %2) (:point %2)] conj %2)
    {}
    (board/empty-cells board)
    ))

(defn apply-conversion
  "Apply a move onto the board, yielding a new board"
  [board move]
  {:pre [(board/board? board) (conversion? move)]
   :post [(board/board? board)]}
  (reduce
    #(assoc-in %1 %2 (:winner move))
    board
    (:taken move)))
