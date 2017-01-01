(ns triboard.logic.move
  (:require
    [triboard.logic.board :as board]
    [triboard.logic.constants :as cst]
    ))


;; -----------------------------------------
;; Public Types
;; -----------------------------------------

(defn move?
  "A move is the result of player a cell at a given coordinate"
  [m]
  (and
    (board/coord? (:move m))                                ;; TODO - rename
    (every? board/coord? (:taken m))
    (cst/player? (:winner m))
    (cst/cell? (:looser m))))

(defn moves? [moves] (every? move? moves))


;; -----------------------------------------
;; Private
;; -----------------------------------------

(defn- ^boolean is-not-convertible?
  [cell]
  (or (nil? cell) (= cell :empty) (= cell :wall)))

(defn- available-cells-by-dir
  "Indicates the convertible cells when clicking at [x y], and returns:
	{:move [7 5]
   :winner :blue,
   :looser :red,
   :taken [[8 5] [9 5]]}"
  [board [xi yi :as coord] [dx dy]]
  {:pre [(board/board? board) (board/coord? coord)]
   :post [(or (nil? %) (move? %))]}
  (loop [x (+ xi dx)
         y (+ yi dy)
         looser nil
         taken []]
    (let [cell (board/get-cell-at board [x y])]
      (cond
        (is-not-convertible? cell) nil ;; No move: reached end and only 1 type of cell
        (and looser (not= looser cell)) {:winner cell   ;; Who wins the cells
                                         :looser looser ;; Who looses the cells
                                         :move coord    ;; The move performed
                                         :taken taken}  ;; The cells taken
        :else (recur
                (+ x dx) (+ y dy)
                cell (conj taken [x y])))
      )))


;; -----------------------------------------
;; Public API
;; -----------------------------------------

(defn available-moves-at
  "Provides the list of moves that can be done from a cell"
  [board point]
  {:pre [(board/board? board) (board/coord? point)]
   :post [(moves? %)]}
  (eduction
    (keep #(available-cells-by-dir board point %))
    cst/directions))
