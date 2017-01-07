(ns triboard.logic.move
  (:require
    [cljs.spec :as s :include-macros true]
    [cljs.spec.impl.gen :as gen]
    [triboard.logic.board :as board]
    [triboard.logic.constants :as cst]
    ))


;; -----------------------------------------
;; Public Types
;; -----------------------------------------

(s/def ::point ::board/coord)
(s/def ::taken (s/coll-of ::board/coord))
(s/def ::winner cst/player?)
(s/def ::looser (conj cst/player? :empty))
(s/def ::conversion  (s/keys :req-un [::point ::taken ::winner ::looser]))

(defn conversion? [c] (s/valid? ::conversion c))
(defn conversions? [coll] (every? conversion? coll))


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

(s/fdef all-available-moves
  :args (s/tuple ::board/board)
  :ret map?)                                                ;; TODO - Improve

(s/fdef apply-conversion
  :args (s/tuple ::board/board ::conversion)
  :ret ::board/board)

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
  (transduce
    (mapcat #(available-conversions-at board %))
    #(update-in %1 [(:winner %2) (:point %2)] conj %2)
    {}
    (board/empty-cells board)
    ))

(defn apply-conversion
  "Apply a move onto the board, yielding a new board"
  [board move]
  (reduce
    #(assoc-in %1 %2 (:winner move))
    board
    (:taken move)))
