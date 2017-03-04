(ns triboard.logic.move
  (:require
    [cljs.spec :as s :include-macros true]
    [triboard.logic.board :as board]
    [triboard.logic.constants :as cst]
    [triboard.logic.player :as player]
    [triboard.utils.algo :as algo]))


;; -----------------------------------------
;; Public Types
;; -----------------------------------------

(s/def ::point ::board/coord)
(s/def ::taken (s/coll-of ::board/coord))
(s/def ::winner ::player/player)
(s/def ::looser ::player/playable-cell)
(s/def ::conversion (s/keys :req-un [::point ::taken ::winner ::looser]))
(s/def ::transition (s/every ::conversion))

(s/def ::transitions
  (s/map-of ::player/player
    (s/map-of ::board/coord ::transition)))


;; -----------------------------------------
;; Private
;; -----------------------------------------

(defn- ^boolean is-not-convertible?
  [cell]
  (or (= cell :empty) (= cell :wall)))

(defn- aboard-cell-at
  [board x y]
  (if (and (< -1 x board/width) (< -1 y board/height))
    (aget board x y)
    :wall))

(defn- available-cells-by-dir
  "Indicates the convertible cells when clicking at [x y]"
  [board [x-init y-init :as init-coord] [dx dy]]
  (loop [x (+ x-init dx)
         y (+ y-init dy)
         looser nil
         taken []]
    (let [cell (aboard-cell-at board x y)]
      (cond
        (is-not-convertible? cell) nil                      ;; No move: reached end and only 1 type of cell
        (and looser (not= looser cell)) {:winner cell       ;; Who wins the cells
                                         :looser looser     ;; Who looses the cells
                                         :point init-coord  ;; The move performed
                                         :taken taken}      ;; The cells taken
        :else (recur
                (+ x dx)
                (+ y dy)
                cell
                (conj taken [x y])))
      )))

(defn- available-conversions-at
  "Provides the list of moves that can be done from a cell"
  [board point]
  (eduction
    (keep #(available-cells-by-dir board point %))
    cst/directions))

(defn- empty-cell-conversion
  "Create a move to take an empty cell"
  [player point]
  {:point point
   :winner player
   :looser :empty
   :taken [point]})

(defn- apply-conversion
  "Apply a move onto the board, yielding a new board"
  [board {:keys [winner taken]}]
  (reduce #(board/convert-cell %1 %2 winner) board taken))

(defn conversions->transition
  [conversions]
  (if-let [{:keys [winner point]} (first conversions)]
    (conj conversions (empty-cell-conversion winner point))
    conversions))

(defn- map-transition-tree
  [xf game-tree]
  (algo/map-values #(algo/map-values xf %) game-tree))

;; -----------------------------------------
;; Public API
;; -----------------------------------------

(s/fdef available-transitions
  :args (s/tuple ::board/board)
  :ret ::transitions)

(defn all-transitions
  [board]
  (let [aboard (board/board->array board)]
    (map-transition-tree
      conversions->transition
      (transduce
        (mapcat #(available-conversions-at aboard %))
        (algo/group-by-reducer :winner :point)
        (board/empty-cells board)))))

(defn apply-transition
  [board transition]
  (reduce apply-conversion board transition))

;; -----------------------------------------
;; TESTS
;; -----------------------------------------

(defn benchmark
  []
  (let [b (board/new-board)]
    (time (dotimes [i 100]
            (all-transitions b)
            ))
    (time (dotimes [i 100]
            (doall (map #(apply-transition b %) (all-transitions b)))
            ))
    ))
