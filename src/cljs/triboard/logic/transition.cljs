(ns triboard.logic.transition
  (:require
    [cljs.spec :as s :include-macros true]
    [triboard.logic.board :as board]
    [triboard.logic.constants :as cst]
    [triboard.logic.player :as player]
    [triboard.utils.algo :as algo]))


;; -----------------------------------------
;; Public Types
;; -----------------------------------------

(s/def ::destination ::board/coord)
(s/def ::taken (s/coll-of ::board/coord))
(s/def ::winner ::player/player)
(s/def ::looser ::player/playable-cell)
(s/def ::jump (s/keys :req-un [::destination ::taken ::winner ::looser]))
(s/def ::transition (s/every ::jump))

(s/def ::transitions
  (s/map-of ::player/player
    (s/map-of ::destination ::transition)))


;; -----------------------------------------
;; Private
;; -----------------------------------------

(defn- ^boolean block-jump?
  [cell]
  (or (= cell :empty) (= cell :wall)))

(defn- is-source?
  [looser cell]
  (and looser (not= looser cell)))

(defn- aboard-cell-at
  [board x y]
  (if (and (< -1 x board/width) (< -1 y board/height))
    (aget board x y)
    :wall))

(defn- seek-jump-source-toward
  "Starting from the destination of a jump (an empty cell):
   * search for valid source for the jump
   * collect the jumped cells along the way"                ;; TODO - Separate this? (could be faster)
  [board [x-init y-init :as destination] [dx dy]]
  (loop [x (+ x-init dx)
         y (+ y-init dy)
         looser nil
         taken []]
    (let [cell (aboard-cell-at board x y)]
      (cond
        (block-jump? cell) nil
        (is-source? looser cell) {:winner cell
                                  :looser looser
                                  :destination destination
                                  :taken taken}
        :else (recur
                (+ x dx)
                (+ y dy)
                cell
                (conj taken [x y])))
      )))

(defn- available-jumps-at
  "Provides the list of moves that can be done from a cell"
  [board point]
  (eduction
    (keep #(seek-jump-source-toward board point %))
    cst/directions))

(defn- empty-cell-conversion
  "Create a move to take an empty cell"
  [player point]
  {:destination point
   :winner player
   :looser :empty
   :taken [point]})

(defn- apply-jump
  "Apply a move onto the board, yielding a new board"
  [board {:keys [winner taken]}]
  (reduce #(board/convert-cell %1 %2 winner) board taken))

(defn add-destination
  [jumps]
  (if-let [{:keys [winner destination]} (first jumps)]
    (conj jumps (empty-cell-conversion winner destination))
    jumps))

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
      add-destination
      (transduce
        (mapcat #(available-jumps-at aboard %))
        (algo/group-by-reducer :winner :destination)
        (board/empty-cells board)))))

(defn apply-transition
  [board transition]
  (reduce apply-jump board transition))

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
