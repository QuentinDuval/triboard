(ns triboard.ai.scoring
  (:require
    [triboard.logic.constants :as cst]
    [triboard.logic.board :as board]
    ))


;; ----------------------------------------------------------------------------
;; Private API
;; ----------------------------------------------------------------------------

(defn- neighbors-coordinates
  [[x y]]
  (for [[dx dy] cst/directions] [(+ x dx) (+ y dy)]))

(defn- neighbouring-walls
  [board point]
  (let [is-wall? #(= :wall (board/get-cell-at board %))]
    (eduction (filter is-wall?) (neighbors-coordinates point))))

(defn- cell-weight
  [board point]
  (let [wall-nb (count (neighbouring-walls board point))]
    (+ 1 (* wall-nb wall-nb 0.25))))                        ;; TODO - Cell weight does not work...

(defn- all-weights
  [board]
  (into {}
    (for [point cst/all-positions]
      [point (cell-weight board point)]
      )))


;; ----------------------------------------------------------------------------
;; Public API
;; ----------------------------------------------------------------------------

(defn get-weighting
  "Return a weighting strategy for all the cells of the board"
  [board]
  (let [weights (all-weights board)]
    (fn [coord] (get weights coord 0))))
