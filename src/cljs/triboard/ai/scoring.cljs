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

(defn- wall-count->weight
  [wall-count]
  (+ 1 (* wall-count wall-count 0.25)))

(defn- pair-cell-with-weight
  [board point]
  (let [wall-count (count (neighbouring-walls board point))]
    [point (wall-count->weight wall-count)]))


;; ----------------------------------------------------------------------------
;; Public API
;; ----------------------------------------------------------------------------

(defn get-weighting
  "Return a weighting strategy for all the cells of the board"
  [board]
  (let [weights (into {} (map #(pair-cell-with-weight board %)) board)]
    (fn [coord] (get weights coord 0))))
