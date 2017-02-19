(ns triboard.ai.scoring
  (:require
    [cljs.spec :as s :include-macros true]
    [triboard.logic.constants :as cst]
    [triboard.logic.board :as board]
    ))


;; ----------------------------------------------------------------------------
;; Private API
;; ----------------------------------------------------------------------------

(defn- neighbors-coordinates
  [[x y]]
  (map (fn [[dx dy]] [(+ x dx) (+ y dy)]) cst/directions))

(defn- neighbouring-walls
  [board point]
  (let [is-wall? #(= :wall (board/get-cell-at board %))]
    (eduction (filter is-wall?) (neighbors-coordinates point))))

(defn- cell-weight
  [board point]
  (let [wall-nb (count (neighbouring-walls board point))]
    (+ 1 (* wall-nb wall-nb 0.25))))

(defn- all-weights
  [board]
  (into {}
    (map (fn [point] [point (cell-weight board point)]))
    cst/all-positions))


;; ----------------------------------------------------------------------------
;; Public API
;; ----------------------------------------------------------------------------

(defn get-weighting
  "Return a weighting strategy for all the cells of the board"
  [board]
  (let [weights (all-weights board)]
    (fn [coord] (get weights coord 0))))
