(ns triboard.ai.scores
  (:require
    [triboard.logic.constants :as cst]
    [triboard.logic.board :as board]
    [triboard.logic.move :as move]
    [triboard.utils :as utils]
    ))


;; -----------------------------------------
;; Private
;; -----------------------------------------

(defn- neighbouring-walls
  [board point]
  (eduction
    (filter #(= :wall (board/get-cell-at board % :wall)))
    (utils/coord-neighbors point)))

(defn- get-cell-weight
  {:pre [(coord? point)]}
  [board point]
  (let [wall-nb (count (neighbouring-walls board point))]
    (+ 1 (* wall-nb wall-nb 0.25))))

(defn- sum-cell-weight
  [weights-by-cell cells]
  (transduce (map weights-by-cell) + cells))


;; -----------------------------------------
;; Public API
;; -----------------------------------------

(defn get-weights-by-cell
  "For each cell of the board, compute a weighting factor to determine its importance"
  [board]
  (into {}
    (map (fn [point] [point (get-cell-weight board point)]))
    cst/all-positions))

(def null-score-diff
  {:blue 0 :red 0 :green 0})

(defn update-score-diff
  [weights-by-cell delta conversion]
  {:pre [(move/conversion? conversion)]}
  (let [diff (sum-cell-weight weights-by-cell (:taken conversion))]
    (-> delta
      (update (:looser conversion) - diff)
      (update (:winner conversion) + diff)
      )))

(defn min-delta-for
  "Compute the minimal diff for a player among a list of delta"
  [player deltas]
  (transduce (map #(get % player)) min deltas))
