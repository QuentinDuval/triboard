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

(defn- compute-cell-strength
  "Compute a cell strength based on the number of walls it has"
  {:pre [(coord? point)]}
  [board point]
  (let [wall-nb (count (neighbouring-walls board point))]
    (+ 1 (* wall-nb wall-nb 0.25))))


;; -----------------------------------------
;; Public API
;; -----------------------------------------

(def null-delta
  {:blue 0 :red 0 :green 0})

(defn update-delta
  [get-cell-strength delta conversion]
  {:pre [(move/conversion? conversion)]}
  (let [diff (transduce (map get-cell-strength) + (:taken conversion))]
    (-> delta
      (update (:looser delta) - diff)
      (update (:winner delta) + diff)
      )))

(defn compute-cells-strength
  "Adds to a given turn the strength of each of its cells"
  [board]
  (into {}
    (map (fn [point] [point (compute-cell-strength board point)]))
    cst/all-positions))
