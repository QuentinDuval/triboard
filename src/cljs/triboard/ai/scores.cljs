(ns triboard.ai.scores
  (:require
    [cljs.spec :as s :include-macros true]
    [cljs.spec.impl.gen :as gen]
    [triboard.logic.constants :as cst]
    [triboard.logic.board :as board]
    [triboard.logic.move :as move]
    ))


;; -----------------------------------------
;; Private
;; -----------------------------------------

(defn neighbors-coordinates
  [[x y]]
  (map (fn [[dx dy]] [(+ x dx) (+ y dy)]) cst/directions))

(defn- neighbouring-walls
  [board point]
  (eduction
    (filter #(= :wall (board/get-cell-at board % :wall)))
    (neighbors-coordinates point)))

(defn- get-cell-weight
  {:pre [(coord? point)]}
  [board point]
  (let [wall-nb (count (neighbouring-walls board point))]
    (+ 1 (* wall-nb wall-nb 0.25))))

(defn- sum-cell-weight
  [weights-by-cell cells]
  (transduce (map weights-by-cell) + cells))


;; -----------------------------------------
;; Public Types
;; -----------------------------------------

;; TODO - How to generate maps with specific keys like coordinates? Meta programming?

(s/def ::score-diff (s/map-of ::cst/player number?))        ;; TODO - Imcomplete player set
(s/def ::weights-by-cell (s/map-of ::board/coord number?))  ;; TODO - Imcomplete map

(s/fdef get-weights-by-cell
  :args (s/tuple ::board/board)
  :ret ::weights-by-cell)

(s/fdef update-score-diff
  :args (s/tuple ::weights-by-cell ::score-diff ::move/conversion)
  :ret ::score-diff)


;; -----------------------------------------
;; Public API
;; -----------------------------------------

(defn get-weights-by-cell
  "For each cell of the board, compute a weighting factor to determine its importance"
  [board]
  (into {}
    (map (fn [point] [point (get-cell-weight board point)]))
    cst/all-positions))

(def null-score-diff {:blue 0 :red 0 :green 0})

(defn update-score-diff
  [weights-by-cell delta conversion]
  (let [diff (sum-cell-weight weights-by-cell (:taken conversion))]
    (-> delta
      (update (:looser conversion) - diff)
      (update (:winner conversion) + diff)
      )))
