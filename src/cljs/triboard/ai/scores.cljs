(ns triboard.ai.scores
  (:require
    [cljs.spec :as s :include-macros true]
    [triboard.logic.constants :as cst]
    [triboard.logic.board :as board]
    [triboard.logic.move :as move]
    [triboard.logic.scores :as scores]
    ))


;; -----------------------------------------
;; Private
;; -----------------------------------------

(defn- neighbors-coordinates
  [[x y]]
  (map (fn [[dx dy]] [(+ x dx) (+ y dy)]) cst/directions))

(defn- neighbouring-walls
  [board point]
  (let [is-wall? #(= :wall (board/get-cell-at board %))]
    (eduction (filter is-wall?) (neighbors-coordinates point))))

(defn- get-cell-weight
  [board point]
  (let [wall-nb (count (neighbouring-walls board point))]
    (+ 1 (* wall-nb wall-nb 0.25))))


;; -----------------------------------------
;; Public Types
;; -----------------------------------------

(s/def ::weights-by-cell (s/map-of ::board/coord number?))  ;; TODO - Imcomplete map

(s/fdef get-weights-by-cell
  :args (s/tuple ::board/board)
  :ret ::weights-by-cell)

(s/fdef update-score-diff
  :args (s/tuple ::weights-by-cell ::scores/scores ::move/conversion)
  :ret ::scores/scores)


;; -----------------------------------------
;; Public API
;; -----------------------------------------

(defn get-weights-by-cell
  "For each cell of the board, compute a weighting factor to determine its importance"
  [board]
  (into {}
    (map (fn [point] [point (get-cell-weight board point)]))
    cst/all-positions))

(def null-score-diff scores/null-scores)

(defn update-score-diff
  [weights-by-cell scores conversion]
  (scores/update-scores-with scores conversion weights-by-cell))
