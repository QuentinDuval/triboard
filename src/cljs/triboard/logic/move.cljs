(ns triboard.logic.move
  (:require
    [cljs.spec :as s :include-macros true]
    [triboard.logic.board :as board]
    [triboard.logic.constants :as cst]
    [triboard.logic.player :as player]
    [triboard.utils :as utils]))


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
  (or (nil? cell) (= cell :empty) (= cell :wall)))

(defn- walk-dir
  [[dx dy] [x y]]
  [(+ x dx) (+ y dy)])

(defn- available-cells-by-dir
  "Indicates the convertible cells when clicking at [x y]"  ;; TODO - Refactor & Test
  [board init-coord next-coord]
  (loop [coord (next-coord init-coord)
         looser nil
         taken []]
    (let [cell (board/get-cell-at board coord)]
      (cond
        (is-not-convertible? cell) nil                      ;; No move: reached end and only 1 type of cell
        (and looser (not= looser cell)) {:winner cell       ;; Who wins the cells
                                         :looser looser     ;; Who looses the cells
                                         :point init-coord  ;; The move performed
                                         :taken taken}      ;; The cells taken
        :else (recur
                (next-coord coord)
                cell
                (conj taken coord)))
      )))

(defn- available-conversions-at
  "Provides the list of moves that can be done from a cell"
  [board point]
  (eduction
    (keep #(available-cells-by-dir board point %))
    (map #(partial walk-dir %) cst/directions)))

(defn- empty-cell-conversion
  "Create a move to take an empty cell"
  [player point]
  {:point point
   :winner player
   :looser :empty
   :taken [point]})

(defn- apply-conversion
  "Apply a move onto the board, yielding a new board"
  [board move]
  (let [updates (map vector (:taken move) (repeat (:winner move)))]
    (board/update-cells board updates)))

(defn conversions->transition
  [conversions]
  (if-let [{:keys [winner point]} (first conversions)]
    (conj conversions (empty-cell-conversion winner point))
    conversions))

(defn- map-game-tree
  [xf game-tree]
  (utils/map-values (partial utils/map-values xf) game-tree))

;; -----------------------------------------
;; Public API
;; -----------------------------------------

(s/fdef available-transitions
  :args (s/tuple ::board/board)
  :ret ::transitions)

(defn all-transitions
  [board]
  (map-game-tree
    conversions->transition
    (transduce
      (mapcat #(available-conversions-at board %))
      (utils/group-by-reducer :winner :point)
      (board/empty-cells board))))

(defn apply-transition
  [board transition]
  (reduce apply-conversion board transition))
