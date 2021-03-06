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
(s/def ::looser ::player/convertible-owner)
(s/def ::jump (s/keys :req-un [::destination ::taken ::winner ::looser]))
(s/def ::transition (s/every ::jump))
(s/def ::all-transitions
  (s/map-of ::player/player
    (s/map-of ::destination ::transition)))


;; -----------------------------------------
;; Private
;; -----------------------------------------

(defn- ^boolean block-jump?
  [owner]
  (or (= owner :none) (= owner :wall)))

(defn- ^boolean jump-start?
  [looser owner]
  (and looser (not= looser owner)))

(defn- ^boolean in-board?
  [x y]
  (and (< -1 x board/width) (< -1 y board/height)))

(defn- ^boolean empty-cell?
  [board [x y]]
  (= :none (aget board x y)))

(defn- seek-jump-source-toward
  "Starting from the destination of a jump (a non-owned cell):
   * Search for valid source for the jump
   * Collect the jumped cells along the way"
  [board [x-init y-init :as jump-destination] [dx dy]]
  (loop [x (+ x-init dx)
         y (+ y-init dy)
         looser nil
         taken []]
    (if (in-board? x y)
      (let [owner (aget board x y)]
        (cond
          (block-jump? owner) nil
          (jump-start? looser owner) {:winner owner
                                      :looser looser
                                      :destination jump-destination
                                      :taken taken}
          :else (recur
                  (+ x dx)
                  (+ y dy)
                  owner
                  (conj taken [x y])))
        ))))

(defn- available-jumps-at
  "Provides the list of jumps that can be done at a given `jump-destination`"
  [board jump-destination]
  (if (empty-cell? board jump-destination)
    (eduction
      (keep #(seek-jump-source-toward board jump-destination %))
      cst/directions)))

(defn- add-destination-jump
  "Create a move to take an empty cell"
  [player destination]
  {:destination destination
   :winner player
   :looser :none
   :taken [destination]})

(defn- apply-jump
  "Apply a move onto the board, yielding a new board"
  [board {:keys [winner taken]}]
  (reduce #(board/convert-cell %1 %2 winner) board taken))

(defn add-destination
  [jumps]
  (if-let [{:keys [winner destination]} (first jumps)]
    (conj jumps (add-destination-jump winner destination))
    jumps))

;; -----------------------------------------
;; Public API
;; -----------------------------------------

(s/fdef all-transitions
  :args (s/cat :board ::board/board)
  :ret ::all-transitions)

(defn all-transitions
  [board]
  (let [aboard (board/board->array board)]
    (transduce
      (mapcat #(available-jumps-at aboard %))
      (algo/group-by-reducer :winner :destination)
      board/coordinates)))

(defn apply-transition
  [board transition]
  (reduce apply-jump board (add-destination transition)))
