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
(s/def ::coord->transition (s/map-of ::destination ::transition))
(s/def ::all-transitions (s/map-of ::player/player ::coord->transition))


;; -----------------------------------------
;; Private
;; -----------------------------------------

(defn- ^boolean block-jump?
  [cell]
  (or (= cell :none) (= cell :wall)))

(defn- ^boolean is-source?
  [looser cell]
  (and looser (not= looser cell)))

(defn- ^boolean? in-board?
  [x y]
  (and (< -1 x board/width) (< -1 y board/height)))

(defn- seek-jump-source-toward
  "Starting from the destination of a jump (an empty cell):
   * Search for valid source for the jump
   * Collect the jumped cells along the way"
  [board [x-init y-init :as destination] [dx dy]]
  (loop [x (+ x-init dx)
         y (+ y-init dy)
         looser nil
         taken []]
    (if (in-board? x y)
      (let [owner (aget board x y)]
        (cond
          (block-jump? owner) nil
          (is-source? looser owner) {:winner owner
                                     :looser looser
                                     :destination destination
                                     :taken taken}
          :else (recur
                  (+ x dx)
                  (+ y dy)
                  owner
                  (conj taken [x y])))
        ))))

(defn- available-jumps-at
  "Provides the list of moves that can be done from a cell"
  [board [x y :as destination]]
  (if (= :none (aget board x y))
    (eduction
      (keep #(seek-jump-source-toward board destination %))
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

(defn- map-transition-tree
  [xf game-tree]
  (algo/map-values #(algo/map-values xf %) game-tree))

;; -----------------------------------------
;; Public API
;; -----------------------------------------

(s/fdef all-transitions
  :args (s/cat :board ::board/board)
  :ret ::all-transitions)

(defn all-transitions
  [board]
  (let [aboard (board/board->array board)]
    (map-transition-tree
      add-destination
      (transduce
        (mapcat #(available-jumps-at aboard %))
        (algo/group-by-reducer :winner :destination)
        board/coordinates))))

(defn apply-transition
  [board transition]
  (reduce apply-jump board transition))

;; -----------------------------------------
;; TESTS
;; -----------------------------------------

(defonce benchmark-board (board/new-board))

(defn benchmark
  []
  (let [b benchmark-board
        t (all-transitions b)]
    (time (dotimes [i 100] (all-transitions b)))
    ;; TODO - 122700 turns in loop for 100 iterations (6.5 ms for a transduce) => need better algo
    ;; The add-destination removal makes us gain around 50 ms
    ;; Getting rid of the systematic conj does not win anything
    ;; One dimentional array could gain us only 2-3 ms
    (time (dotimes [i 100]
            (doall
              (map
                #(apply-transition b (second %))
                (mapcat #(get t %) [:blue :red :green])
                ))
            ))
    ))

;; -----------------------------------------
;; Random experiments
;; -----------------------------------------

;; TODO - Completely change the algorithm (solution 1):
;; - We only need one pass... for each dimention
;; - partition-by to get the zones of same color
;; - partition size 3 step 1 to get the surroundings
;; - filter on "one side exactly is :none" and you get the jumps

;; TODO - Completely change the algorithm (solution 2):
;; Search from the cells owned by a player... allows more incremental mode


#_(let [b (board/new-board)]
    (time
      (dotimes [i 100]
        (doall (partition-by
                 second
                 (map
                   (fn [c] [c (board/get-cell-at b c)])
                   board/coordinates))))))


#_(def columns
    (mapv
      (fn [x] (mapv vector (repeat x) (range board/height)))
      (range board/width)))

#_(def rows
    (apply mapv vector columns))

#_(defn jumpables
    [board [x-init y-init :as init-coord] [dx dy :as direction]]
    (loop [prev (board/get-cell-at board init-coord)
           prev-pos init-coord
           x (+ x-init dx)
           y (+ y-init dy)
           jumps []]
      (if-not (in-board? x y)
        jumps
        (let [curr (board/get-cell-at board [x y])]
          (if (= curr prev)
            (recur prev prev-pos (+ x dx) (+ y dy) jumps)
            (recur curr [x y] (+ x dx) (+ y dy)
              (conj jumps
                {:start prev-pos
                 :end [(- x dx) (- y dy)]
                 :owner prev}))
            ))
        )))



#_(defn test-new-algo
    []
    (let [b (board/new-board)]
      (keep (fn [[p c n]]
              (cond
                (and (player/player? (:owner p)) (= :none (:owner n)))
                {:winner (:owner p)
                 :looser (:owner c)
                 :destination (:start n)
                 :taken []}

                (and (player/player? (:owner n)) (= :none (:owner p)))
                {:winner (:owner n)
                 :looser (:owner c)
                 :destination (:end p)
                 :taken []}

                :else nil)
              )
        (partition 3 1 (jumpables b [0 0] [0 1])))
      ))

