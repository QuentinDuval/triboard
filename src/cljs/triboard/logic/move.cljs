(ns triboard.logic.move
  (:require
    [cljs.spec :as s :include-macros true]
    [triboard.logic.board :as board]
    [triboard.logic.constants :as cst]
    [triboard.logic.player :as player]
    ))


;; -----------------------------------------
;; Public Types
;; -----------------------------------------

(s/def ::point ::board/coord)
(s/def ::taken (s/coll-of ::board/coord))
(s/def ::winner ::player/player)
(s/def ::looser ::player/playable-cell)
(s/def ::conversion (s/keys :req-un [::point ::taken ::winner ::looser]))
(s/def ::conversions (s/every ::conversion))


(s/def ::available-moves
  (s/map-of ::player/player
    (s/map-of ::board/coord
      #_(s/every ::conversion)                              ;; TODO map with conversion in it, but also next board
      any?
      )))


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

(defn- apply-conversions
  [board moves]
  (reduce apply-conversion board moves))

(defn- to-game-transition
  [board moves]
  {:transition moves
   :scores 0                         ;; TODO - Put a score diff (more by player, less by player)
   :board (delay (apply-conversions board moves))})

(defn- add-empty-cell-conversion
  [moves]
  (if-let [move (first moves)]
    (conj moves (empty-cell-conversion (:winner move) (:point move)))
    moves))

;; -----------------------------------------
;; Public API
;; -----------------------------------------

#_(s/fdef all-available-moves
    :args (s/tuple ::board/board)
    :ret ::available-moves)


;; (require '[clojure.test.check.generators :as gen])
;; (def b (first (gen/sample (s/gen ::board/board) 1)))

(defn group-by-reducer
  [keys]
  #(update-in %1 ((apply juxt keys) %2) conj %2))

(defn map-values
  "Apply a function to the values of a key-value collection"
  [xf coll]
  (into {}
    (map (fn [[k v]] [k (xf v)]))
    coll))

(defn available-transitions
  [board]
  (map-values
    (partial map-values
      #(to-game-transition board (add-empty-cell-conversion %)))

    (transduce
      (mapcat #(available-conversions-at board %))          ;; TODO - Cut that in two? (moves vs transitions)
      (group-by-reducer [:winner :point])                   ;; TODO - it creates nil...
      (board/empty-cells board))
    ))

