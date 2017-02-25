(ns triboard.ai.ai
  (:require
    [cljs.spec :as s :include-macros true]
    [triboard.ai.scoring :as scoring]
    [triboard.logic.game :as game]
    [triboard.logic.player :as player]
    [triboard.logic.scores :as scores]
    [triboard.logic.turn :as turn]
    ))


;; -----------------------------------------
;; Private
;; -----------------------------------------

(defn- make-ai
  [board player]
  {:player player
   :other-players (remove #{player} player/all)
   :cell-weights (scoring/get-weighting board)
   })

(defn- score-move
  "Compute the strength of a move, based on the converted cells"
  [{:keys [player cell-weights]} transition]
  (reduce                                                   ;; TODO - Factor in move
    #(scores/update-scores-with %1 %2 cell-weights)
    scores/null-scores
    transition))

;; -----------------------------------------
;; Attempts game - tree (later minimax decoupled)
;; -----------------------------------------

(defn build-game-tree
  [game depth]
  (if (= 0 depth)
    game
    (let [transitions (turn/player-transitions (game/current-turn game))
          next-games (map #(game/play-move game (first %)) transitions)]
      (map vector
        transitions
        (map #(build-game-tree % (dec depth)) next-games)))
    ))

(defn test-build-game-tree
  []
  (let [init-game (game/new-game)]
    (build-game-tree init-game 1)))

;; -----------------------------------------
;; Attempts minimax
;; -----------------------------------------

;; TODO - How to add the custom scoring? How to keep the game? (use keys)
;; TODO - Use mapmin from http://worrydream.com/refs/Hughes-WhyFunctionalProgrammingMatters.pdf
;; TODO - Sort by scores as well


(defn contains-lower-value?
  [coll pot]
  (cond
    (empty? coll) false
    (<= (first coll) pot) true
    :else (recur (rest coll) pot)
    ))

(defn map-min-impl
  [max-min-val coll]
  (cond
    (empty? coll) max-min-val
    (contains-lower-value? (first coll) max-min-val) max-min-val ;; Will depend on player
    :else (recur
            (max max-min-val (apply min (first coll)))      ;; Will depend on player
            (rest coll))
    ))

(defn map-min
  [coll]
  (map-min-impl (min (first coll)) (rest coll)))

;; -----------------------------------------

;; TODO - use the heuristic for the scoring (worse move after - look-ahead)
(defn leaf-score
  [ai turn]
  (get (:scores turn) (:player ai))
  ;; (* -1 (get-in turn [:scores :blue])) ;; TODO - Make AI aligned against blue
  )

(defn tree-score
  [ai turn depth]
  (if (= 0 depth)
    (leaf-score ai turn)
    (let [transitions (turn/player-transitions turn)
          min-max (if (= (:player ai) (:player turn)) max min)]
      (apply min-max
        (map
          (fn [[coord transition]]
            (tree-score ai
              (turn/apply-transition turn transition)
              (dec depth)))
          transitions)))
    ))

(defn test-score-tree
  []
  (let [init-game (game/new-game)
        turn (game/current-turn init-game)
        ai (make-ai (:board turn) (:player turn))]
    (tree-score ai (game/current-turn init-game) 2)))

(defn benchmark
  []
  (time (dotimes [i 10]
          (time (test-score-tree))
          )))

;; -----------------------------------------

(defn- worst-possible-score-from
  [{:keys [player other-players] :as ai} moves]
  (transduce
    (comp
      (mapcat #(get moves %))
      (map #(score-move ai (second %)))
      (map #(get % player)))
    min
    other-players))

(defn- move-best-outcome
  [ai game [coord transition]]
  (let [new-game (game/play-move game coord)
        move-diff (get (score-move ai transition) (:player ai))
        new-moves (:transitions (game/current-turn new-game))
        next-diff (worst-possible-score-from ai new-moves)]
    {:scoring (+ move-diff next-diff)
     :game new-game}))

(defn- max-by [key-fn coll] (apply max-key key-fn coll))


;; -----------------------------------------
;; Public API
;; -----------------------------------------

(s/fdef play-best-move
  :args (s/tuple ::game/game)
  :ret ::game/game)

(defn- play-best-move
  [game]
  (let [turn (game/current-turn game)
        ai (make-ai (:board turn) (:player turn))]
    (:game
      (max-by :scoring
        (map
          (fn [[coord _]]
            (let [new-game (game/play-move game coord)
                  new-turn (game/current-turn new-game)]
              {:game new-game
               :scoring (tree-score ai new-turn 1)}))
          (turn/player-transitions turn)))
      )))

#_(defn- play-best-move
    "[SIMPLISTIC] Play the best move for a player based on:
     * The immediate gain
     * The worse immediate lost afterwards"
    [game]
    (let [turn (game/current-turn game)
          ai (make-ai (:board turn) (:player turn))]
      (:game
        (max-by :scoring
          (map
            #(move-best-outcome ai game %)
            (turn/player-transitions turn)))
        )))
