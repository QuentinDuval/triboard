(ns triboard.ai.ai
  (:require
    [cljs.spec :as s :include-macros true]
    [triboard.ai.scoring :as scoring]
    [triboard.logic.game :as game]
    [triboard.logic.move :as move]
    [triboard.logic.player :as player]
    [triboard.logic.scores :as scores]
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
  [{:keys [player cell-weights]} [point conversions]]
  (reduce                                                   ;; TODO - Factor in move
    #(scores/update-scores-with %1 %2 cell-weights)
    scores/null-scores
    conversions))

(defn- worst-possible-score-from
  [{:keys [player other-players] :as ai} moves]
  (transduce
    (comp
      (mapcat #(get moves %))
      (map #(score-move ai %))
      (map #(get % player)))
    min
    other-players))

(defn- move-best-outcome
  [ai game [coord converted :as move]]
  (let [new-game (game/play-move game coord)
        move-diff (get (score-move ai move) (:player ai))
        new-moves (:moves (game/current-state new-game))
        next-diff (worst-possible-score-from ai new-moves)]
    (+ move-diff next-diff)))

(defn- max-by
  [key-fn coll]
  (apply max-key (memoize key-fn) coll))


;; -----------------------------------------
;; Public API
;; -----------------------------------------

(s/fdef play-best-move
  :args (s/tuple ::game/game ::player/player)
  :ret ::game/game)

(defn- best-move                                            ;; TODO - Compute games directly...
  [game player]
  (let [game-state (game/current-state game)
        ai (make-ai (:board game-state) player)]
    (first
      (max-by #(move-best-outcome ai game %)
        (get-in game-state [:moves player]))
      )))

(defn play-best-move
  "[SIMPLISTIC] Play the best move for a player based on:
   * The immediate gain
   * The worse immediate lost afterwards"
  [game player]
  (game/play-move game (best-move game player)))
