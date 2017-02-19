(ns triboard.logic.scores
  (:require
    [cljs.spec :as s :include-macros true]
    [triboard.logic.board :as board]
    [triboard.logic.constants :as cst]
    [triboard.logic.move :as move]
    [triboard.logic.player :as player]
    ))


;; ----------------------------------------------------------------------------
;; Private API
;; ----------------------------------------------------------------------------

(defn- transfer-delta
  [scores conversion delta]
  (-> scores
    (update (:winner conversion) + delta)
    (update (:looser conversion) - delta)
    (dissoc :empty)))


;; ----------------------------------------------------------------------------
;; Public API
;; ----------------------------------------------------------------------------

(s/def ::scores (s/map-of ::player/player number?))         ;; TODO - parameterized specs?
(s/fdef weighting :args (s/tuple ::board/coord) :ret number?)

(s/fdef update-scores
  :args (s/tuple ::scores ::move/conversion) :ret ::scores)

(s/fdef update-scores-with
  :args (s/tuple ::scores ::move/conversion ::weighting) :ret ::scores)

(def null-scores
  {:blue 0 :red 0 :green 0})

(def initial-scores
  {:blue cst/init-block-count
   :red cst/init-block-count
   :green cst/init-block-count})

(defn update-scores-with
  "Update the scoring based on the provided move, and a weighting policy"
  [scores conversion weight-of]
  (let [delta (transduce (map weight-of) + (:taken conversion))]
    (transfer-delta scores conversion delta)))

(defn update-scores
  "Update the scoring based on the provided move"
  [scores conversion]
  (let [delta (count (:taken conversion))]
    (transfer-delta scores conversion delta)))
