(ns triboard.logic.scores
  (:require
    [cljs.spec :as s :include-macros true]
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

(s/def ::pos-int (s/and integer? #(<= 0 %)))
(s/def ::scores (s/map-of ::player/player ::pos-int))

(s/fdef update-scores
  :args (s/tuple ::scores ::move/conversion)
  :ret (s/map-of ::player/player integer?))                    ;; TODO - Hard to ensure positive number

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
