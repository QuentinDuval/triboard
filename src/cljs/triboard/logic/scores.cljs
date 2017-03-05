(ns triboard.logic.scores
  (:require
    [cljs.spec :as s :include-macros true]
    [triboard.logic.constants :as cst]
    [triboard.logic.transition :as transition]
    [triboard.logic.player :as player]
    ))


;; ----------------------------------------------------------------------------
;; Private API
;; ----------------------------------------------------------------------------

(defn- update-from-jump
  [scores jump]
  (let [delta (count (:taken jump))]
    (-> scores
      (update (:winner jump) + delta)
      (update (:looser jump) - delta)
      (dissoc :empty))))


;; ----------------------------------------------------------------------------
;; Public API
;; ----------------------------------------------------------------------------

(s/def ::weight number?)
(s/def ::scores (s/map-of ::player/player ::weight))

(s/fdef update-scores
  :args (s/cat :scores ::scores :transition ::transition/transition)
  :ret ::scores)

(def null-scores
  {:blue 0 :red 0 :green 0})

(def initial-scores
  {:blue cst/init-block-count
   :red cst/init-block-count
   :green cst/init-block-count})

(defn update-scores
  "Update the scoring based on the provided transition"
  [scores transition]
  (reduce update-from-jump scores transition))
