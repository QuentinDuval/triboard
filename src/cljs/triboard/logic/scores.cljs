(ns triboard.logic.scores
  (:require
    [cljs.spec :as s :include-macros true]
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
      )))


;; ----------------------------------------------------------------------------
;; Public API
;; ----------------------------------------------------------------------------

(s/def ::weight number?)
(s/def ::scores (s/map-of ::player/player ::weight))

(s/fdef update-scores
  :args (s/cat :scores ::scores :transition ::transition/transition)
  :ret ::scores)

(defn initial-scores
  [score-val]
  {:blue score-val
   :red score-val
   :green score-val})

(defn update-scores
  "Update the scoring based on the provided transition"
  [scores transition]
  (let [{:keys [winner]} (first transition)]
    (->
      (reduce update-from-jump scores transition)
      (update winner inc))))
