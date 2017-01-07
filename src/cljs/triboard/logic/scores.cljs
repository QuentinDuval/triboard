(ns triboard.logic.scores
  (:require
    [cljs.spec :as s :include-macros true]
    [triboard.logic.constants :as cst]
    [triboard.logic.move :as move]
    ))


;; -----------------------------------------
;; Public API
;; -----------------------------------------

(s/def ::pos-int (s/and integer? #(<= 0 %)))
(s/def ::scores (s/map-of ::cst/player ::pos-int))

(s/fdef update-scores
  :args (s/tuple ::scores ::move/conversion)
  :ret (s/map-of ::cst/player integer?))                    ;; Hard to ensure positive number

(def initial-scores
  {:blue cst/init-block-count
   :red cst/init-block-count
   :green cst/init-block-count})

(defn update-scores
  "Update the scoring based on the provided move"
  [scores move]
  (let [delta (count (:taken move))]
    (-> scores
      (update (:winner move) + delta)
      (update (:looser move) - delta)
      (dissoc :empty))))
