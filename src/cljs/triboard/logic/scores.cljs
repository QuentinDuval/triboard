(ns triboard.logic.scores
  (:require
    [cljs.spec :as s :include-macros true]
    [cljs.spec.impl.gen :as gen]
    [triboard.logic.constants :as cst]
    [triboard.logic.move :as move]
    ))


;; -----------------------------------------
;; Public API
;; -----------------------------------------

(s/def ::pos-int (s/and integer? #(<= 0 %)))
(s/def ::blue ::pos-int)
(s/def ::red ::pos-int)
(s/def ::green ::pos-int)
(s/def ::scores (s/keys :req-un [::blue ::red ::green]))

(s/fdef update-scores
  :args (s/tuple ::scores ::move/conversion)
  ;; :ret ::scores ;;TODO - There could be violation without more constraints on inputs
  )

(def initial-scores
  {:blue cst/init-block-count
   :red cst/init-block-count
   :green cst/init-block-count})

(defn update-scores
  "Update the scoring based on the provided move"
  [scores move]
  {:pre [(move/conversion? move)]}
  (let [delta (count (:taken move))]
    (-> scores
      (update (:winner move) + delta)
      (update (:looser move) - delta)
      )))
