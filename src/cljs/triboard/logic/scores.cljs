(ns triboard.logic.scores
  (:require
    [triboard.logic.constants :as cst]
    [triboard.logic.move :as move]
    ))


;; -----------------------------------------
;; Public API
;; -----------------------------------------

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
