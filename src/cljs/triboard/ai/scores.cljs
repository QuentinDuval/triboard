(ns triboard.ai.scores
  (:require
    [triboard.logic.move :as move]
    ))


(def null-delta
  {:blue 0 :red 0 :green 0})

(defn update-delta
  [get-cell-strength delta conversion]
  {:pre [(move/conversion? conversion)]}
  (let [diff (transduce (map get-cell-strength) + (:taken conversion))]
    (-> delta
      (update (:looser delta) - diff)
      (update (:winner delta) + diff)
      )))
