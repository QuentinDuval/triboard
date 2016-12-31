(ns triboard.view.panel
  (:require
    [clojure.string :as str]
    [triboard.logic.constants :as cst]
    ))


(defn scores?
  "Specification of the input for `show-scores`"
  [s]
  (and
    (<= 0 (reduce + (vals s)) cst/max-score)
    (every? s cst/players)))


(defn show-scores
  [scores player]
  {:pre [(scores? scores)]}
  (for [p cst/players]
    ^{:key p}
    [(if (= player p) :div.score--is-current :div.score)
     {:class (str "score--" (name p))}
     (str (str/capitalize (name p)) " - " (get scores p))]
    ))


(defn top-panel-button
  [on-click txt]
  [:button.help-button {:on-click on-click} txt])

