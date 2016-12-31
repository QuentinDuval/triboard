(ns triboard.view.panel
  (:require
    [clojure.string :as str]
    [triboard.logic.constants :as cst]
    [triboard.view.utils :as vutils]
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


(defn show-top-panel
  [scores player {:keys [on-new-game on-help on-restart on-undo]}]
  [:div.scores
   [top-panel-button on-new-game (vutils/special-char "&#9733;")]
   [top-panel-button on-help "?"]
   (show-scores scores player)
   [top-panel-button on-restart (vutils/special-char "&#x21bb;")]
   ;;[panel/top-panel-button on-undo (special-char "&#x21A9;")]
   [top-panel-button on-undo (vutils/special-char "&larr;")]
   ])
