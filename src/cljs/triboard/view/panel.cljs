(ns triboard.view.panel
  (:require
    [clojure.string :as str]
    [cljs.spec :as s]
    [triboard.logic.constants :as cst]
    [triboard.view.callbacks :as cb]
    [triboard.view.utils :as vutils]
    ))


(s/fdef show-scores
  :args (s/tuple #(every? % cst/players)))

(defn- show-scores
  [scores player]
  (for [p cst/players]
    ^{:key p}
    [(if (= player p) :div.score--is-current :div.score)
     {:class (str "score--" (name p))}
     (str (str/capitalize (name p)) " - " (get scores p))]
    ))

(defn- top-panel-button
  [on-click txt]
  [:button.help-button {:on-click on-click} txt])

(defn show-top-panel
  [scores player cb]
  [:div.scores
   [top-panel-button #(cb/on-new-game cb) vutils/star]
   [top-panel-button #(cb/on-toogle-help cb) "?"]
   (show-scores scores player)
   [top-panel-button #(cb/on-restart cb) vutils/circle-arrow]
   [top-panel-button #(cb/on-undo cb) vutils/back-arrow]
   ])
