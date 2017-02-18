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

(defn- player->css-style
  [player]
  {:class (str "score--" (name player))})

(defn- player->score-text
  [player score]
  (str (str/capitalize (name player)) " - " score))

(defn- show-scores
  [scores highlight?]
  (for [p cst/players]
    ^{:key p}
    [(if (highlight? p) :div.score--is-current :div.score)
     (player->css-style p)
     (player->score-text p (get scores p))]
    ))

(defn- top-panel-button
  [on-click txt]
  [:button.help-button {:on-click on-click} txt])

(defn show-top-panel
  "Show the top panel of the game that contains
   * The player scores
   * The main commands"
  [scores current-player cb]
  [:div.scores
   [top-panel-button #(cb/on-new-game cb) vutils/star]
   [top-panel-button #(cb/on-toogle-help cb) "?"]
   (show-scores scores #(= % current-player))
   [top-panel-button #(cb/on-restart cb) vutils/circle-arrow]
   [top-panel-button #(cb/on-undo cb) vutils/back-arrow]
   ])
