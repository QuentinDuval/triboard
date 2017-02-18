(ns triboard.view.panel
  (:require
    [clojure.string :as str]
    [cljs.spec :as s]
    [triboard.logic.constants :as cst]
    [triboard.view.callbacks :as cb]
    [triboard.view.utils :as vutils]
    ))


;; ----------------------------------------------------------------------------
;; Private
;; ----------------------------------------------------------------------------

(defn- player->css-style
  [player highlight?]
  (let [player-class (str "score--" (name player))
        score-class (if (highlight? player) "score--is-current" "score")]
    (str player-class " " score-class)))

(defn- player->score-text
  [player score]
  (str (str/capitalize (name player)) " - " score))

(defn- show-scores
  [scores highlight?]
  (for [player cst/players :let [score (get scores player)]]
    ^{:key player}
    [:div
     {:class (player->css-style player highlight?)}
     (player->score-text player score)]
    ))

(defn- top-panel-button
  [on-click txt]
  [:button.help-button {:on-click on-click} txt])


;; ----------------------------------------------------------------------------
;; Public
;; ----------------------------------------------------------------------------

(s/fdef show-scores
  :args (s/tuple #(every? % cst/players)))

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
