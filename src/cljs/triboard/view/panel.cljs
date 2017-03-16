(ns triboard.view.panel
  (:require
    [clojure.string :as str]
    [triboard.logic.player :as player]
    [triboard.view.interactions :as cb]
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
  (for [player player/all :let [score (get scores player)]]
    ^{:key player}
    [:div
     {:class (player->css-style player highlight?)}
     (player->score-text player score)]
    ))

(defn- make-button
  [on-click txt]
  [:button.help-button {:on-click on-click} txt])


;; ----------------------------------------------------------------------------
;; Public
;; ----------------------------------------------------------------------------

(defn show-top-panel
  "Show the top panel of the game that contains
   * The player scores
   * The main commands"
  [scores current-player cb]
  [:div.scores
   [make-button #(cb/on-new-game cb) vutils/star]
   [make-button #(cb/on-toogle-help cb) "?"]
   (show-scores scores #(= % current-player))
   [make-button #(cb/on-restart cb) vutils/circle-arrow]
   [make-button #(cb/on-undo cb) vutils/back-arrow]
   ])
