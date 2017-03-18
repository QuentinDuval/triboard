(ns triboard.view.menu
  (:require
    [clojure.string :as str]
    [triboard.logic.player :as player]
    [triboard.view.interactions :as i]
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

(defn show-top-menu
  "Show the top menu of the game that contains
   * The player scores
   * The main commands"
  [scores current-player interactions]
  [:div.scores
   [make-button #(i/on-new-game interactions) vutils/star]
   [make-button #(i/on-toogle-help interactions) "?"]
   (show-scores scores #(= % current-player))
   [make-button #(i/on-restart interactions) vutils/circle-arrow]
   [make-button #(i/on-undo interactions) vutils/back-arrow]
   ])
