(ns triboard.view.frame
  (:require
    [cljs.spec :as s]
    [triboard.logic.turn :as turn]
    [triboard.view.board :as board]
    [triboard.view.interactions :as cb]
    [triboard.view.menu :as panel]))

(s/fdef main-frame
  :args (s/cat
          :turn ::turn/turn
          :suggestions any?
          :callbacks #(satisfies? cb/IUserInteractions %)))

(defn main-frame
  [turn suggestions callbacks]
  [:div.game-panel
   [panel/show-top-menu (:scores turn) (:player turn) callbacks]
   [board/render-board (:board turn) suggestions callbacks]
   ])
