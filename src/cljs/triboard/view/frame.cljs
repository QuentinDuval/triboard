(ns triboard.view.frame
  (:require
    [cljs.spec :as s]
    [triboard.logic.turn :as turn]
    [triboard.view.board :as board]
    [triboard.view.callbacks :as cb]
    [triboard.view.panel :as panel]))

#_(s/fdef main-frame
  :args (s/cat
          :turn ::turn/turn
          :suggestions fn?
          :callbacks #(satisfies? cb/CallBacks %)))

(defn main-frame
  [turn suggestions callbacks]
  [:div.game-panel
   [panel/show-top-panel (:scores turn) (:player turn) callbacks]
   [board/render-board (:board turn) suggestions callbacks]
   ])
