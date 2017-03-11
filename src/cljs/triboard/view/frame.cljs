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
  (let [turn-data (turn/turn->info turn)]
    [:div.game-panel
     [panel/show-top-panel (:scores turn-data) (:player turn-data) callbacks]
     [board/render-board (:board turn-data) suggestions callbacks]
     ]))
