(ns triboard.view.frame
  (:require
    [cljs.spec :as s]
    [triboard.logic.turn :as turn]
    [triboard.view.board :as board]
    [triboard.view.callbacks :as cb]
    [triboard.view.panel :as panel]
    ))

(s/fdef main-frame
  :args (s/tuple
          ::turn/turn
          (s/fspec :args (s/tuple ::board/coord) :ret any?)
          #(satisfies? cb/CallBacks %)))

(defn main-frame
  [turn suggestions interactions]
  [:div.game-panel
   [panel/show-top-panel (turn/get-scores turn) (turn/get-player turn) interactions]
   [board/render-board (turn/get-board turn) suggestions interactions]
   ])
