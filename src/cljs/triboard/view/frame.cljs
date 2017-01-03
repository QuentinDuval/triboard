(ns triboard.view.frame
  (:require
    [triboard.logic.turn :as turn]
    [triboard.view.board :as board]
    [triboard.view.panel :as panel]
    ))


(defn main-frame
  [turn suggestions interactions]
  [:div.game-panel
   [panel/show-top-panel (turn/get-scores turn) (turn/get-player turn) interactions]
   [board/render-board (turn/get-board turn) suggestions interactions]
   ])
