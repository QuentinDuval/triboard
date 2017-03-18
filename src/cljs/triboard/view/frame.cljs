(ns triboard.view.frame
  (:require
    [cljs.spec :as s]
    [triboard.logic.board :as board-model]
    [triboard.logic.turn :as turn]
    [triboard.view.board :as board]
    [triboard.view.interactions :refer [IUserInteractions]]
    [triboard.view.menu :as menu]))

(s/fdef main-frame
  :args (s/cat
          :turn ::turn/turn
          :helps (s/fspec :args (s/cat :coord ::board-model/coord))
          :interactions #(satisfies? IUserInteractions %)))

(defn main-frame
  [turn suggestions interactions]
  [:div.game-panel
   [menu/show-top-menu (:scores turn) (:player turn) interactions]
   [board/render-board (:board turn) suggestions interactions]
   ])
