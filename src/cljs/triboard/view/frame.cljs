(ns triboard.view.frame
  (:require
    [cljs.spec :as s]
    [triboard.logic.game :as game]
    [triboard.view.board :as board]
    [triboard.view.callbacks :as cb]
    [triboard.view.panel :as panel]
    ))

(s/fdef main-frame
  :args (s/tuple
          ::game/game-state
          (s/fspec :args (s/tuple ::board/coord) :ret any?)
          #(satisfies? cb/CallBacks %)))

(defn main-frame
  [game-state suggestions interactions]
  [:div.game-panel
   [panel/show-top-panel (:scores game-state) (:player game-state) interactions]
   [board/render-board (:board game-state) suggestions interactions]
   ])
