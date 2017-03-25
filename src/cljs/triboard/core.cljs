(ns triboard.core
  (:require
    [reagent.core :as reagent]
    [triboard.game-loop :as loop]
    [triboard.store :as store]
    [triboard.view.interactions :as view]
    [triboard.view.frame :as frame]))

(defn triboard []
  (frame/main-frame @store/current-turn @store/suggestions
    (reify view/IUserInteractions
      (on-new-game [_] (loop/send-game-event! :new-game))
      (on-toogle-help [_] (store/toggle-help!))
      (on-restart [_] (loop/send-game-event! :restart))
      (on-undo [_] (loop/send-game-event! :undo))
      (on-player-move [_ x y] (loop/send-play-event! [x y]))
      )))

(reagent/render [triboard]
  (js/document.getElementById "app"))
