(ns triboard.core
  (:require
    [reagent.core :as reagent]
    [triboard.game-loop :as loop]
    [triboard.store :as store]
    [triboard.view.interactions :as view]
    [triboard.view.frame :as frame]))

;; (cljs.spec.test/instrument)
;; (enable-console-print!)
;; (set! *assert* false)

(defn run-game []
  (frame/main-frame @store/current-turn @store/suggestions
    (reify view/IUserInteractions
      (on-new-game [_] (loop/sent-menu-event! [:new-game]))
      (on-toogle-help [_] (loop/sent-menu-event! [:toggle-help]))
      (on-restart [_] (loop/sent-menu-event! [:restart]))
      (on-undo [_] (loop/sent-menu-event! [:undo]))
      (on-player-move [_ x y] (loop/send-game-event! [x y]))
      )))

(reagent/render [run-game]
  (js/document.getElementById "app"))
