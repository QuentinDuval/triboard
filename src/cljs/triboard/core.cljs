(ns triboard.core
  (:require
    [reagent.core :as reagent]
    [triboard.game-loop :as loop]
    [triboard.store :as store]
    [triboard.view.callbacks :as view]
    [triboard.view.frame :as frame]))

;; (cljs.spec.test/instrument)
;; (enable-console-print!)
;; (set! *assert* false)

;; TODO - Extract the parts that are related to costmetic: Help
;; TODO - Rework the game loop to be a state machine (beware of consuming messages)

(defn run-game []
  (frame/main-frame @store/current-turn @store/suggestions
    (reify view/CallBacks
      (on-new-game [_] (loop/sent-menu-event! [:new-game]))
      (on-toogle-help [_] (loop/sent-menu-event! [:toggle-help]))
      (on-restart [_] (loop/sent-menu-event! [:restart]))
      (on-undo [_] (loop/sent-menu-event! [:undo]))
      (on-player-move [_ x y] (loop/send-game-event! [x y]))
      )))

(reagent/render [run-game]
  (js/document.getElementById "app"))
