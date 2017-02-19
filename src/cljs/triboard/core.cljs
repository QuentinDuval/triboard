(ns triboard.core
  (:require
    [cljs.core.async :as async :refer [put! chan <! >!]]
    [cljs.spec.test :as stest]
    [reagent.core :as reagent]
    [triboard.store :as store]
    [triboard.view.callbacks :as view]
    [triboard.view.frame :as frame]
    )
  (:require-macros
    [cljs.core.async.macros :refer [go go-loop alt!]]
    [reagent.ratom :refer [reaction]]))


;; (stest/instrument)
;; (enable-console-print!)
;; (set! *assert* false)

;; TODO - Extract the parts that are related to costmetic: Help
;; TODO - Rework the game loop to be a state machine (beware of consuming messages)
;; TODO - Rework the AI part, to be able to keep data linked to the init board

;; -----------------------------------------
;; GAME LOOP
;; -----------------------------------------

(def ai-move-delay 1000)

(defn start-game-loop
  "Manage transitions between player moves, ai moves, and generic game events"
  []
  (let [is-human-xf (fn [_] (not @store/ai-player?))
        player-events (chan 1 (filter is-human-xf))
        game-events (chan 1)]
    (go
      (while true
        (alt!
          game-events ([msg] (store/send-event! msg))
          player-events ([coord] (store/send-event! [:player-move coord]))
          (async/timeout ai-move-delay) ([_] (if @store/ai-player? (store/send-event! [:ai-play])))
          )))
    {:player-events player-events
     :game-events game-events}))

(defonce game-loop (start-game-loop))

(defn send-player-event! [e] (put! (game-loop :player-events) e))
(defn send-game-event! [e] (put! (game-loop :game-events) e))


;; -----------------------------------------
;; PLUGGING THE BLOCKS
;; -----------------------------------------

(defn run-game []
  (frame/main-frame @store/current-state @store/suggestions
    (reify view/CallBacks
      (on-new-game [_] (send-game-event! [:new-game]))
      (on-toogle-help [_] (send-game-event! [:toggle-help]))
      (on-restart [_] (send-game-event! [:restart]))
      (on-undo [_] (send-game-event! [:undo]))
      (on-player-move [_ x y] (send-player-event! [x y]))
      )))

(reagent/render [run-game]
  (js/document.getElementById "app"))
