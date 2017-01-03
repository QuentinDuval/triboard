(ns triboard.core
  (:require
    [cljs.core.async :as async :refer [put! chan <! >!]]
    [reagent.core :as reagent]
    [triboard.store :as store]
    [triboard.view.callbacks :as view]
    [triboard.view.frame :as frame]
    )
  (:require-macros
    [cljs.core.async.macros :refer [go go-loop alt!]]
    [reagent.ratom :refer [reaction]]))


(enable-console-print!)
(set! *assert* true)                                        ;; Set to true for the debug mode

;; TODO - Extract the parts that are related to costmetic: Help
;; TODO - Rework the game loop to be a state machine (beware of consuming messages)
;; TODO - Rework the AI part, to be able to keep data linked to the init board

;; TODO - http://www.w3schools.com/howto/howto_js_sidenav.asp
;; TODO - http://www.w3schools.com/svg/svg_grad_radial.asp


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
          game-events ([msg] (store/handle-game-event! msg))
          player-events ([coord] (store/handle-game-event! [:player-move coord]))
          (async/timeout ai-move-delay) ([_] (if @store/ai-player? (store/handle-game-event! [:ai-play])))
          )))
    {:player-events player-events
     :game-events game-events}))

(defonce game-loop (start-game-loop))

(defn send-player-event! [e]
  (put! (game-loop :player-events) e))

(defn send-game-event! [e]
  (put! (game-loop :game-events) e))


;; -----------------------------------------
;; PLUGGING THE BLOCKS
;; -----------------------------------------

(defn run-game []
  (frame/main-frame @store/current-turn @store/suggestions
    (reify view/CallBacks
      (on-new-game [_] (send-game-event! [:new-game]))
      (on-toogle-help [_] (store/toogle-help!))
      (on-restart [_] (send-game-event! [:restart]))
      (on-undo [_] (send-game-event! [:undo]))
      (on-player-move [_ x y] (send-player-event! [x y]))
      )))

(reagent/render [run-game]
  (js/document.getElementById "app"))
