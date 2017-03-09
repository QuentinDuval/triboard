(ns triboard.core
  (:require
    [cljs.core.async :as async :refer [put! chan <! >!]]
    [reagent.core :as reagent]
    [triboard.ai.ai :as ai]
    [triboard.store :as store]
    [triboard.view.callbacks :as view]
    [triboard.view.frame :as frame])
  (:require-macros
    [cljs.core.async.macros :refer [go go-loop alt!]]
    [reagent.ratom :refer [reaction]]))


;; (cljs.spec.test/instrument) ;; TODO - Does not work
;; (enable-console-print!)
;; (set! *assert* false)

;; TODO - Extract the parts that are related to costmetic: Help
;; TODO - Rework the game loop to be a state machine (beware of consuming messages)

;; -----------------------------------------
;; GAME LOOP
;; -----------------------------------------

(def ai-move-delay 1000)

(defn run-async
  [computation]
  (let [out-chan (chan 1)]
    (go (>! out-chan (computation)))
    out-chan))

(defn ai-computation
  [game]
  (let [out-chan (chan 1)]
    (go
      (<! (async/timeout 500))
      (let [ai-chan (run-async #(ai/play-best-move game))
            _ (<! (async/timeout ai-move-delay))
            g (<! ai-chan)]
        (>! out-chan g)))
    out-chan))

(defn start-game-loop
  "Manage transitions between player moves, ai moves, and generic game events"
  []
  (let [is-human-xf (fn [_] (not @store/ai-player?))
        player-events (chan 1 (filter is-human-xf))
        game-events (chan 1)]
    (go
      (while true
        (let [ai-chan (if @store/ai-player? (ai-computation @store/game) (chan 1))]
          (alt!
            game-events ([msg] (store/send-event! msg))
            player-events ([coord] (store/send-event! [:player-move coord]))
            ai-chan ([game] (store/send-event! [:ai-play game]))
            ))))
    {:player-events player-events
     :game-events game-events}))

(defonce game-loop (start-game-loop))

(defn send-player-event! [e] (put! (game-loop :player-events) e))
(defn send-game-event! [e] (put! (game-loop :game-events) e))


;; -----------------------------------------
;; PLUGGING THE BLOCKS
;; -----------------------------------------

(defn run-game []
  (frame/main-frame @store/current-turn @store/suggestions
    (reify view/CallBacks
      (on-new-game [_] (send-game-event! [:new-game]))
      (on-toogle-help [_] (send-game-event! [:toggle-help]))
      (on-restart [_] (send-game-event! [:restart]))
      (on-undo [_] (send-game-event! [:undo]))
      (on-player-move [_ x y] (send-player-event! [x y]))
      )))

(reagent/render [run-game]
  (js/document.getElementById "app"))
