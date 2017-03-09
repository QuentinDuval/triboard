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


;; (cljs.spec.test/instrument)
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
      (let [ai-chan (run-async #(ai/find-best-move game))]
        (<! (async/timeout ai-move-delay))
        (>! out-chan (<! ai-chan))))
    out-chan))

(defn player-moves-chan
  []
  (chan 1 (filter #(not @store/ai-player?))))

(defn start-game-loop
  "Manage transitions between player moves, ai moves, and generic game events"
  []
  (let [player-moves (player-moves-chan)
        menu-events (chan 1)]
    (go
      (while true
        (let [play-chan (if @store/ai-player? (ai-computation @store/game) player-moves)]
          (alt!
            menu-events ([msg] (store/send-event! msg))
            play-chan ([coord] (store/send-event! [:play-at coord]))
            ))))
    {:game-events player-moves
     :menu-events menu-events}))

(defonce game-loop (start-game-loop))

(defn send-game-event! [e] (put! (game-loop :game-events) e))
(defn sent-menu-event! [e] (put! (game-loop :menu-events) e))


;; -----------------------------------------
;; PLUGGING THE BLOCKS
;; -----------------------------------------

(defn run-game []
  (frame/main-frame @store/current-turn @store/suggestions
    (reify view/CallBacks
      (on-new-game [_] (sent-menu-event! [:new-game]))
      (on-toogle-help [_] (sent-menu-event! [:toggle-help]))
      (on-restart [_] (sent-menu-event! [:restart]))
      (on-undo [_] (sent-menu-event! [:undo]))
      (on-player-move [_ x y] (send-game-event! [x y]))
      )))

(reagent/render [run-game]
  (js/document.getElementById "app"))
