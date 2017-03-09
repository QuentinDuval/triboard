(ns triboard.game-loop
  (:require
    [cljs.core.async :as async :refer [put! chan <! >!]]
    [triboard.ai.ai :as ai]
    [triboard.store :as store]
    [triboard.utils.async :refer [run-async-fn]])
  (:require-macros
    [cljs.core.async.macros :refer [go go-loop alt!]]
    [triboard.utils.async :refer [run-async]]
    ))


;; -----------------------------------------
;; GAME LOOP
;; -----------------------------------------

(def animation-delay 500)
(def ai-move-delay 1000)

(defn ai-computation
  [game]
  (let [out-chan (chan 1)]
    (go
      (<! (async/timeout animation-delay))
      (let [ai-chan (run-async (ai/find-best-move game))]
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
