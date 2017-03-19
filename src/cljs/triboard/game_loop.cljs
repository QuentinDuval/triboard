(ns triboard.game-loop
  (:require
    [cljs.core.async :as async :refer [put! chan <! >!]]
    [triboard.ai.ai :as ai]
    [triboard.logic.game :as game]
    [triboard.store :as store])
  (:require-macros
    [cljs.core.async.macros :refer [go go-loop alt!]]
    ))


(def animation-delay 500)
(def ai-move-delay 1000)

(defn ai-computation
  "Run the computation of the AI asynchronously:
   * Wait 500ms to start (animation might be frozen otherwise)
   * Wait 1s to play the move (avoid moves being played too fast)"
  [game]
  (go
    (<! (async/timeout animation-delay))
    (let [ai-chan (go (ai/find-best-move game))]
      (<! (async/timeout ai-move-delay))
      (<! ai-chan))))

(defn handle-menu-event!
  "Handle the menu event"
  [msg]
  (case msg
    :toggle-help (store/toggle-help!)
    :new-game (store/swap-game! (fn [_] (game/new-game)))
    :restart (store/swap-game! game/restart-game)
    :undo (store/swap-game! game/undo-player-move)))

(defn start-game-loop
  "Manage transitions between player moves, ai moves, and generic game events"
  []
  (let [player-moves (chan 1 (filter #(not @store/ai-player?)))
        menu-events (chan 1)]
    (go
      (while true
        (let [play-chan (if @store/ai-player?
                          (ai-computation @store/game)
                          player-moves)]
          (alt!
            menu-events ([msg] (handle-menu-event! msg))
            play-chan ([coord] (store/swap-game! game/play-at coord))
            ))))
    {:game-events player-moves
     :menu-events menu-events}))

(defonce game-loop (start-game-loop))

(defn send-game-event! [e] (put! (game-loop :game-events) e))
(defn sent-menu-event! [e] (put! (game-loop :menu-events) e))
