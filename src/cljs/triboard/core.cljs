(ns triboard.core
  (:require
    [cljs.core.async :as async :refer [put! chan <! >!]]
    [reagent.core :as reagent :refer [atom]]
    [triboard.ai.ai :as ai]
    [triboard.logic.game :as game]
    [triboard.logic.turn :as turn]
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
;; APP STATE
;; -----------------------------------------

(def is-ai? #{:red :green})
(def ai-move-delay 1000)

(defonce app-state
  (atom {:game (game/new-game)
         :help false}))

(def current-turn (reaction (game/current-turn (:game @app-state))))
(def current-player (reaction (turn/get-player @current-turn)))

(def suggestions
  (reaction
    (if (and (:help @app-state) (not (is-ai? @current-player)))
      (turn/get-moves-of @current-turn @current-player)
      {})))

(defn play-game-turn! [move]
  (swap! app-state update :game game/play-move move))

(defn- handle-game-event!
  [msg]
  (case msg
    :new-game (swap! app-state assoc :game (game/new-game))
    :restart (swap! app-state update :game #(take-last 1 %))
    :undo (swap! app-state update :game game/undo-player-move is-ai?)
    :ai-play (play-game-turn! (ai/best-move @current-turn @current-player))
    ))


;; -----------------------------------------
;; GAME LOOP
;; -----------------------------------------

(defn start-game-loop
  "Manage transitions between player moves, ai moves, and generic game events"
  []
  (let [is-human-xf (fn [_] (not (is-ai? @current-player)))
        player-events (chan 1 (filter is-human-xf))
        game-events (chan 1)]
    (go
      (while true
        (alt!
          game-events ([msg] (handle-game-event! msg))
          player-events ([coord] (play-game-turn! coord))
          (async/timeout ai-move-delay) ([_] (if (is-ai? @current-player) (handle-game-event! :ai-play)))
          )))
    {:player-events player-events
     :game-events game-events}))

(defonce game-loop (start-game-loop))

(defn send-player-event! [e]
  (put! (game-loop :player-events) e))

(defn send-game-event! [e]
  (put! (game-loop :game-events) e))

(defn toogle-help! []
  (swap! app-state update :help not))


;; -----------------------------------------
;; PLUGGING THE BLOCKS
;; -----------------------------------------

(defn run-game []
  (frame/main-frame @current-turn @suggestions
    (reify view/CallBacks
      (on-new-game [_] (send-game-event! :new-game))
      (on-toogle-help [_] (toogle-help!))
      (on-restart [_] (send-game-event! :restart))
      (on-undo [_] (send-game-event! :undo))
      (on-player-move [_ x y] (send-player-event! [x y]))
      )))

(reagent/render [run-game]
  (js/document.getElementById "app"))
