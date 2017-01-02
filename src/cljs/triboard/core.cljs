(ns triboard.core
  (:require
    [cljs.core.async :as async :refer [put! chan <! >!]]
    [reagent.core :as reagent :refer [atom]]
    [triboard.ai.ai :as ai]
    [triboard.logic.game :as game]
    [triboard.logic.turn :as turn]
    [triboard.view.board :as vboard]
    [triboard.view.callbacks :as view]
    [triboard.view.panel :as panel]
    )
  (:require-macros
    [cljs.core.async.macros :refer [go go-loop alt!]]
    [reagent.ratom :refer [reaction]]))


(enable-console-print!)
(set! *assert* true) ;; Set to true for the debug mode

;; TODO - Rework the connection with AI: the AI should not be in the game (could be function)
;; TODO - Move the assembling of the display in view
;; TODO - Rework the queue system to have something simpler
;; TODO - Extract the parts that are related to costmetic: Help

;; TODO - http://www.w3schools.com/howto/howto_js_sidenav.asp
;; TODO - http://www.w3schools.com/svg/svg_grad_radial.asp


;; -----------------------------------------
;; APP STATE
;; -----------------------------------------

(defonce app-state
  (atom {:games (game/new-game)
         :help false}))

(def game (reaction (game/current-turn (:games @app-state))))
(def board (reaction (turn/get-board @game)))
(def scores (reaction (turn/get-scores @game)))
(def current-player (reaction (turn/current-player @game)))


;; -----------------------------------------
;; GAME LOOP
;; -----------------------------------------

(def is-ai? #{:red :green})
(def ai-move-delay 1000)

(defn play-game-turn! [move]
  (swap! app-state update-in [:games] game/play-move move))

(defn- handle-ai! []
  ;; TODO - Find a way to cache the cells-strenght as it was done before
  (let [move (ai/best-move (ai/compute-cells-strength @board) @game @current-player)]
    (play-game-turn! move)))

(defn- handle-game-event!
  [msg]
  (case msg
    :new-game (swap! app-state assoc-in [:games] (game/new-game))
    :restart (swap! app-state update-in [:games] #(take-last 1 %))
    :undo (swap! app-state update-in [:games] game/undo-player-move is-ai?)))

(defn start-game-loop
  "Manage transitions between player moves, ai moves, and generic game events"
  []
  (let [game-on-xf (fn [_] (not (turn/game-over? @game)))
        is-human-xf (fn [_] (not (is-ai? @current-player)))
        player-events (chan 1 (comp (filter game-on-xf) (filter is-human-xf)))
        game-events (chan 1)]
    (go
      (while true
        (alt!
          game-events ([msg] (handle-game-event! msg))
          player-events ([coord] (play-game-turn! coord))
          (async/timeout ai-move-delay) ([_] (if (is-ai? @current-player) (handle-ai!)))
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
;; DISPLAY
;; -----------------------------------------

(defn ^boolean show-help?
  [x y]
  (and
    (:help @app-state)
    (not (is-ai? @current-player))
    (turn/get-move-at @game @current-player [x y])))

(def interactions
  (reify view/CallBacks
    (on-new-game [_] (send-game-event! :new-game))
    (on-toogle-help [_] (toogle-help!))
    (on-restart [_] (send-game-event! :restart))
    (on-undo [_] (send-game-event! :undo))
    (on-player-move [_ x y] (send-player-event! [x y]))
    (show-as-help? [_ x y] (show-help? x y))
    ))

(defn run-game []
  [:div.game-panel
   [panel/show-top-panel @scores @current-player interactions]
   (vboard/render-board @board interactions)
   ])

(reagent/render [run-game]
  (js/document.getElementById "app"))
