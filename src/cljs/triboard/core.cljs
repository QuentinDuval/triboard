(ns triboard.core
  (:require
    [cljs.core.async :as async :refer [put! chan <! >!]]
    [reagent.core :as reagent :refer [atom]]
    [triboard.ai.ai :as ai]
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

;; TODO - Try to bring some domain vocabulary here: it gets too complicated to trace
;; TODO - http://www.w3schools.com/howto/howto_js_sidenav.asp
;; TODO - http://www.w3schools.com/svg/svg_grad_radial.asp


;; -----------------------------------------
;; ARTIFICIAL INTELLIGENCE
;; -----------------------------------------

(defn with-ai-data
  "Add the AI data needed to play the game"
  [{:keys [board] :as init-game}]
  (merge init-game
    {:ai-players
     {:red (partial ai/best-move (ai/compute-cells-strength board))
      :green (partial ai/best-move (ai/compute-cells-strength board))}}
    ))


;; -----------------------------------------
;; GAME STATE
;; -----------------------------------------

(defn new-game []
  (list (with-ai-data (turn/new-init-turn))))

(defonce app-state
  (atom {:games (new-game)
         :help false}))

(def game (reaction (first (get-in @app-state [:games]))))
(def board (reaction (get-in @game [:board])))
(def scores (reaction (get-in @game [:scores])))
(def current-player (reaction (get-in @game [:player])))
(def ai-players (reaction (get-in @game [:ai-players])))
(def end-of-game (reaction (nil? @current-player)))

(defn is-ai? [player]
  (contains? @ai-players player))


;; -----------------------------------------
;; GAME LOOP
;; -----------------------------------------

(def ai-move-delay 1000)

(defn update-game! [f & args]
  (swap! app-state update-in [:games] conj (apply f @game args)))

(defn- handle-ai! []
  (let [ai-algo (get @ai-players @current-player)
        move (ai-algo @game @current-player)]
    (update-game! turn/play-move move)))

(defn cancel-last-move ;; TODO - It needs the ai: how to move it in the game? undo then undo-while
  [old-turns]
  (let [ai-turn? #(contains? (:ai-players %) (:player %))
        new-turns (drop-while ai-turn? (drop 1 old-turns))]
    (if (empty? new-turns)
      (take-last 1 old-turns)
      new-turns)))

(defn- handle-game-event!
  [msg]
  (case msg
    :new-game (swap! app-state assoc-in [:games] (new-game))
    :restart (swap! app-state update-in [:games] #(take-last 1 %))
    :undo (swap! app-state update-in [:games] cancel-last-move)))

(defn start-game-loop
  "Manage transitions between player moves, ai moves, and generic game events"
  []
  (let [game-on-xf (fn [_] (not @end-of-game))
        is-human-xf (fn [_] (not (is-ai? @current-player)))
        is-ai-xf (fn [_] (is-ai? @current-player))
        ai-events (chan 1 (comp (filter game-on-xf) (filter is-ai-xf)))
        player-events (chan 1 (comp (filter game-on-xf) (filter is-human-xf)))
        game-events (chan 1)]

    (go
      (while true
        (alt!
          game-events ([msg] (handle-game-event! msg))
          player-events ([coord] (update-game! turn/play-move coord))
          ai-events ([msg] (when (= msg :ai-play) (handle-ai!)))
          (async/timeout ai-move-delay) ([_] (put! ai-events :ai-play))
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
