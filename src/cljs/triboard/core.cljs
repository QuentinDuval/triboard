(ns triboard.core
  (:require
    [clojure.string :as str]
    [cljs.core.async :as async :refer [put! chan <! >!]]
    [reagent.core :as reagent :refer [atom]])
  (:require-macros
    [cljs.core.async.macros :refer [go go-loop alt!]]
    [reagent.ratom :refer [reaction]]))


(enable-console-print!)
(set! *assert* false) ;; Set to true for the debug mode

;; TODO - Try to plug the history of brower for "back"
;; TODO - Try to bring some domain vocabulary here: it gets too complicated to trace

;; -----------------------------------------
;; GAME PARAMETERS
;; -----------------------------------------

(def board-width 16)
(def board-height 11)
(def init-block-count 12) ;; Init blocks for red, blue, green, gray
(def players [:blue :red :green])
(def directions [[-1 0] [1 0] [0 -1] [0 1] [-1 1] [1 1] [-1 -1] [1 -1]])


;; -----------------------------------------
;; EXTRINSIC TYPES
;; -----------------------------------------

(def player? (set players))
(def cell? (conj player? :wall :empty))

(defn board? [b]
  (every? #(every? cell? %) b))

(defn coord? [p]
  (and
    (integer? (first p))
    (integer? (second p))
    (= 2 (count p))))

(defn move? [m]
  (and
    (coord? (:move m))
    (every? coord? (:taken m))
    (player? (:winner m))
    (cell? (:looser m))))

(defn scores? [s]
  (and
    (<= 0 (reduce + (vals s)) 164)
    (every? s players)))


;; -----------------------------------------
;; UTILS
;; -----------------------------------------

(defn fast-max-key
  "Fast max key that avoids recomputing things several times"
  {:pre [(fn? key-fn) (seq? coll)]}
  [key-fn coll]
  (apply max-key (memoize key-fn) coll))

(def all-positions
  (vec
    (for [x (range board-width)
          y (range board-height)]
      [x y])))

(defn coord-neighbors
  "All neighbors of a given coordinate"
  [[x y]]
  (map (fn [[dx dy]] [(+ x dx) (+ y dy)]) directions))


;; -----------------------------------------
;; INIT THE BOARD
;; -----------------------------------------

(def empty-board
  (let [column (vec (repeat board-height :empty))]
    (vec (repeat board-width column))))

(defn draw-slices
  "Draw n * m slices from the collection"
  [n slices positions]
  (mapcat
    (fn [k] (map vector
              (repeat (nth slices k))
              (subvec positions (* k n) (* (inc k) n))
              ))
    (range (count slices))))

(defn init-positions
  "Create random initial positions for the players"
  []
  (draw-slices init-block-count
    (conj players :wall)
    (shuffle all-positions)))

(defn new-board []
  {:post [(board? %)]}
  (reduce
    (fn [r [color point]] (assoc-in r point color))
    empty-board (init-positions)))


;; -----------------------------------------
;; IDENTIFY CONVERTIBLE CELLS
;; -----------------------------------------

(defn ^boolean is-cell-empty?
  "Indicates whether a cell is owned by any player"
  [cell]
  (or (nil? cell) (= cell :empty) (= cell :wall)))

(defn available-cells-by-dir
  "Indicates the convertible cells when clicking at [x y], and returns:
	{:move [7 5]
   :winner :blue,
   :looser :red,
   :taken [[8 5] [9 5]]}"
  [board [xi yi :as move] [dx dy]]
  (loop [x (+ xi dx)
         y (+ yi dy)
         looser nil
         taken []]
    (let [cell (get-in board [x y])]
      (cond
        (is-cell-empty? cell) nil ;; No move: reached end and only 1 type of cell
        (and looser (not= looser cell)) {:winner cell   ;; Who wins the cells
                                         :looser looser ;; Who looses the cells
                                         :move move     ;; The move performed 
                                         :taken taken}  ;; The cells taken
        :else (recur
                (+ x dx) (+ y dy)
                cell (conj taken [x y])))
      )))

(defn available-moves-at
  "Provides the list of moves that can be done from a cell"
  {:pre [(board? board) (coord? point)]}
  [board point]
  (eduction (keep #(available-cells-by-dir board point %)) directions))

(defn take-empty-cell-move
  "Create a move to take an empty cell" 
  [player point]
  {:move point
   :winner player
   :looser :empty
   :taken [point]})

(defn available-moves-xf
  "Transducer to get available moves on the map"
  [board]
  (comp
    (filter #(= (get-in board %) :empty))
    (mapcat #(available-moves-at board %))
    ))

(defn with-available-moves
  "Compute all available moves on the board.
   And group these positions by player then by position
   Example:
   {:blue
    {[0 5]
     ({:move [0 5],
       :winner :blue,
       :looser :red,
       :taken [[0 4]]})}}"
  [{:keys [board] :as game}]
  (assoc game :moves
    (transduce
      (available-moves-xf board)
      #(update-in %1 [(:winner %2) (:move %2)] conj %2)
      {} all-positions)))


;; -----------------------------------------
;; ON PLAYER MOVE
;; -----------------------------------------

(defn- next-player
  [player] 
  (case player
    :blue :red
    :red :green
    :green :blue))

(defn- with-next-player
  "Find the next player to act - dismiss those that cannot play any move"
  [{:keys [moves player] :as game}]
  (let [nexts (take 3 (iterate next-player (next-player player)))]
    (assoc game :player
      (some #(if (get moves %) % false) nexts))
    ))

(defn- apply-move
  [game {:keys [winner looser taken] :as move}]
  {:pre [(move? move)]}
  (-> game
    (assoc :board (reduce #(assoc-in %1 %2 winner) (:board game) taken))
    (update-in [:scores winner] + (count taken))
    (update-in [:scores looser] - (count taken))
    ))

(defn- get-move-at
  "Access the available moves for the provided player at the provided point"
  [game player point]
  (get-in game [:moves player point]))

(defn play-move
  "On player playing the move [x y]"
  [{:keys [player board] :as game} point]
  (if-let [moves (get-move-at game player point)]
    (->
      (reduce apply-move game moves)
      (apply-move (take-empty-cell-move player point))
      (with-available-moves)
      (with-next-player))
    game))


;; -----------------------------------------
;; ARTIFICIAL INTELLIGENCE
;; -----------------------------------------

(defn- compute-cell-strength
  "Compute a cell strength based on the number of walls it has"
  {:pre [(coord? point)]}
  [board point]
  (let [neighbors (coord-neighbors point)
        walls (filter #(= :wall (get-in board % :wall)) neighbors)]
    (+ 1 (* (count walls) 0.25))))

(defn with-cells-strength
  "Adds to a given game the strength of each of its cells"
  [{:keys [board] :as game}]
  (assoc game :cells-strength
    (reduce
      #(assoc %1 %2 (compute-cell-strength board %2))
      {} all-positions)))

(defn- move-strength
  "Compute the strength of a move, based on the converted cells"
  [cells-strength converted-filter [move converted]]
  {:pre [(fn? converted-filter) (every? move? converted)]}
  (transduce
    (comp
      (filter converted-filter) ;; TODO - Extract this part (specific to worst move)
      (mapcat :taken)
      (map cells-strength))
    + converted))

(defn- worst-immediate-loss
  "Return the next worse lost game move for 'looser' if 'player' plays"
  {:pre [(player? player) (player? looser)]}
  [game player looser]
  (let [converted-filter #(= looser (:looser %))
        all-moves (get-in game [:moves player])]
    (transduce
      (map #(move-strength (:cells-strength game) converted-filter %))
      max all-moves)))

(defn best-move
  "[SIMPLISTIC] Return the best move for a player based on:
   * The immediate gain
   * The worse immediate lost afterwards"
  {:pre [(player? player)]}
  [game player]
  (let [moves (get-in game [:moves player])
        others (remove #{player} players)]
    (first
      (fast-max-key
        (fn [[m converted :as move]]
          (let [new-game (play-move game m)
                diff-score (move-strength (:cells-strength game) identity move)
                losses (map #(worst-immediate-loss new-game % player) others)]
            (- diff-score (apply max losses))))
        moves))
    ))


;; -----------------------------------------
;; GAME STATE
;; -----------------------------------------

(defn new-game []
  (with-available-moves
    (with-cells-strength
      {:board (new-board)
       :player (rand-nth players)
       :moves {}
       :help false
       :scores
       {:blue 12
        :red 12
        :green 12}})))

(defonce app-state (atom (new-game)))
(def board (reagent/cursor app-state [:board]))
(def scores (reagent/cursor app-state [:scores]))
(def current-player (reagent/cursor app-state [:player]))

(def player-is-blue (reaction (= @current-player :blue)))
(def end-of-game (reaction (nil? @current-player)))

;; -----------------------------------------
;; GAME LOOP
;; -----------------------------------------

(defn start-game-loop
  "Attempt at creating a game loop to manage the animation and game state"
  []
  (let [is-human (fn [_] @player-is-blue)
        is-ai (fn [_] (and (not @end-of-game) (not @player-is-blue)))
        ai-events (chan 1 (filter is-ai))
        player-events (chan 1 (filter is-human))
        game-events (chan 1)]
    
    (go-loop []
      (alt!
        player-events
        ([coord]
          (swap! app-state play-move coord)
          (put! ai-events :start-ai))
        
        game-events
        ([msg]
          (when (= msg :new-game)
            (reset! app-state (new-game))
            (put! ai-events :start-ai)))
        
        ai-events
        ([msg]
          (condp = msg
            :start-ai (let [auto-move (time (best-move @app-state (:player @app-state)))]
                        (swap! app-state play-move auto-move)
                        (put! ai-events :start-ai))
            )))
      
      (recur))
    {:game-events game-events
     :player-events player-events}))

(defonce game-loop (start-game-loop))


;; -----------------------------------------
;; DISPLAY
;; -----------------------------------------

(defn rect-cell
  [x y color options]
  [:rect.cell
   (merge
     {:x (+ 0.05 x) :width  0.9
      :y (+ 0.05 y) :height 0.9
      :fill color}
     options)
   ])

(defn ^boolean show-help?
  [game x y]
  (and (:help game) (get-move-at game (:player game) [x y])))

(defn empty-cell
  [x y game]
  (rect-cell x y
    (if-not (show-help? game x y) "lightgray" "lightblue")
    {:on-click #(put! (:player-events game-loop) [x y])}
    ))

(defn show-scores
  [scores player]
  {:pre [(scores? scores)]} 
  (for [p players]
    ^{:key p}
    [(if (= player p) :div.score--is-current :div.score)
     (str (str/capitalize (name p)) ": " (get scores p))]
    ))

(defn special-char
  [str-code]
  [:div {:dangerouslySetInnerHTML {:__html str-code}}])

(defn top-panel-button
  [on-click txt]
  [:button.help-button {:on-click on-click} txt])

(defn show-top-panel
  [scores player]
  [:div.scores
   [top-panel-button #(put! (:game-events game-loop) :new-game) (special-char "&#x21bb;")]
   (show-scores scores player)
   [top-panel-button #(swap! app-state update :help not) "?"]])

(defn run-game []
  [:div
   [show-top-panel @scores @current-player]
   (into
     [:svg#board
      {:view-box (str "0 0 " board-width " " board-height)}]
     (for [[x y] all-positions]
       ^{:key [x y]}
       (case (get-in @app-state [:board x y])
         :empty [empty-cell x y @app-state]
         :blue [rect-cell x y "blue"]
         :red [rect-cell x y "red"]
         :green [rect-cell x y "green"]
         :wall [rect-cell x y "gray"])
       ))
   ])

(reagent/render [run-game]
  (js/document.getElementById "app"))
