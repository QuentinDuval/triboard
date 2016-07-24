(ns triboard.core
  (:require
    [reagent.core :as reagent :refer [atom]]
    [clojure.string :as str]))

(enable-console-print!)


;; -----------------------------------------
;; GAME PARAMETERS
;; -----------------------------------------

(def board-width 16)
(def board-height 11)
(def init-block-count 12) ;; Init blocks for red, blue, green, gray
(def players [:blue :red :green])
(def directions [[-1 0] [1 0] [0 -1] [0 1] [-1 1] [1 1] [-1 -1] [1 -1]])

;; -----------------------------------------
;; INIT THE BOARD
;; -----------------------------------------

(def empty-board
  (let [column (vec (repeat board-height :empty))]
    (vec (repeat board-width column))))

(def all-positions
  (vec
    (for [x (range board-width)
          y (range board-height)]
      [x y])))

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
  [board point]
  (eduction (keep #(available-cells-by-dir board point %)) directions))

(defn take-empty-cell-move
  "Create a move to take an empty cell" 
  [player pos]
  {:winner player
   :looser :empty
   :taken [pos]})

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
    (time
      (transduce
        (available-moves-xf board)
        #(update-in %1 [(:winner %2) (:move %2)] conj %2)
        {}
        all-positions))
    ))


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
;; GAME STATE
;; -----------------------------------------

(defn new-game []
  (with-available-moves
    {:board (new-board)
     :player (rand-nth players)
     :moves {}
     :help false
     :scores
     {:blue 12
      :red 12
      :green 12}}))

(defonce app-state (atom (new-game)))
(def board (reagent/cursor app-state [:board]))
(def scores (reagent/cursor app-state [:scores]))
(def current-player (reagent/cursor app-state [:player]))

;; -----------------------------------------
;; DISPLAY
;; -----------------------------------------

(defn rect-cell
  [x y color options]
  [:rect
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
    {:on-click #(swap! app-state play-move [x y])}
    ))

(defn show-scores
  [scores player]
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
   (concat
     [[top-panel-button #(reset! app-state (new-game)) (special-char "&#x21bb;")]]
     (show-scores scores player)
     [[top-panel-button #(swap! app-state update :help not) "?"]]
     )])

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
