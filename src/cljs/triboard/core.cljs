(ns triboard.core
  (:require [reagent.core :as reagent :refer [atom]]))

(enable-console-print!)


;; -----------------------------------------
;; GAME PARAMETERS
;; -----------------------------------------

(def board-width 16)
(def board-height 11)
(def init-block-count 12) ;; Init blocks for red, blue, green, gray
(def players [:blue :red :green])


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
  (reduce (fn [r [color [x y]]]
            (prn [x y])
            (assoc-in r [x y] color))
    empty-board (init-positions)))

(defn new-game []
  {:board (new-board)
   :turn 0
   ;; From the turn you can deduce the number of turn left
   ;; There is an invariant holding: total score = 12 * 3 + turn
   ;; TODO - Enhance the display to show the available moves (compute them once)
   :scores
   {:blue 12
    :red 12
    :green 12}})


;; -----------------------------------------
;; TAKING CELLS
;; -----------------------------------------

(defn range-single-coord
  "Creates a range along one coordinate"
  [x dx max-val]
  (let [end-range (if (neg? dx) -1 max-val)]
    (range x end-range dx)))

(defn range-coord
  "Give all the coordinate in the provided direction"
  [[xi yi] [dx dy]]
  (map vector
    (range-single-coord xi dx board-width)
    (range-single-coord yi dy board-height)))

(defn is-cell-owned?
  "Indicates whether a cell is owned by any player"
  [cell]
  (and (not= cell :empty) (not= cell :wall)))

(defn range-cells ;; TODO - Use the concept of sentinel for the walls?
  "Give all the cells in the provided direction - until you reach an empty / blocked / wall cell"
  [board [x y] [dx dy]]
  (eduction
    (comp
      (map (juxt #(get-in board %) identity))
      (take-while (comp is-cell-owned? first))
      (partition-by first)
      (map (juxt #(-> % first first) #(mapv second %))))
    (range-coord [x y] [dx dy])))

(defn available-cells-by-dir
  "Indicates the convertible cells for the provided player - when clicking at [x y]
   Returns an object of the form
		{:winner :blue,
		 :looser :red,
		 :taken [[8 5] [9 5]]}"
  [board [x y] [dx dy]]
  (let [[head tail :as cells] (take 2 (range-cells board [(+ x dx) (+ y dy)] [dx dy]))]
    (when (= 2 (count cells))
      {:winner (first tail)  ;; Who wins the cells
       :looser (first head)  ;; Who looses the cells
       :taken (second head)} ;; Which cells are taken
      )))

(defn available-moves-at
  "Provides the list of moves that can be done from a cell
   Return an object of the form:
		{:blue
		 [{:winner :blue,
		   :looser :red,
		   :taken [[8 5] [9 5]]}]}"
  [board [x y]]
  (group-by :winner
    (keep #(available-cells-by-dir board [x y] %)
     [[-1 0] [1 0] [0 -1] [0 1]
      [-1 1] [1 1] [-1 -1] [1 -1]]
     )))


;; -----------------------------------------
;; ON PLAYER MOVE
;; -----------------------------------------

(defn- next-player
  [player]
  (case player
    :blue :red
    :red :green
    :green :blue))

(defn- apply-move
  [board {:keys [winner taken] :as move}]
  (reduce #(assoc-in %1 %2 winner) board taken))

(defn play-move
  "On player playing the move [x y]" ;; TODO - Handle the scores
  [board [x y] player]
  (let [moves (get (available-moves-at board [x y]) player nil)]
    (if moves
      (reduce apply-move (assoc-in board [x y] player) moves)
      board)))


;; -----------------------------------------
;; GAME STATE
;; -----------------------------------------

(defonce app-state (atom (new-game)))
(def board (reagent/cursor app-state [:board]))
(def scores (reagent/cursor app-state [:scores]))


;; -----------------------------------------
;; DISPLAY
;; -----------------------------------------

(defn rect-cell
  [x y color options]
  [:rect
   (merge
     {:width 0.9
      :height 0.9
      :x (+ 0.05 x)
      :y (+ 0.05 y)
      :fill color}
     options)
   ])

(defn empty-cell
  [x y]
  (rect-cell x y "lightgray"
    {:on-click
     (fn []
       (prn [x y])
       (swap! board play-move [x y] :blue))
     }))

(defn greeting []
  [:h1 "Triboard"
   [:div.scores
    (for [[k v] @scores]
      [:div.score (str (name k) ":" v)])]
   (into
     [:svg#board
      {:view-box (str "0 0 " board-width " " board-height)}]
     (for [[x y] all-positions]
       ^{:key [x y]}
       (case (get-in @app-state [:board x y])
         :empty  [empty-cell x y]
         :blue [rect-cell x y "blue"]
         :red [rect-cell x y "red"]
         :green [rect-cell x y "green"]
         :wall [rect-cell x y "gray"])
       ))
   ])

(reagent/render [greeting]
  (js/document.getElementById "app"))
