(ns triboard.view.board
  (:require
    [triboard.logic.board :as model]
    [triboard.logic.constants :as cst]
    [triboard.view.callbacks :as cb]
    [triboard.view.utils :as vutils]
    ))


(defn rect-cell
  [x y player options]
  [:rect.cell
   (merge
     {:class (str "cell--" (name player))
      :x (+ 0.05 x) :width 0.9
      :y (+ 0.05 y) :height 0.9}
     options)])

(defn empty-cell
  [x y cb player]
  (rect-cell x y player
    {:on-click #(cb/on-player-move cb x y)}
    ))

(defn render-board
  [board suggestions cb]
  (into
    [:svg.board
     {:view-box (str "0 0 " cst/board-width " " cst/board-height)
      :style {:max-height (str (vutils/max-board-height) "px")}}]
    (for [[[x y] cell] (model/to-iterable board)]
      ^{:key [x y]}
      (if (= :empty cell)
        [empty-cell x y cb (if (suggestions [x y]) :help :empty)]
        [rect-cell x y cell])
      )))
