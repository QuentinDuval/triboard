(ns triboard.view.board
  (:require
    [triboard.logic.board :as board]
    [triboard.view.interactions :as i]
    [triboard.view.utils :as vutils]))


;; ----------------------------------------------------------------------------
;; Private
;; ----------------------------------------------------------------------------

(def relative-size 0.9)
(def border-size (/ (- 1 relative-size) 2))

(defn- rect-cell
  [[x y] player options]
  [:rect.cell
   (merge
     {:class (str "cell--" (name player))
      :x (+ border-size x) :width relative-size
      :y (+ border-size y) :height relative-size}
     options)])

(defn- empty-cell
  [[x y :as position] interactions player]
  (rect-cell position player
    {:on-click #(i/on-player-move interactions x y)}))

(defn- empty-svg-board []
  [:svg.board
   {:view-box (str "0 0 " board/width " " board/height)
    :style {:max-height (str (vutils/max-board-height) "px")}}])


;; ----------------------------------------------------------------------------
;; Public
;; ----------------------------------------------------------------------------

(defn render-board
  [board suggestions interactions]
  (into
    (empty-svg-board)
    (for [[position cell] (board/to-seq board)]
      ^{:key position}
      (if (= :none cell)
        [empty-cell position interactions
         (if (suggestions position) :help :none)]
        [rect-cell position cell])
      )))
