(ns triboard.view.board
  (:require
    [triboard.logic.board :as board]
    [triboard.view.callbacks :as cb]
    [triboard.view.utils :as vutils]))


;; ----------------------------------------------------------------------------
;; Private
;; ----------------------------------------------------------------------------

(defn- rect-cell
  [[x y] player options]
  [:rect.cell
   (merge
     {:class (str "cell--" (name player))
      :x (+ 0.05 x) :width 0.9
      :y (+ 0.05 y) :height 0.9}
     options)])

(defn- empty-cell
  [[x y :as position] cb player]
  (rect-cell position player
    {:on-click #(cb/on-player-move cb x y)}))

(defn- empty-svg-board []
  [:svg.board
   {:view-box (str "0 0 " board/board-width " " board/board-height)
    :style {:max-height (str (vutils/max-board-height) "px")}}])


;; ----------------------------------------------------------------------------
;; Public
;; ----------------------------------------------------------------------------

(defn render-board
  [board suggestions cb]
  (into
    (empty-svg-board)
    (for [[position cell] (board/to-iterable board)]
      ^{:key position}
      (if (= :empty cell)
        [empty-cell position cb
         (if (and suggestions (suggestions position))       ;; Avoids crash !
           :help :empty)]
        [rect-cell position cell])
      )))
