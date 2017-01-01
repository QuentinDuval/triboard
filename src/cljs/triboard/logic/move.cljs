(ns triboard.logic.move
  (:require
    [triboard.logic.board :as board]
    [triboard.logic.constants :as cst]
    ))


;; -----------------------------------------
;; Public Types
;; -----------------------------------------

(defn move?
  "A move is the result of player a cell at a given coordinate"
  [m]
  (and
    (board/coord? (:move m))                                ;; TODO - rename
    (every? board/coord? (:taken m))
    (cst/player? (:winner m))
    (cst/cell? (:looser m))))


;; -----------------------------------------
;; Public API
;; -----------------------------------------



