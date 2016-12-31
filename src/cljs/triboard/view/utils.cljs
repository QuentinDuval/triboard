(ns triboard.view.utils
  (:require
    [goog.dom :as dom]
    ))


(defn special-char
  "To use special characters in the DOM"
  [str-code]
  [:div {:dangerouslySetInnerHTML {:__html str-code}}])


(defn max-board-height
  "Reach the DOM to get the maximum height"
  []
  (* (-> (dom/getWindow) dom/getViewportSize .-height) 0.85))
