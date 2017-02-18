(ns triboard.view.utils
  (:require
    [goog.dom :as dom]
    ))


(defn- special-char
  "To use special characters in the DOM"
  [str-code]
  [:div {:dangerouslySetInnerHTML {:__html str-code}}])

(def star (special-char "&#9733;"))
(def circle-arrow (special-char "&#x21bb;"))
(def back-arrow (special-char "&larr;"))

(defn max-board-height
  "Reach the DOM to get the maximum height"
  []
  (* (-> (dom/getWindow) dom/getViewportSize .-height) 0.85))
