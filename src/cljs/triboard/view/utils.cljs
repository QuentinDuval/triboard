(ns triboard.view.utils)


(defn special-char
  "To use special characters in the DOM"
  [str-code]
  [:div {:dangerouslySetInnerHTML {:__html str-code}}])
