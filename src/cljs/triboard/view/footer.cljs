(ns triboard.view.footer)

(def github-link
  "https://github.com/QuentinDuval/triboard")

(defn credits
  []
  [:div.credits
   [:a.github-link {:href github-link} github-link]])
