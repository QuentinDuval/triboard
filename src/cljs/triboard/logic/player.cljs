(ns triboard.logic.player
  (:require
    [cljs.spec :as s :include-macros true]
    ))


(def all [:blue :red :green])

(def player? (set all))
(def cell? (conj player? :wall :none))
(def is-ai? #{:red :green})

(s/def ::player player?)
(s/def ::cell cell?)
(s/def ::convertible-owner (conj player? :none))

(defn next-one
  [player]
  (case player
    :blue :red
    :red :green
    :green :blue
    nil nil))                                               ;; TODO - remove

(defn next-three
  [player]
  (take 3 (rest (iterate next-one player))))
