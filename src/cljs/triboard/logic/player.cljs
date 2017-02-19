(ns triboard.logic.player
  (:require
    [cljs.spec :as s :include-macros true]
    ))


(def all [:blue :red :green])

(def player? (set all))
(def cell? (conj player? :wall :empty))

(s/def ::player player?)
(s/def ::cell cell?)
(s/def ::playable-cell (conj player? :empty))

(defn next-one
  [player]
  (case player
    :blue :red
    :red :green
    :green :blue))

(defn next-three
  [player]
  (take 3 (rest (iterate next-one player))))
