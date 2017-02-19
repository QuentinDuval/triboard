(ns triboard.logic.player
  (:require
    [cljs.spec :as s :include-macros true]
    ))


(def players [:blue :red :green])

(def player? (set players))
(def cell? (conj player? :wall :empty))

(s/def ::player player?)
(s/def ::cell cell?)
