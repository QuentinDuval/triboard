(ns triboard.logic.scores
  (:require
    [triboard.logic.constants :as cst]
    ))


(def initial-scores
  {:blue cst/init-block-count
   :red cst/init-block-count
   :green cst/init-block-count})
