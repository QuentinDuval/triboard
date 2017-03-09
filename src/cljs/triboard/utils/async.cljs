(ns triboard.utils.async
  (:require [cljs.core.async :refer [chan >!]])
  (:require-macros [cljs.core.async.macros :refer [go]]))

(defn run-async-fn
  [computation]
  (let [out-chan (chan 1)]
    (go (>! out-chan (computation)))
    out-chan))
