(ns triboard.utils.async)

(defmacro run-async
  [computation]
  `(run-async-fn (fn [] ~computation)))
