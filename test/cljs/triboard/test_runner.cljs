(ns ^:figwheel-always triboard.test-runner
  (:require
    [cljs.test :refer-macros [run-all-tests]]
    ))

(run-all-tests #"triboard.*test")
