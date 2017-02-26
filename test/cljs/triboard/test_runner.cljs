(ns ^:figwheel-always triboard.test-runner
  (:require
    [cljs.test]
    ))

;;(cljs.test/run-all-tests #"triboard.*test")

(cljs.test/run-tests
  'triboard.core-test)
