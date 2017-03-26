(ns ^:figwheel-always triboard.test-runner
  (:require
    [cljs.test :refer [run-tests]]
    [triboard.algo-test]
    [triboard.core-test]
    ))

(run-tests
  'triboard.algo-test
  'triboard.core-test)
