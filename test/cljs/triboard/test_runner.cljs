(ns ^:figwheel-always triboard.test-runner
  (:require
    [cljs.test :refer [run-tests]]
    ))

(run-tests 'triboard.core-test)
