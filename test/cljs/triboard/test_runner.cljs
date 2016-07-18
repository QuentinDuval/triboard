(ns triboard.test-runner
  (:require
   [doo.runner :refer-macros [doo-tests]]
   [triboard.core-test]
   [triboard.common-test]))

(enable-console-print!)

(doo-tests 'triboard.core-test
           'triboard.common-test)
