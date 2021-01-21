(ns kalenders.easter-test
  (:require [kalenders.easter :as easter]
            [clojure.test :as t]
            [kalenders.time :as time]))

(t/deftest easter
  (t/is (= (time/of 2001 04 15)
           (easter/easter 2001)))
   (t/is (= (time/of 1971 04 11)
            (easter/easter 1971)))
   (t/is (= (time/of 2011 04 24)
            (easter/easter 2011)))
   (t/is (= (time/of 2025 04 20)
         (easter/easter 2025))))

