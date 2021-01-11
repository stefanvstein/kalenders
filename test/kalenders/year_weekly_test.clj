(ns kalenders.year-weekly-test
  (:require [kalenders.year-weekly :as yw]
            [kalenders.time :as time]
            [clojure.test :refer :all ]))

(deftest start-until
  (is (= [(time/of 2021 01 05 21 0 0) (time/of 2021 01 13 20 30 0)]
         (yw/start-until 1 time/tuesday (time/time-part 21 0 0)
                         2 time/wednesday (time/time-part 20 30 0)
                         (time/of 2020 12 01))))
  (is (= [(time/of 2021 01 05 21 0 0) (time/of 2021 01 13 20 30 0)]
         (yw/start-until 1 time/tuesday (time/time-part 21 0 0)
                         2 time/wednesday (time/time-part 20 30 0)
                         (time/of 2021 01 07))))
  
  (is (= [(time/of 2019 12 31 21 0 0) (time/of 2021 01 05 20 30 0)]
         (yw/start-until 1 time/tuesday (time/time-part 21 0 0)
                         1 time/tuesday (time/time-part 20 30 0)
                         (time/of 2021 01 01)))))

