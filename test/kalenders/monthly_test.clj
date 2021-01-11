(ns kalenders.monthly-test
  (:require [kalenders.monthly :as m]
            [kalenders.time :as time] 
            [clojure.test :as t]))


(t/deftest monthly-end-test
  (t/is (= [(time/of 2021 3 30 9 14 10)
           (time/end-of-month (time/of 2021 3))]
          (m/start-until-end-of-month 30 (time/time-part 9 14 10)
                                      (time/of 2021 3 23)))
        "Before")
  (t/is (= [(time/of 2021 3 20 9 14 10)
           (time/end-of-month (time/of 2021 3))]
           (m/start-until-end-of-month 20 (time/time-part 9 14 10)
                                       (time/of 2021 3 23)))
        "During")
   (t/is (= [(time/of 2021 3 30 9 14 10)
           (time/end-of-month (time/of 2021 3))]
           (m/start-until-end-of-month 30 (time/time-part 9 14 10)
                                       (time/of 2021 2 23)))
         "Abandon non valid start"))

(t/deftest monthly-start-test
  (t/is (= [(time/of 2021 3 01)
           (time/of 2021 3 20 9 14 10)]
          (m/begining-of-month-until 20 (time/time-part 9 14 10)
                                     (time/of 2021 3 10))))
  (t/is (= [(time/of 2021 3 01)
           (time/of 2021 3 20 9 14 10)]
          (m/begining-of-month-until 20 (time/time-part 9 14 10)
                                     (time/of 2021 2 23))) "before")
  (t/is (= [(time/of 2021 2 01)
           (time/of 2021 2 28 9 14 10)]
          (m/begining-of-month-until 30 (time/time-part 9 14 10)
                                     (time/of 2021 2 23))) "Shortened"))

(t/deftest monthly-test
  (t/is (= [(time/of 2021 3 04 8)
            (time/of 2021 3 20 9 14 10)]
           (m/start-until 4 (time/time-part 8 0 0)
                          20 (time/time-part 9 14 10)
                          (time/of 2021 3 10)))
        "During")
  (t/is (= [(time/of 2021 3 04 8)
           (time/of 2021 3 20 9 14 10)]
          (m/start-until 4 (time/time-part 8 0 0)
                         20 (time/time-part 9 14 10)
                         (time/of 2021 2 27)))
        "After end")
  (t/is (= [(time/of 2021 3 04 8)
            (time/of 2021 3 20 9 14 10)]
           (m/start-until 4 (time/time-part 8 0 0)
                          20 (time/time-part 9 14 10)
                          (time/of 2021 3 2)))
        "Prior to start")
  (t/is (=[(time/of 2021 3 30 8)
           (time/of 2021 3 31 9 14 10)]
          (m/start-until 30 (time/time-part 8 0 0)
                         31 (time/time-part 9 14 10)
                         (time/of 2021 2 10)))
        "Abandon non valid start")
  (t/is (=[(time/of 2021 2 12 8)
           (time/of 2021 2 28 9 14 10)]
          (m/start-until 12 (time/time-part 8 0 0)
                         31 (time/time-part 9 14 10)
                         (time/of 2021 2 10)))
        "Shortened")
  )

