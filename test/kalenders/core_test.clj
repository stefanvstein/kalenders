(ns kalenders.core-test
  (:require [clojure.pprint :refer [pprint]]
            [clojure.test :refer :all]
            [kalenders.core :as c]
            [kalenders.duration :as duration]
            [kalenders.time :as t])
  (:import java.time.DayOfWeek))

(deftest once-test
  (let [start (t/now)
        duration (duration/of-seconds 2)
        definition (c/once start duration)
        
        before (c/occurrences definition (t/add-seconds start -4))
        after (c/occurrences definition (t/add-seconds start 4))
        during (c/occurrences definition (t/add-seconds start 1))
                                        ;  on-end (occurrences definition (.plusSeconds start 2))
        ] 
    (is (= (seq [[start duration]]) before))
    (is (= (seq [[start duration]]) during))
    (is (empty? after))
    #_(is (empty? on-end))
    ))

(deftest two-once-test
  
    
 (let [start1 (t/now)
       duration (duration/of-seconds 2)
       start2 (t/add-duration start1 (duration/of-seconds 4))
       definition (c/combine [(c/once start1 duration) (c/once start2 duration)])
       
       before (c/occurrences definition (t/add-seconds start1 -4))
       after (c/occurrences definition (t/add-seconds start1 4))
       during (c/occurrences definition (t/add-seconds start1 1))
       ] 
   (is (= (seq [[start1 duration] [start2 duration]]) before))
   (is (= (seq [[start1 duration] [start2 duration]]) during))
   (is (= (seq [[start2 duration]]) after))))

(deftest interleaving-once-test
 (let [start1 (t/now)
       duration (duration/of-seconds 3)
       start2 (t/add-duration start1 (duration/of-seconds 1))
       definition (c/combine [(c/once start1 duration) (c/once start2 duration)])
       
       before (c/occurrences definition (t/add-seconds start1 -4))
       after (c/occurrences definition (t/add-seconds start1 4))
       during (c/occurrences definition (t/add-seconds start1 1))
       duration4 (duration/of-seconds 4)
       ] 
       (is (= (seq [[start1 duration4]]) before))
       (is (= (seq [[start1 duration4]]) during))
       (is (= (seq [[start1 duration4]]) after))))
  
(deftest test-duration
  (let [start1 (t/now)
        start2 (t/add-duration start1 (duration/of-seconds 1))
        dur (duration/between start1 start2)
        duration (duration/of-seconds 1)
        d (duration/between start1 start2)]
    (is (= dur d))))
    
(deftest test-daily
  (let [now (t/now)
        from (t/with now (t/time-part 10 00 00))
        start (t/time-part 11 00 00)
        stop (t/time-part 13 00 00)
        every-day (c/daily start stop)]
    (let [a (take 2 (c/occurrences every-day from))]
      (is (= (seq [[(t/with now start) (duration/of-hours 2)]
                   [(t/add-days (t/with now start) 1) (duration/of-hours 2)] ])
             a)))))

#_(comment
        November 2020      
   sö må ti on to fr lö  
    1  2  3  4  5  6  7  
    8  9 10 11 12 13 14  
   15 16 17 18 19 20 21  
   22 23 24 25 26 27 28  
   29 30  )

(deftest test-weekly
  (let [sunday (t/of 2020 11 15 13 40)
        from (t/with sunday (t/time-part 10 00 00))
        start (t/time-part 11 00 00)
        stop (t/time-part 13 00 00)]
    (let [every-week (c/weekly t/tuesday start
                               t/monday stop)]
      (let [a (take 2 (c/occurrences every-week from))
            d (duration/of-hours (+ 2 (* 24 6 )))
            expected-1 [(t/of 2020 11 10, 11 00 00) d]
            expected-2 [(t/of 2020 11 17, 11 00 00) d]] 
        (is (= (seq [expected-1 expected-2])
               a))))
    (let [every-week (c/weekly t/tuesday start
                               t/friday stop)]
      (let [a (take 2 (c/occurrences every-week from))
            d (duration/of-hours (+ 2 (* 24 3 )))
            expected-1 [(t/of 2020 11 17, 11 00 00) d]
            expected-2 [(t/of 2020 11 24, 11 00 00) d]] 
        (is (= (seq [expected-1 expected-2])
               a))))
    (let [every-week (c/weekly t/friday start
                               t/tuesday stop)]
      (let [a (take 2 (c/occurrences every-week from))
            d (duration/of-hours (+ 2 (* 24 4 )))
            expected-1 [(t/of 2020 11 13, 11 00 00) d]
            expected-2 [(t/of 2020 11 20, 11 00 00) d]] 
        (is (= (seq [expected-1 expected-2])
               a))))))
