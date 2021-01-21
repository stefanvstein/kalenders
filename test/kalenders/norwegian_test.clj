(ns kalenders.norwegian-test
  (:require [kalenders.norwegian :as no]
            [kalenders.core :as kalenders]
            [kalenders.time :as time]
            [kalenders.duration :as duration]
            [clojure.test :as t]))
(t/deftest norwegian?
  (let [d (duration/of 24 0 0)]
    (t/is (= [[(time/of 2014 02 9) d]
              [(time/of 2014 11 9) d]
              [(time/of 2015 02 8) d]
              [(time/of 2015 11 8) d]]
             (take 4 (kalenders/occurrences
                      (kalenders/days #(or (no/morsdag? %)
                                           (no/farsdag? %)))
                      (time/of 2014 1 1)))))

    (t/is (= [[(time/of 2014 4 13) d]
              [(time/of 2014 4 16) d]
              [(time/of 2014 4 19) d]
              [(time/of 2014 12 24) d]
              [(time/of 2014 12 31) d]
              [(time/of 2015 3 29) d]]
             (take 6 (kalenders/occurrences
                      (kalenders/days no/begrenset-arbeidstid?)
                      (time/of 2014 1 1)))))
    (t/is (= [[(time/of 2014 1 1) d]
              [(time/of 2014 4 17) d]
              [(time/of 2014 4 18) d]
              [(time/of 2014 4 20) d]
              [(time/of 2014 4 21) d]
              [(time/of 2014 5 29) d]
              [(time/of 2014 6 8) d]
              [(time/of 2014 6 9) d]
              [(time/of 2014 12 25) d]
              [(time/of 2014 12 26) d]
              [(time/of 2015 1 1) d]]
             (take 11 (kalenders/occurrences
                       (kalenders/days no/helligdag?)
                       (time/of 2014 1 1)))))
    (t/is (= [[(time/of 2016 1 1) d]
              [(time/of 2016 3 24) d]
              [(time/of 2016 3 25) d]
              [(time/of 2016 3 27) d]
              [(time/of 2016 3 28) d]
              [(time/of 2016 5 1) d]
              [(time/of 2016 5 5) d]
              [(time/of 2016 5 15) d]
              [(time/of 2016 5 16) d]
              [(time/of 2016 5 17) d]
              [(time/of 2016 12 25) d]
              [(time/of 2016 12 26) d]
              [(time/of 2017 1 1) d]]
             (take 13 (kalenders/occurrences
                       (kalenders/days no/fridag?)
                       (time/of 2016 1 1)))))))
