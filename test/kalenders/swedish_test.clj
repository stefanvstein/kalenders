(ns kalenders.swedish-test
  (:require [kalenders.swedish :as swe]
            [kalenders.core :as kalenders]
            [kalenders.time :as time]
            [kalenders.duration :as duration]
            [clojure.test :as t]))

(t/deftest swedish?
  (let [d (duration/of 24 0 0)]
    (t/is (= [[(time/of 2001 01 06) d]
              [(time/of 2001 04 12) d]
              [(time/of 2001 04 30) d]
              [(time/of 2001 11 02) d]]
             (take 4 (kalenders/occurrences
                      (kalenders/days swe/halvdag?)
                      (time/of 2001 1 1)))))

    (t/is (= [[(time/of 2001 01 01) d]
              [(time/of 2001 01 06) d]
              [(time/of 2001 01 07) d]
              [(time/of 2001 01 14) d]
              [(time/of 2001 01 21) d]
              [(time/of 2001 01 28) d]
              [(time/of 2001 02 4) d]
              [(time/of 2001 02 11) d]
              [(time/of 2001 02 18) d]
              [(time/of 2001 02 25) d]
              [(time/of 2001 03 4) d]]
             (take 11 (kalenders/occurrences
                       (kalenders/days swe/söndag-enligt-semesterlagen?)
                       (time/of 2001 1 1)))))
    (t/is (= [[(time/of 2001 04 01) d]
              [(time/of 2001 04 8) d]
              [(time/of 2001 04 13) d]
              [(time/of 2001 04 15) d]
              [(time/of 2001 04 16) d]
              [(time/of 2001 04 22) d]
              [(time/of 2001 04 29) d]
              [(time/of 2001 05 1) d]
              [(time/of 2001 05 6) d]
              [(time/of 2001 05 13) d]
              [(time/of 2001 05 20) d]
              [(time/of 2001 05 24) d]
              [(time/of 2001 05 27) d]
              [(time/of 2001 06 3) d]
              [(time/of 2001 06 4) d]
              [(time/of 2001 06 10) d]]
             (take 16 (kalenders/occurrences
                       (kalenders/days swe/söndag-enligt-semesterlagen?)
                       (time/of 2001 4 1)))))
    (t/is (= [[(time/of 2007 05 27) d]
              [(time/of 2007 06 3) d]
              [(time/of 2007 06 6) d]
              [(time/of 2007 06 10) d]
              [(time/of 2007 06 17) d]
              [(time/of 2007 06 22) d]
              [(time/of 2007 06 23) d]
              [(time/of 2007 06 24) d]]
             (take 8 (kalenders/occurrences
                      (kalenders/days swe/söndag-enligt-semesterlagen?)
                      (time/of 2007 5 21)))))

    (t/is (= [[(time/of 2001 04 14) d]
              [(time/of 2001 06 02) d]
              [(time/of 2001 06 22) d]
              [(time/of 2001 12 24) d]
              [(time/of 2001 12 31) d]
              [(time/of 2002 03 30) d]
              [(time/of 2002 05 18) d]
              [(time/of 2002 06 21) d]]
             (take 8 (kalenders/occurrences
                      (kalenders/days swe/helgdagsafton?)
                      (time/of 2001 1 1)))))
    (t/is (= [[(time/of 2001 11 03) d]
              [(time/of 2001 11 4) d]
              [(time/of 2001 11 11) d]
              [(time/of 2001 11 18) d]
              [(time/of 2001 11 25) d]
              [(time/of 2001 12 2) d]
              [(time/of 2001 12 9) d]
              [(time/of 2001 12 16) d]
              [(time/of 2001 12 23) d]
              [(time/of 2001 12 25) d]
              [(time/of 2001 12 26) d]
              [(time/of 2001 12 30) d]]
             (take 12 (kalenders/occurrences
                       (kalenders/days swe/helgdag?)
                       (time/of 2001 11 1)))))
    (t/is (= [[(time/of 1939 5 1) d]
              [(time/of 1940 5 1) d]]
             (take 2 (kalenders/occurrences
                      (kalenders/days swe/borgerlig-helgdag?)
                      (time/of 1938 1)))))
    (t/is (= [[(time/of 2004 5 1) d]
              [(time/of 2005 5 1) d]
              [(time/of 2005 6 6) d]
              [(time/of 2006 5 1) d]
              [(time/of 2006 6 6) d]]
             (take 5 (kalenders/occurrences
                      (kalenders/days swe/borgerlig-helgdag?)
                      (time/of 2004 1)))))))



