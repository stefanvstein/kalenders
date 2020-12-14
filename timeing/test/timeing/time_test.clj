(ns timeing.time-test
  (:require [timeing.time :as t]
            [clojure.test :refer :all]
            [clojure.string :as string]
            [timeing.time :as time]
            [timeing.duration :as duration]))

(deftest epoch-now
  (is (t/ordered? [t/epoch (t/now) (t/now)])))

(deftest test-with-year
  (is (= [2019 02 28]
         (t/year-month-day (t/with-year (t/of 2020 02 28 0 0 0)
                             2019))))
  )
(deftest of-y-m-d-h-m-s-n
  (let [time (t/add-nanos (t/of 2020 1 2 3 4 5)
                          6000007)]
    (is (= [2020 1 2] (t/year-month-day time)))
    (is (= [3 4 5] (t/hour-minute-second time)))
    (is (= [3 4 5 6] (t/hour-minute-second-millis time)))
    (is (= [3 4 5 6000007] (t/hour-minute-second-nanos time)))
    (t/of 2020 12)))

(deftest of
  (let [a (t/of 2020 1 1 0 0 0)]
    (is (= a (t/of 2020 1 1 0 0)))
    (is (= a (t/of 2020 1 1 0)))
    (is (= a (t/of 2020 1 1)))
    (is (= a (t/of 2020 1)))
    (is (= a (t/of 2020)))))

(deftest ordering
  (let [a (t/of 2020 1 2 3 4 5)
        b (t/of 2020 1 2 3 4 6)
        b' (t/of 2020 1 2 3 4 6)
        c (t/of 2020 1 2 3 4 7)]
    (is (t/ordered? a b b' c))
    (is (not (t/ordered? c b b' a)))
    (is (t/increasing? a b c))
    (is (not (t/increasing? a b b' c)))
    (is (not (t/increasing? c b a)))
    (is (t/decreasing? c b a))
    (is (not (t/decreasing? c b b' a)))
    (is (not (t/decreasing? a b c)))
    (is (t/reversed? c b b' a))
    (is (not (t/reversed? a b c)))
    (is (t/ordered?))
    (is (t/ordered? a))
    (is (t/ordered? a b))
    (is (not (t/ordered? b a)))
    (is (t/ordered? a a))
    (is (t/increasing? ))
    (is (t/increasing? a))
    (is (t/increasing? a b))
    (is (not (t/increasing? b a)))
    (is (not (t/increasing? a a)))
    (is (t/decreasing? ))
    (is (t/decreasing? a))
    (is (t/decreasing? b a))
    (is (not (t/decreasing? a b)))
    (is (not (t/decreasing? a a)))
    (is (t/reversed? ))
    (is (t/reversed? a))
    (is (t/reversed? b a))
    (is (not (t/reversed? a b)))
    (is (t/reversed? a a))))

(deftest adding
  (let [a (t/of 2020 1 2 3 4 5)]
    (is (= (t/of 2020 1 3 4 4 5) (t/add-hours a 25)))
    (is (= (t/of 2020 1 1 2 4 5) (t/add-hours a -25)))
    (is (= (t/of 2020 1 2 4 7 5) (t/add-minutes a 63)))
    (is (= (t/of 2020 1 2 2 1 5) (t/add-minutes a -63)))
    (is (= (t/of 2020 1 2 3 5 8) (t/add-seconds a 63)))
    (is (= (t/of 2020 1 2 3 3 2) (t/add-seconds a -63)))
    (is (= (t/of 2020 2 1 3 4 5) (t/add-days a 30)))
    (is (= (t/of 2019 12 3 3 4 5) (t/add-days a -30)))
    (is (= (t/of 2030 1 2 3 4 5) (t/add-years a 10)))
    (is (= (t/with-millis (t/of 2020 1 2 3 4 5) 10) (t/add-millis a 10)))
    (is (= (t/with-nano (t/of 2020 1 2 3 4 5) 10) (t/add-nanos a 10)))
    (is (= (t/with-millis (t/of 2020 1 2 3 4 5) 10) (t/add-nanos a 10000000)))
    (is (= (t/of 2020 1 2 4 5 6 ) (t/add-duration a (duration/of 1 1 1))))
    (is (= (-> (t/of 2020 1 2 4 5 6)
               (t/with-nano 7000008))
           (t/add-duration a (-> (duration/of 1 1 1)
                                 (duration/add-millis 7)
                                 (duration/add-nanos 8)))))))

(deftest with-year
  (is (= (t/of 2025 1 2 3 4 5)
         (t/with-year (t/of 2020 1 2 3 4 5) 2025)))
  (try (t/with-year (t/of 1975 4 6 2 0 1) 1980)
       (is (not "hit"))
       (catch clojure.lang.ExceptionInfo e
         (is (= {:value 1980,
                 :conflict :year}
                (select-keys (ex-data e) [:value :conflict])))
         (is (string/includes? (ex-message e) "gap"))))
  (try (t/with-year (t/of 2004 2 29 2 0 1) 2005)
       (is (not "hit"))
       (catch clojure.lang.ExceptionInfo e
         (is (= {:value 2005,
                 :conflict :year}
                (select-keys (ex-data e) [:value :conflict])))
         (is (string/includes? (ex-message e) "valid day")))))

(deftest with-month
  (is (= (t/of 2020 10 2 3 4 5)
         (t/with-month (t/of 2020 1 2 3 4 5) 10)))
  (try (t/with-month (t/of 2003 1 29 2 0 1) 2)
       (is (not "hit"))
       (catch clojure.lang.ExceptionInfo e
         (is (=  {:value 2,
                  :conflict :month}
                 (select-keys (ex-data e) [:value :conflict])))
         (is (string/includes? (ex-message e) "valid day"))))
  (try (t/with-month (t/of 1980 3 6 2 0 1) 4)
       (is (not "hit"))
       (catch clojure.lang.ExceptionInfo e
         (is (= {:value 4,
                 :conflict :month}
                (select-keys (ex-data e) [:value :conflict])))
         (is (string/includes? (ex-message e) "gap")))))

(deftest with-day
  (is (= (t/of 2020 1 31 3 4 5)
         (t/with-day (t/of 2020 1 2 3 4 5) 31)))
  (try (t/with-day (t/of 2003 2 28 2 0 1) 29)
       (is (not "hit"))
       (catch clojure.lang.ExceptionInfo e
         (is (= {:value 29,
                 :conflict :day}
                (select-keys (ex-data e) [:value :conflict])))
         (is (string/includes? (ex-message e) "valid day"))))
  (try (t/with-day (t/of 1980 4 5 2 0 1) 6)
       (is (not "hit"))
       (catch clojure.lang.ExceptionInfo e
         (is (= {:value 6,
                 :conflict :day}
                (select-keys (ex-data e) [:value :conflict])))
         (is (string/includes? (ex-message e) "gap")))))

(deftest with-hour
  (is (= (t/of 2020 1 2 23 4 5)
         (t/with-hour (t/of 2020 1 2 3 4 5) 23)))
  (try (t/with-hour (t/of 2020 1 2 3 4 5) 32)
       (is (not "hit"))
       (catch clojure.lang.ExceptionInfo e
         (is (= {:value 32,
                 :conflict :hour}
                (select-keys (ex-data e) [:value :conflict])))
         (is (string/includes? (ex-message e) "valid hour"))))
  (try (t/with-hour (t/of 1980 4 6 1 0 1) 2)
       (is (not "hit"))
       (catch clojure.lang.ExceptionInfo e
         (is (= {:value 2,
                 :conflict :hour}
                (select-keys (ex-data e) [:value :conflict])))
         (is (string/includes? (ex-message e) "gap")))))


(deftest with-minute
  (is (= (t/of 2020 1 2 3 13 5)
         (t/with-minute (t/of 2020 1 2 3 4 5) 13)))
  (try (t/with-minute (t/of 2020 1 2 3 4 5) 61)
       (is (not "hit"))
       (catch clojure.lang.ExceptionInfo e
         (is (= {:value 61,
                 :conflict :minute}
                (select-keys (ex-data e) [:value :conflict])))
         (is (string/includes? (ex-message e) "valid minute")))))

(deftest with-second
  (is (= (t/of 2020 1 2 3 4 13)
         (t/with-second (t/of 2020 1 2 3 4 5) 13)))
  (try (t/with-second (t/of 2020 1 2 3 4 5) 61)
       (is (not "hit"))
       (catch clojure.lang.ExceptionInfo e
         (is (= {:value 61,
                 :conflict :second}
                (select-keys (ex-data e) [:value :conflict])))
         (is (string/includes? (ex-message e) "valid second")))))

(deftest with-millis
  (is (= (t/add-millis (t/of 2020 1 2 3 4 5) 13)
         (t/with-millis (t/of 2020 1 2 3 4 5) 13)))
  (try (t/with-millis (t/of 2020 1 2 3 4 5) 1000)
       (is (not "hit"))
       (catch clojure.lang.ExceptionInfo e
         (is (= {:value 1000,
                 :conflict :millis}
                (select-keys (ex-data e) [:value :conflict])))
         (is (string/includes? (ex-message e) "valid millis")))))

(deftest with-nanos
  (is (= (t/add-nanos (t/of 2020 1 2 3 4 5) 13)
         (t/with-nano (t/of 2020 1 2 3 4 5) 13)))
  (is (= (t/add-millis (t/of 2020 1 2 3 4 5) 900)
         (t/with-nano (t/of 2020 1 2 3 4 5) (* 900 1000 1000))))
  (is (string/includes?  (str (t/with-nano
                                (t/of 2020 1 2 3 4 5)
                                (* 900 1000 1000)))
                         "03:04:05.9"))
  (try (t/with-nano (t/of 2020 1 2 3 4 5) (* 1 1000 1000 1000))
       (is (not "hit"))
       (catch clojure.lang.ExceptionInfo e
         (is (= {:value (* 1 1000 1000 1000),
                 :conflict :nano}
                (select-keys (ex-data e) [:value :conflict])))
         (is (string/includes? (ex-message e) "valid nanos")))))

(deftest with-time-part
  (is (= (t/of 2020 10 01 13 44 55)
         (t/with-time-part
           (t/of 2020 10 01 20 30 00)
           (t/time-part 13 44 55))))
  (let [[from to] (first (t/transitions (t/of 2020 01 01)))
        hole (-> (t/add-minutes (t/time-part-of from)
                                15)
                 (t/just-after))]
    (try (t/with-time-part from hole)
         (catch clojure.lang.ExceptionInfo e
           (is (= {:value hole
                   :conflict :time-part}
                  (select-keys (ex-data e) [:value :conflict])))
           (is (string/includes? (ex-message e) "valid" ))))))


(deftest time-part
  (is (= (t/time-part 3 4 5)
         (t/time-part-of (t/of 2020 1 2 3 4 5))))
  (is (= (t/time-part 0 4 5)
         (t/time-part-of (t/of 2020 1 2 0 4 5))))
  (is (= (t/time-part 0 0 5)
         (t/time-part-of (t/of 2020 1 2 0 0 5))))
  (is (= (t/time-part 0 0 0)
         (t/time-part-of (t/of 2020 1 2 0 0 0))))
  (is (= (t/time-part 0 0 59)
         (t/time-part-of (t/of 2020 1 2 0 0 59))))
  (is (= (t/time-part 0 59 0)
         (t/time-part-of (t/of 2020 1 2 0 59 0))))
  (is (= (t/time-part 0 59)
         (t/time-part-of (t/of 2020 1 2 0 59 0))))
  (is (= (t/time-part 23 0 0)
         (t/time-part-of (t/of 2020 1 2 23 0 0))))
  (try (t/time-part 24 0 0)
       (is (not "hit"))
       (catch clojure.lang.ExceptionInfo e
         (is (= {:value 24,
                 :conflict :hour}
                (select-keys (ex-data e) [:value :conflict])))
         (is (and (string/includes? (ex-message e) "valid")
                  (string/includes? (ex-message e) "hour")))))
  (try (t/time-part -1 0 0)
       (is (not "hit"))
       (catch clojure.lang.ExceptionInfo e
         (is (= {:value -1,
                 :conflict :hour}
                (select-keys (ex-data e) [:value :conflict])))
         (is (and (string/includes? (ex-message e) "valid")
                  (string/includes? (ex-message e) "hour")))))
  (try (t/time-part 0 -1 0)
       (is (not "hit"))
       (catch clojure.lang.ExceptionInfo e
         (is (= {:value -1,
                 :conflict :minute}
                (select-keys (ex-data e) [:value :conflict])))
         (is (and (string/includes? (ex-message e) "valid")
                  (string/includes? (ex-message e) "minute")))))
  (try (t/time-part 0 60 0)
       (is (not "hit"))
       (catch clojure.lang.ExceptionInfo e
         (is (= {:value 60,
                 :conflict :minute}
                (select-keys (ex-data e) [:value :conflict])))
         (is (and (string/includes? (ex-message e) "valid")
                  (string/includes? (ex-message e) "minute")))))
  (try (t/time-part 0 0 60)
       (is (not "hit"))
       (catch clojure.lang.ExceptionInfo e
         (is (= {:value 60,
                 :conflict :second}
                (select-keys (ex-data e) [:value :conflict])))
         (is (and (string/includes? (ex-message e) "valid")
                  (string/includes? (ex-message e) "second")))))
  (try (t/time-part 0 0 -1)
       (is (not "hit"))
       (catch clojure.lang.ExceptionInfo e
         (is (= {:value -1,
                 :conflict :second}
                (select-keys (ex-data e) [:value :conflict])))
         (is (and (string/includes? (ex-message e) "valid")
                  (string/includes? (ex-message e) "second"))))))


(deftest date-part
  (is (= (t/date-part 2020 1 2)
         (t/date-part-of (t/of 2020 1 2 3 4 5))))
  (is (= (t/date-part 2020 1 1)
         (t/date-part-of (t/of 2020 1 1 0 4 5))))
  (is (= (t/date-part 2020 1 31)
         (t/date-part-of (t/of 2020 1 31 0 0 5))))
  (is (= (t/date-part 2020 1 2)
         (t/date-part-of (t/of 2020 1 2 0 0 0))))
  
  
  (try (t/date-part 2020 0 1)
       (is (not "hit"))
       (catch clojure.lang.ExceptionInfo e
         (is (= {:value 0,
                 :conflict :month}
                (select-keys (ex-data e) [:value :conflict])))
         (is (and (string/includes? (ex-message e) "valid")
                  (string/includes? (ex-message e) "month")))))
  (try (t/date-part 2020 13  0)
       (is (not "hit"))
       (catch clojure.lang.ExceptionInfo e
         (is (= {:value 13,
                 :conflict :month}
                (select-keys (ex-data e) [:value :conflict])))
         (is (and (string/includes? (ex-message e) "valid")
                  (string/includes? (ex-message e) "month")))))
  (try (t/date-part 2010 1 60)
       (is (not "hit"))
       (catch clojure.lang.ExceptionInfo e
         (is (= {:value 60,
                 :conflict :day}
                (select-keys (ex-data e) [:value :conflict])))
         (is (and (string/includes? (ex-message e) "valid")
                  (string/includes? (ex-message e) "day")))))
  (try (t/date-part 2020 1 0)
       (is (not "hit"))
       (catch clojure.lang.ExceptionInfo e
         (is (= {:value 0,
                 :conflict :day}
                (select-keys (ex-data e) [:value :conflict])))
         (is (and (string/includes? (ex-message e) "valid")
                  (string/includes? (ex-message e) "day"))))))


(deftest truncate
  (let [a (t/of 2121 10 21 13 14 15)
        b (t/with-nano (t/of 2121 10 21 13 14 15) 1002003)]
    (is (= (t/of 2120) (t/truncate a :decade)))
    (is (= (t/of 2100) (t/truncate a :century)))
    (is (= (t/of 2000) (t/truncate a :millenia)))
    (is (= (t/of 2121) (t/truncate a :year)))
    (is (= (t/of 2121 10) (t/truncate a :month)))
    (is (= (t/of 2121 10 21) (t/truncate a :day)))
    (is (= (t/of 2121 10 21 13) (t/truncate a :hour)))
    (is (= (t/of 2121 10 21 13 14) (t/truncate a :minute)))
    (is (= (t/of 2121 10 21 13 14 15) (t/truncate a :second)))
    (is (= (t/of 2121 10 21 13 14 15) (t/truncate a :millis)))
    (is (= (t/with-millis (t/of 2121 10 21 13 14 15) 1) (t/truncate b :millis)))
    (is (= (t/with-nano (t/of 2121 10 21 13 14 15) 1002000) (t/truncate b :micros)))
    (try (t/truncate b :birds)
      (is (not "hit"))
      (catch clojure.lang.ExceptionInfo e
        (is (and (string/includes? (ex-message e) ":birds")))))))

(deftest overlapp
  (let [dump (fn [q text ] (println (str text q)) q)
        [from to] (first (t/transitions (t/of 2020 06 01)))]
    (is (= (t/time-part-of to)
           (-> (t/just-after from)
               (t/with-later-at-overlap)
               (t/time-part-of))))
    (is (= (-> from
               (t/just-after)
               (t/time-part-of))
           (-> to
               (t/with-earlier-at-overlap )
               (t/time-part-of))))))

