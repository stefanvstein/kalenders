(ns kalenders.time-test
  (:require [clojure.string :as string]
            [clojure.test :refer :all]
            [kalenders.duration :as duration]
            [kalenders.time :as time]))

(deftest epoch-now
  (is (time/ordered? [time/epoch (time/now) (time/now)])))

(deftest test-with-year
  (is (= [2019 02 28]
         (time/year-month-day (time/with-year (time/of 2020 02 28 0 0 0)
                             2019))))
  )
(deftest of-y-m-d-h-m-s-n
  (let [time (time/add-nanos (time/of 2020 1 2 3 4 5)
                          6000007)]
    (is (= [2020 1 2] (time/year-month-day time)))
    (is (= [3 4 5] (time/hour-minute-second time)))
    (is (= [3 4 5 6] (time/hour-minute-second-millis time)))
    (is (= [3 4 5 6000007] (time/hour-minute-second-nanos time)))
    (time/of 2020 12)))

(deftest of
  (let [a (time/of 2020 1 1 0 0 0)]
    (is (= a (time/of 2020 1 1 0 0)))
    (is (= a (time/of 2020 1 1 0)))
    (is (= a (time/of 2020 1 1)))
    (is (= a (time/of 2020 1)))
    (is (= a (time/of 2020)))))

(deftest ordering
  (let [a (time/of 2020 1 2 3 4 5)
        b (time/of 2020 1 2 3 4 6)
        b' (time/of 2020 1 2 3 4 6)
        c (time/of 2020 1 2 3 4 7)]
    (is (time/ordered? a b b' c))
    (is (not (time/ordered? c b b' a)))
    (is (time/increasing? a b c))
    (is (not (time/increasing? a b b' c)))
    (is (not (time/increasing? c b a)))
    (is (time/decreasing? c b a))
    (is (not (time/decreasing? c b b' a)))
    (is (not (time/decreasing? a b c)))
    (is (time/reversed? c b b' a))
    (is (not (time/reversed? a b c)))
    (is (time/ordered?))
    (is (time/ordered? a))
    (is (time/ordered? a b))
    (is (not (time/ordered? b a)))
    (is (time/ordered? a a))
    (is (time/increasing? ))
    (is (time/increasing? a))
    (is (time/increasing? a b))
    (is (not (time/increasing? b a)))
    (is (not (time/increasing? a a)))
    (is (time/decreasing? ))
    (is (time/decreasing? a))
    (is (time/decreasing? b a))
    (is (not (time/decreasing? a b)))
    (is (not (time/decreasing? a a)))
    (is (time/reversed? ))
    (is (time/reversed? a))
    (is (time/reversed? b a))
    (is (not (time/reversed? a b)))
    (is (time/reversed? a a))))

(deftest adding
  (let [a (time/of 2020 1 2 3 4 5)]
    (is (= (time/of 2020 1 3 4 4 5) (time/add-hours a 25)))
    (is (= (time/of 2020 1 1 2 4 5) (time/add-hours a -25)))
    (is (= (time/of 2020 1 2 4 7 5) (time/add-minutes a 63)))
    (is (= (time/of 2020 1 2 2 1 5) (time/add-minutes a -63)))
    (is (= (time/of 2020 1 2 3 5 8) (time/add-seconds a 63)))
    (is (= (time/of 2020 1 2 3 3 2) (time/add-seconds a -63)))
    (is (= (time/of 2020 2 1 3 4 5) (time/add-days a 30)))
    (is (= (time/of 2019 12 3 3 4 5) (time/add-days a -30)))
    (is (= (time/of 2030 1 2 3 4 5) (time/add-years a 10)))
    (is (= (time/with-millis (time/of 2020 1 2 3 4 5) 10) (time/add-millis a 10)))
    (is (= (time/with-nano (time/of 2020 1 2 3 4 5) 10) (time/add-nanos a 10)))
    (is (= (time/with-millis (time/of 2020 1 2 3 4 5) 10) (time/add-nanos a 10000000)))
    (is (= (time/of 2020 1 2 4 5 6 ) (time/add-duration a (duration/of 1 1 1))))
    (is (= (-> (time/of 2020 1 2 4 5 6)
               (time/with-nano 7000008))
           (time/add-duration a (-> (duration/of 1 1 1)
                                 (duration/add-millis 7)
                                 (duration/add-nanos 8)))))))


(deftest with-year
  (is (= (time/of 2025 1 2 3 4 5)
         (time/with-year (time/of 2020 1 2 3 4 5) 2025)))
  
  (try (time/with-year (time/with-time-zone
                         (time/of 1975 4 6 2 0 1)
                         (time/find-time-zone "stockholm")) 1980)
       (is (not "hit"))
       (catch clojure.lang.ExceptionInfo e
         (is (= {:value 1980,
                 :conflict :year}
                (select-keys (ex-data e) [:value :conflict])))
         (is (string/includes? (ex-message e) "gap"))))
  (try (time/with-year (time/of 2004 2 29 2 0 1) 2005)
       (is (not "hit"))
       (catch clojure.lang.ExceptionInfo e
         (is (= {:value 2005,
                 :conflict :year}
                (select-keys (ex-data e) [:value :conflict])))
         (is (string/includes? (ex-message e) "valid day")))))
(def stockholm (time/find-time-zone "stockholm"))
(def moscow (time/find-time-zone "moscow"))

(deftest with-month
  (is (= (time/of 2020 10 2 3 4 5)
         (time/with-month (time/of 2020 1 2 3 4 5) 10)))
  (try (time/with-month (time/of 2003 1 29 2 0 1) 2)
       (is (not "hit"))
       (catch clojure.lang.ExceptionInfo e
         (is (=  {:value 2,
                  :conflict :month}
                 (select-keys (ex-data e) [:value :conflict])))
         (is (string/includes? (ex-message e) "valid day"))))
  
  (try (time/with-month
         (time/with-time-zone (time/of 1980 3 6 2 0 1) stockholm)
         4)
       (is (not "hit"))
       (catch clojure.lang.ExceptionInfo e
         (is (= {:value 4,
                 :conflict :month}
                (select-keys (ex-data e) [:value :conflict])))
         (is (string/includes? (ex-message e) "gap")))))

(deftest with-day
  (is (= (time/of 2020 1 31 3 4 5)
         (time/with-day (time/of 2020 1 2 3 4 5) 31)))
  (try (time/with-day (time/of 2003 2 28 2 0 1) 29)
       (is (not "hit"))
       (catch clojure.lang.ExceptionInfo e
         (is (= {:value 29,
                 :conflict :day}
                (select-keys (ex-data e) [:value :conflict])))
         (is (string/includes? (ex-message e) "valid day"))))

  (try (time/with-day (time/with-time-zone (time/of 1980 4 5 2 0 1) stockholm) 6)
           (is (not "hit"))
           (catch clojure.lang.ExceptionInfo e
             (is (= {:value 6,
                     :conflict :day}
                    (select-keys (ex-data e) [:value :conflict])))
             (is (string/includes? (ex-message e) "gap")))))

(deftest with-hour
  (is (= (time/of 2020 1 2 23 4 5)
         (time/with-hour (time/of 2020 1 2 3 4 5) 23)))
  (try (time/with-hour (time/of 2020 1 2 3 4 5) 32)
       (is (not "hit"))
       (catch clojure.lang.ExceptionInfo e
         (is (= {:value 32,
                 :conflict :hour}
                (select-keys (ex-data e) [:value :conflict])))
         (is (string/includes? (ex-message e) "valid hour"))))

  (try (time/with-hour (time/with-time-zone (time/of 1980 4 6 1 0 1) stockholm) 2)
       (is (not "hit"))
       (catch clojure.lang.ExceptionInfo e
         (is (= {:value 2,
                 :conflict :hour}
                (select-keys (ex-data e) [:value :conflict])))
         (is (string/includes? (ex-message e) "gap")))))


(deftest with-minute
  (is (= (time/of 2020 1 2 3 13 5)
         (time/with-minute (time/of 2020 1 2 3 4 5) 13)))
  (try (time/with-minute (time/of 2020 1 2 3 4 5) 61)
       (is (not "hit"))
       (catch clojure.lang.ExceptionInfo e
         (is (= {:value 61,
                 :conflict :minute}
                (select-keys (ex-data e) [:value :conflict])))
         (is (string/includes? (ex-message e) "valid minute")))))

(deftest with-second
  (is (= (time/of 2020 1 2 3 4 13)
         (time/with-second (time/of 2020 1 2 3 4 5) 13)))
  (try (time/with-second (time/of 2020 1 2 3 4 5) 61)
       (is (not "hit"))
       (catch clojure.lang.ExceptionInfo e
         (is (= {:value 61,
                 :conflict :second}
                (select-keys (ex-data e) [:value :conflict])))
         (is (string/includes? (ex-message e) "valid second")))))

(deftest with-millis
  (is (= (time/add-millis (time/of 2020 1 2 3 4 5) 13)
         (time/with-millis (time/of 2020 1 2 3 4 5) 13)))
  (try (time/with-millis (time/of 2020 1 2 3 4 5) 1000)
       (is (not "hit"))
       (catch clojure.lang.ExceptionInfo e
         (is (= {:value 1000,
                 :conflict :millis}
                (select-keys (ex-data e) [:value :conflict])))
         (is (string/includes? (ex-message e) "valid millis")))))

(deftest with-nanos
  (is (= (time/add-nanos (time/of 2020 1 2 3 4 5) 13)
         (time/with-nano (time/of 2020 1 2 3 4 5) 13)))
  (is (= (time/add-millis (time/of 2020 1 2 3 4 5) 900)
         (time/with-nano (time/of 2020 1 2 3 4 5) (* 900 1000 1000))))
  (is (string/includes?  (str (time/with-nano
                                (time/of 2020 1 2 3 4 5)
                                (* 900 1000 1000)))
                         "03:04:05.9"))
  (try (time/with-nano (time/of 2020 1 2 3 4 5) (* 1 1000 1000 1000))
       (is (not "hit"))
       (catch clojure.lang.ExceptionInfo e
         (is (= {:value (* 1 1000 1000 1000),
                 :conflict :nano}
                (select-keys (ex-data e) [:value :conflict])))
         (is (string/includes? (ex-message e) "valid nanos")))))

(deftest with-time-part ()
      (is (= (time/of 2020 10 01 13 44 55)
             (time/with-time-part
               (time/with-time-zone (time/of 2020 10 01 20 30 00) stockholm)
               (time/time-part 13 44 55))))
  (let [[from to] (first (time/transitions (time/with-time-zone (time/of 2020 01 01) stockholm)))
            hole (-> (time/add-minutes (time/time-part-of from)
                                       15)
                     (time/just-after))]
        (try (time/with-time-part from hole)
             (catch clojure.lang.ExceptionInfo e
               (is (= {:value hole
                       :conflict :time-part}
                      (select-keys (ex-data e) [:value :conflict])))
               (is (string/includes? (ex-message e) "valid" ))))))


(deftest time-part
  (is (= (time/time-part 3 4 5)
         (time/time-part-of (time/of 2020 1 2 3 4 5))))
  (is (= (time/time-part 0 4 5)
         (time/time-part-of (time/of 2020 1 2 0 4 5))))
  (is (= (time/time-part 0 0 5)
         (time/time-part-of (time/of 2020 1 2 0 0 5))))
  (is (= (time/time-part 0 0 0)
         (time/time-part-of (time/of 2020 1 2 0 0 0))))
  (is (= (time/time-part 0 0 59)
         (time/time-part-of (time/of 2020 1 2 0 0 59))))
  (is (= (time/time-part 0 59 0)
         (time/time-part-of (time/of 2020 1 2 0 59 0))))
  (is (= (time/time-part 0 59)
         (time/time-part-of (time/of 2020 1 2 0 59 0))))
  (is (= (time/time-part 23 0 0)
         (time/time-part-of (time/of 2020 1 2 23 0 0))))
  (try (time/time-part 24 0 0)
       (is (not "hit"))
       (catch clojure.lang.ExceptionInfo e
         (is (= {:value 24,
                 :conflict :hour}
                (select-keys (ex-data e) [:value :conflict])))
         (is (and (string/includes? (ex-message e) "valid")
                  (string/includes? (ex-message e) "hour")))))
  (try (time/time-part -1 0 0)
       (is (not "hit"))
       (catch clojure.lang.ExceptionInfo e
         (is (= {:value -1,
                 :conflict :hour}
                (select-keys (ex-data e) [:value :conflict])))
         (is (and (string/includes? (ex-message e) "valid")
                  (string/includes? (ex-message e) "hour")))))
  (try (time/time-part 0 -1 0)
       (is (not "hit"))
       (catch clojure.lang.ExceptionInfo e
         (is (= {:value -1,
                 :conflict :minute}
                (select-keys (ex-data e) [:value :conflict])))
         (is (and (string/includes? (ex-message e) "valid")
                  (string/includes? (ex-message e) "minute")))))
  (try (time/time-part 0 60 0)
       (is (not "hit"))
       (catch clojure.lang.ExceptionInfo e
         (is (= {:value 60,
                 :conflict :minute}
                (select-keys (ex-data e) [:value :conflict])))
         (is (and (string/includes? (ex-message e) "valid")
                  (string/includes? (ex-message e) "minute")))))
  (try (time/time-part 0 0 60)
       (is (not "hit"))
       (catch clojure.lang.ExceptionInfo e
         (is (= {:value 60,
                 :conflict :second}
                (select-keys (ex-data e) [:value :conflict])))
         (is (and (string/includes? (ex-message e) "valid")
                  (string/includes? (ex-message e) "second")))))
  (try (time/time-part 0 0 -1)
       (is (not "hit"))
       (catch clojure.lang.ExceptionInfo e
         (is (= {:value -1,
                 :conflict :second}
                (select-keys (ex-data e) [:value :conflict])))
         (is (and (string/includes? (ex-message e) "valid")
                  (string/includes? (ex-message e) "second"))))))


(deftest date-part
  (is (= (time/date-part 2020 1 2)
         (time/date-part-of (time/of 2020 1 2 3 4 5))))
  (is (= (time/date-part 2020 1 1)
         (time/date-part-of (time/of 2020 1 1 0 4 5))))
  (is (= (time/date-part 2020 1 31)
         (time/date-part-of (time/of 2020 1 31 0 0 5))))
  (is (= (time/date-part 2020 1 2)
         (time/date-part-of (time/of 2020 1 2 0 0 0))))
  
  
  (try (time/date-part 2020 0 1)
       (is (not "hit"))
       (catch clojure.lang.ExceptionInfo e
         (is (= {:value 0,
                 :conflict :month}
                (select-keys (ex-data e) [:value :conflict])))
         (is (and (string/includes? (ex-message e) "valid")
                  (string/includes? (ex-message e) "month")))))
  (try (time/date-part 2020 13  0)
       (is (not "hit"))
       (catch clojure.lang.ExceptionInfo e
         (is (= {:value 13,
                 :conflict :month}
                (select-keys (ex-data e) [:value :conflict])))
         (is (and (string/includes? (ex-message e) "valid")
                  (string/includes? (ex-message e) "month")))))
  (try (time/date-part 2010 1 60)
       (is (not "hit"))
       (catch clojure.lang.ExceptionInfo e
         (is (= {:value 60,
                 :conflict :day}
                (select-keys (ex-data e) [:value :conflict])))
         (is (and (string/includes? (ex-message e) "valid")
                  (string/includes? (ex-message e) "day")))))
  (try (time/date-part 2020 1 0)
       (is (not "hit"))
       (catch clojure.lang.ExceptionInfo e
         (is (= {:value 0,
                 :conflict :day}
                (select-keys (ex-data e) [:value :conflict])))
         (is (and (string/includes? (ex-message e) "valid")
                  (string/includes? (ex-message e) "day"))))))


(deftest truncate
  (let [a (time/of 2121 10 21 13 14 15)
        b (time/with-nano (time/of 2121 10 21 13 14 15) 1002003)]
    (is (= (time/of 2120) (time/truncate a :decade)))
    (is (= (time/of 2100) (time/truncate a :century)))
    (is (= (time/of 2000) (time/truncate a :millenia)))
    (is (= (time/of 2121) (time/truncate a :year)))
    (is (= (time/of 2121 10) (time/truncate a :month)))
    (is (= (time/of 2121 10 21) (time/truncate a :day)))
    (is (= (time/of 2121 10 21 13) (time/truncate a :hour)))
    (is (= (time/of 2121 10 21 13 14) (time/truncate a :minute)))
    (is (= (time/of 2121 10 21 13 14 15) (time/truncate a :second)))
    (is (= (time/of 2121 10 21 13 14 15) (time/truncate a :millis)))
    (is (= (time/with-millis (time/of 2121 10 21 13 14 15) 1) (time/truncate b :millis)))
    (is (= (time/with-nano (time/of 2121 10 21 13 14 15) 1002000) (time/truncate b :micros)))
    (try (time/truncate b :birds)
      (is (not "hit"))
      (catch clojure.lang.ExceptionInfo e
        (is (and (string/includes? (ex-message e) ":birds")))))))


(deftest overlapp
      (let [dump (fn [q text ] (println (str text q)) q)
            [from to] (first (time/transitions (time/with-time-zone (time/of 2020 06 01) stockholm)))]
        (is (= (time/time-part-of to)
               (-> (time/just-after from)
                   (time/with-later-at-overlap)
                   (time/time-part-of))))
        (is (= (-> from
                   (time/just-after)
                   (time/time-part-of))
               (-> to
                   (time/with-earlier-at-overlap )
                   (time/time-part-of))))))

