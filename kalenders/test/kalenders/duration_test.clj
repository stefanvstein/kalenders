(ns kalenders.duration-test
  (:require [clojure.test :refer :all]
            [kalenders.duration :as duration]
            [kalenders.time :as time]))

(deftest minutes (let [a (duration/of-seconds 60)
                       b (duration/add-minutes duration/none 1)
                       c (duration/add-minutes a 1)
                       d (duration/add-minutes a -2)]
                   (is (= a b))
                   (is (= 1 (duration/minutes-of b)))
                   (is (= 2 (duration/minutes-of c)))
                   (is (= -1 (duration/minutes-of d)))))

(deftest hours (let [a (duration/add-minutes duration/none 60)
                     b (duration/add-hours duration/none 1)
                       c (duration/add-hours a 1)
                       d (duration/add-hours a -2)]
                   (is (= a b))
                   (is (= 1 (duration/hours-of b)))
                   (is (= 2 (duration/hours-of c)))
                   (is (= -1 (duration/hours-of d)))))

(deftest seconds (let [a (duration/add-millis duration/none 1000)
                       b (duration/add-seconds duration/none 1)
                       c (duration/add-seconds a 1)
                       d (duration/add-seconds a -2)]
                   (is (= a b))
                   (is (= 1 (duration/seconds-of b)))
                   (is (= 2 (duration/seconds-of c)))
                   (is (= -1 (duration/seconds-of d)))))

(deftest millis (let [a (duration/add-nanos duration/none 1000000)
                      b (duration/add-millis duration/none 1)
                      c (duration/add-millis a 1)
                      d (duration/add-millis a -2)]
                  (is (= a b))
                  (is (= 1 (duration/millis-of b)))
                  (is (= 2 (duration/millis-of c)))
                  (is (= -1 (duration/millis-of d)))))

(deftest nonos (let [b (duration/add-nanos duration/none 1)
                     c (duration/add-nanos b 1)
                     d (duration/add-nanos b -2)]
                 (is (= 1 (duration/nanos-of b)))
                 (is (= 2 (duration/nanos-of c)))
                 (is (= -1 (duration/nanos-of d)))))


(deftest days (let [a (duration/add-hours duration/none 24)
                      b (duration/add-days duration/none 1)
                      c (duration/add-days a 1)
                      d (duration/add-days a -2)]
                  (is (= a b))
                  (is (= 1 (duration/days-of b)))
                  (is (= 2 (duration/days-of c)))
                  (is (= -1 (duration/days-of d)))))

(deftest duration-of
  (is (= (-> duration/none
             (duration/add-days 1)
             (duration/add-hours 10)
             (duration/add-minutes 4)
             (duration/add-seconds 5))
         (duration/of 34 4 5))))

(deftest neg-and-abs
  (let [s (duration/of-seconds 1)]
    (is (not (duration/negative? s)))
    (is (duration/negative? (duration/negated s)))
    (is (not (duration/negative? (duration/abs s))))
    (is (not (duration/negative? (duration/abs (duration/negated s)))))))

(deftest plus-and-minus
  (let [a (duration/of 1 2 3)
        b (duration/of 25 59 3)]
    (is (= (duration/of 27 1 6) (duration/plus a b)))
    (is (= (duration/of 3 6 9) (duration/plus a a a)))
    (is (= (duration/of 24 57 0) (duration/minus b a)))
    (is (= (duration/of 23 54 57) (duration/minus b a a)))))

(deftest between
  (is (= (duration/of 24 2 0)
         (duration/between (time/of 2020 1 1 0 0 0)
                           (time/of 2020 1 2 0 2 0))))
  (is (duration/none? 
         (duration/between (time/of 2020 1 1 0 0 0)
                           (time/of 2020 1 1 0 0 0)))))

(deftest multiply-divide
  (is (= (duration/of 1 2 3) (duration/divided-by (duration/of 3 6 9) 3)))
  (is (= (duration/of 0 31 2) (duration/divided-by (duration/of 3 6 12) 3 2)))
  (is (= (duration/of 4 9 20) (duration/multiplied-by (duration/of 1 2 20) 4)))
  (is (= (duration/of 12 28 0) (duration/multiplied-by (duration/of 1 2 20) 4 3))))

(deftest ordering
  (is (duration/increasing-order))
  (is (duration/increasing-order (duration/of 1 2 3)))
  (is (duration/increasing-order (duration/of 1 2 3)
                                 (duration/of 1 2 4)))
  
  (is (not (duration/increasing-order (duration/of 1 2 3)
                                      (duration/of 1 2 3)
                                      (duration/of 1 2 2))))
  (is (not (duration/increasing-order (duration/of 1 2 4)
                                      (duration/of 1 2 3))))
  (is (duration/decreasing-order))
  (is (duration/decreasing-order (duration/of 1 2 3)))
  (is (duration/decreasing-order (duration/of 1 2 3)
                                 (duration/of 1 2 3)
                                 (duration/of 1 2 2)))
  (is (not (duration/decreasing-order (duration/of 1 2 4)
                                      (duration/of 1 2 3)
                                      (duration/of 1 2 4))))
  (is (not (duration/decreasing-order (duration/of 1 2 4)
                                      (duration/of 1 2 4)
                                      (duration/of 1 2 5)))))

(deftest values-of
  (is (= [1 2 3] (duration/hours-minutes-seconds-of (duration/of 1 2 3))))
  (is (= [1 2 3 4] (duration/days-hours-minutes-seconds-of (duration/of 26 3 4))))
  
  
  (is (= [4 5] (duration/seconds-millis-of
                (-> (duration/of 0 0 4)
                    (duration/add-millis 5)
                    (duration/add-nanos 6))))))
(deftest to-and-from-text
  (let [a (-> (duration/of 0 0 4)
              (duration/add-millis 5)
              (duration/add-nanos 6))]
    (is (= a (duration/from-iso8601 (duration/to-iso8601 a))))))
