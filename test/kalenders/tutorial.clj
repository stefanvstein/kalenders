(ns kalenders.tutorial
  (:require  [clojure.test :refer :all]
             [kalenders.core :as k]
             [kalenders.time :as t]
             [kalenders.swedish :as swe]
             [kalenders.duration :as duration]
             [clojure.pprint :as pprint]))


(defn print-from [definition from n]
  (doseq [from-to (->> from
                       (k/occurrences definition)
                       (take n)
                       (map (fn [[time duration]]
                              (str time " - " (t/add-duration time duration)))))]
    (println from-to)))

(deftest tutorial
  (println "weekends")
  (let [weekends (k/weekly t/friday (t/time-part 16 0)
                           t/monday (t/time-part 8 0))]
    (print-from weekends (t/now) 3))
  (println "combinations")
  (let [weekends (k/weekly t/friday (t/time-part 16 0)
                           t/monday (t/time-part 8 0))
        daily (k/daily (t/time-part 16 0)
                       (t/time-part 8 0))
        midnight (t/time-part 0 0)
        sportlov (k/year-weekly 9 t/monday midnight
                                9 t/sunday (t/just-before midnight))
        holidays (k/days swe/helgdag? 365)
        closed (k/combine weekends daily sportlov holidays)
        ]
    (print-from closed (t/now) 5)
    (is (= 1 1))))

