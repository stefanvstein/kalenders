(ns kalenders.core
  "A library for occurrences in time. A occurrence has a begining and a
  duration. Overlapping occurences are merged into a larger occurence.
  The core occurences function creates sequence of occurences based on
  a definition and a timestamp, starting with the ongoing occurence or
  next future occurence in relation to the timestamp, followed by the
  chronologic continuing.
  A definition is two functions. The first produces a [start-time duration] from
  a timestamp. The second produces a description of fully covered definitions from
  a [start-time duration] that tells what descriptions caused the occurence. 
  Definitions can be combined to produce complex
  definitions of occurrence sequences"
  (:require [kalenders.daily :as daily]
            [kalenders.duration :as duration]
            [kalenders.searching :as searching]
            [kalenders.time :as time]
            [kalenders.weekly :as weekly]
            [kalenders.monthly :as monthly]
            [kalenders.year-weekly :as year-weekly]
            [kalenders.macros :refer :all]
            [clojure.set :as set]))

(defn combine [& definitions]
  (->> (or (seq definitions)
           [searching/ZERO])
       (reduce searching/either)))
             
(defn occurrences [definition timestamp]
  (let [next-occurance (definition timestamp)]
    (if (= searching/ZERO next-occurance)
      nil
      (let [[start duration] next-occurance
            ending (time/add-duration start duration)
            earlier-timestamp (time/just-before start)
            real-beginning  (searching/starting definition earlier-timestamp start ending)
            [real-start real-duration] real-beginning
            ending (time/add-duration real-start real-duration)
            following-timestamp (time/just-after ending)]
        (cons real-beginning
              (searching/periods-forward definition following-timestamp))))))

(defn matching [definition start duration]
  (let [end (time/add-duration start duration)]
    (let [hits (searching/hits-within definition start end)]
      (reduce (fn [a [start' duration']]
                (set/union a (set (definition start' duration'))))
              #{}
              hits))))

(defn once [from duration]
  (fn ([timestamp] (let [end (time/add-duration from duration)]
                     (if (time/increasing? end timestamp)
                       searching/ZERO
                       [from duration])))
    ([start d] [{:type :once :from from :duration duration}])))

(defn between [from until]
   (if (time/increasing? until from)
      (between until from)
      (fn between'
        ([timestamp]
         (if (time/increasing? until timestamp)
           searching/ZERO
           [from (duration/between from until)]))
        ([start dur] [{:type :between :from from :until until}]))))

(defn daily [from-time until-time]
  (fn
    ([t] (daily/daily from-time until-time t))
    ([from duration] [{:type :daily
                       :occurance from
                       :from from-time
                       :until until-time}])))
          
(defn weekly
  "Returns a occurance "
  [fromDay fromTime untilDay untilTime]
  (fn ([t] (weekly/weekly fromDay fromTime untilDay untilTime t))
    ([from duration] [{:type weekly :from-day fromDay :from-time fromTime :until-day untilDay :until-time untilTime :occurence from}])))

(defn year-weekly [from-week from-day from-time-of-day
                   until-week until-day until-time-of-day]
  (fn [t] (year-weekly/year-weekly from-week from-day from-time-of-day
                                   until-week until-day until-time-of-day
                                   t))) 

(defn monthly
  [from-day from-time-of-day until-day until-time-of-day]
  (fn ([t] (monthly/monthly from-time-of-day until-day until-time-of-day t))
    ([from duration] [{:type :monthly :from-day from-day :from-time from-time-of-day :until-day until-day :until-time until-time-of-day :occuence from}])))

(defn begining-of-month [until-day until-time-of-day]
  (fn ([t] (monthly/monthly-from-start until-day until-time-of-day t))
    ([start duration] [{:type :beginning-of-month :until-day  until-day :until-time until-time-of-day :occurence start}])))

(defn end-of-month [from-day from-time-of-day]
  (fn ([t] (monthly/monthly-until-end from-day from-time-of-day t))
    ([start duration] [{:type :end-of-month :from-day from-day :from-time from-time-of-day :occurence start}])))

(defn days
  ([pred look-ahead-days]
   (fn ([timestamp]
        (let [start (time/with-time-part timestamp (time/time-part 0 0 0))]
          ((fn [days]
             (let [date (-> start
                            (time/add-days days))]
               (if (and (| days < look-ahead-days)
                        (pred date))
                 (let [day (time/with-time-part date (time/time-part 0 0))
                       duration (duration/of-hours 24)]
                   [day duration])
                 (if (| days < look-ahead-days)
                   (recur (inc days))
                   searching/ZERO)))) 0)))
     ([start dur] (let [days (range (duration/days-of dur))]
                    (reduce (fn [a n]
                              (let [start' (time/add-days start n)]
                                (if-let [c (pred start')]
                                  (apply conj
                                         a
                                         (map #(do {:type %
                                                    :date (time/date-part-of start')})
                                              c))
                                  a)))
                            #{} days)))))
  ([pred] (days pred (* 2 366))))

(defn with [def1 def2])
(comment "Se till att alla tider är until exclusive, nano sekunden innan")

(comment "Why Zero and not nil punning?")

  



