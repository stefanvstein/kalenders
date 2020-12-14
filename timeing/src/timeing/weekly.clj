(ns timeing.weekly
  (:require [timeing.time :as time]
            [timeing.duration :as duration])
  (:import java.time.DayOfWeek))

(defn- next-week [time]
  (time/add-days time 7))

(defn- previous-week [time]
  (time/add-days time -7))

(defn- roll [from until-day until-time]
  (let [from-value (-> (time/day-of-week from)
                       (time/day-of-week-nr))
        until-value (time/day-of-week-nr until-day)
        days (-> (- until-value
                    from-value)
                 (+ 7)
                 (mod 7))
              
        with-days (time/add-days from days)
        prospect (time/with with-days until-time)]
    (if (time/increasing? prospect from)
      (next-week prospect)
      prospect)))


(defn- next-hit [from day time]
  (let [prospect (roll from day time)]
    (if (time/increasing? (next-week from) prospect)
      (previous-week prospect)
      prospect)))
  
(defn weekly [from-day from-time until-day until-time t]
  (let [until (next-hit t until-day until-time)
        prospect (roll t from-day from-time)
        from (if (time/increasing? until prospect)
                   (previous-week prospect)
                   prospect)
        duration (duration/between from until)]
    [from duration]))
  
