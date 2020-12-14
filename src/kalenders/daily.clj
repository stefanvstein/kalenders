(ns kalenders.daily
  (:require [kalenders.duration :as duration]
            [kalenders.searching :as searching]
            [kalenders.time :as time]))

(defn- previous-day [time]
  (time/add-days time -1))

(defn- next-day [time]
  (time/add-days time 1))

(defn- roll-days-until [timestamp until-time]
  (if (time/increasing? until-time (time/time-part-of timestamp))
    (time/with (next-day timestamp) until-time)
    (time/with timestamp until-time)))



(defn daily [from-time until-time timestamp]
    (let [next-until (roll-days-until timestamp until-time)
          next-from (roll-days-until timestamp from-time)
          
          next-from (if (time/increasing? next-from next-until)
                     next-from
                     (previous-day next-from))
          duration (duration/between next-from next-until)]
      (if (duration/none? duration)
        searching/ZERO
        [next-from duration])))





