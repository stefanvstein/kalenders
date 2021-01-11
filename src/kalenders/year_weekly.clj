(ns kalenders.year-weekly
  "The week can belong to another year"
  (:require [kalenders.time :as time]
            [kalenders.duration :as duration]
            [kalenders.macros :refer :all]))

(defn- year-of [time]
  (let [[year] (time/year-month-day time)]
    year))

(defn- calc [year week week-day time-part]
  (let [adjust {:adjust true}]        
     (-> (time/start-of-week-nr year week adjust)
         (time/adjust-day-of-week week-day)
         (time/with-time-part time-part adjust))))

(defn- next-until [ref-time week week-day time-part]
  (let [year (year-of ref-time)
        next-year (inc year)
        until (calc year week week-day time-part)]
    (if (time/ordered? ref-time until)
      until
      (calc next-year week week-day time-part))))


(defn- start [until ref-time week week-day time-part]
  
  (let [year (year-of ref-time)
        next-year (inc year)]
    ((fn [y w d tp]
       (let [start (calc y w d tp)]
         (if (time/ordered? start until)
           start
           (recur (dec y) w d tp))))
     next-year week week-day time-part)))

(defn start-until [from-week from-day from-time until-week until-day until-time t]
  (when (| until-week > 53)  
    (throw (ex-info "Week number is not higher than 53" {:value until-week
                                                         :conflict :until-week})))
  (when (| from-week > 53)  
    (throw (ex-info "Week number is not higher than 53" {:value from-week
                                                         :conflict :from-week})))
  
  (let [until (next-until t until-week until-day until-time)
        start (start until t from-week from-day from-time)]
    [start until]))

(defn year-weekly [from-week from-day from-time until-week until-day until-time t]
  (let [[start until] (start-until from-week
                                   from-day
                                   from-time
                                   until-week
                                   until-day
                                   until-time
                                   t) 
        duration (duration/between start until)]
    [start duration])) 
