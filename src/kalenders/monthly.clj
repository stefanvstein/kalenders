(ns kalenders.monthly
  "A month period can not span more than one calendar month, meaning that start has to be lower than start. You can combine a monthly-until-end and monthly-from-start using kalenders.core/combine to act as one

  A month period will not start if the start is not valid for that month. A end will occur in the end of the month when the ending value is higher than available of the month, but at the same time of day. The monthly-until-end definition will though end as the month ends at midnight"
    (:require [kalenders.time :as time]
              [kalenders.duration :as duration]
              [kalenders.macros :refer :all]))


(defn- calc [ref day time-part skip-non-existing]
  (let [adjust {:adjust true}
        [year month] (time/year-month-day ref)
        ymd (time/date-part year month day adjust)
        [_ _ d] (time/year-month-day ymd)]
    (if (or (= d day) (not skip-non-existing))
      (time/with-time-part (time/with-date-part ref ymd) time-part adjust))))

(defn- until [ref-time day time-part]
  (if day
    (calc ref-time day time-part nil)
    (time/end-of-month ref-time)))

(defn- start [ref-time day time-part]
  (if day
    (calc ref-time day time-part :skip-non-existing)
    (time/begining-of-month ref-time)))

(defn- define-recursivly [from-day from-time-part until-day until-time-part timestamp]
  (loop [attempt 0 ref-time (time/begining-of-month timestamp)]
    (when (| 13 > attempt)
      (let [next-attempt (inc attempt)
            start-of-next-month (time/just-after (time/end-of-month ref-time))]
        (if-let [start (start ref-time from-day from-time-part)]
          (let [end (until ref-time until-day until-time-part)]
            (if (time/ordered? timestamp end)
              [start end]
              (recur next-attempt start-of-next-month)))
          (recur next-attempt start-of-next-month))))))

(defn start-until [from-day from-time-part until-day until-time-part timestamp]
  (when (| from-day > 31)
    (throw (ex-info "Day of month is not higher than 31" {:value from-day
                                                          :conflict :from-day})))
  (when (| until-day > 31)
    (throw (ex-info "Day of month is not higher than 31" {:value until-day
                                                          :conflict :until-day})))
  (when (| until-day < from-day)
    (throw (ex-info (str "Ending day " until-day " is before start day " from-day)
                    {:value until-day
                     :conflict :until-day})))
  (when (and (= until-day from-day)
             (not (time/ordered? from-time-part until-time-part)))
    (throw (ex-info (str "Ending time " until-time-part
                         " is before start time " from-day
                         " for the same day " until-day)
                    {:value until-time-part
                     :conflict :until-time})))
  (define-recursivly
    from-day from-time-part
    until-day until-time-part
    timestamp))

(defn start-until-end-of-month [from-day from-time-part timestamp]
  (when (| from-day > 31)
    (throw (ex-info "Day of month is not higher than 31" {:value from-day
                                                          :conflict :from-day})))
  (define-recursivly from-day from-time-part nil nil timestamp))

(defn begining-of-month-until [until-day until-time-part timestamp]
  (when (| until-day > 31)
    (throw (ex-info "Day of month is not higher than 31" {:value until-day
                                                          :conflict :until-day})))
  (define-recursivly nil nil until-day until-time-part timestamp))


(defn monthly [from-day from-time-part until-day until-time-part timestamp]
  (let [[start until] (start-until from-day from-time-part
                                   until-day until-time-part
                                   timestamp)
        duration (duration/between start until)]
    [start duration]))

(defn monthly-until-end [from-day from-time-part timestamp]
  (let [[start until] (start-until-end-of-month from-day from-time-part
                                                timestamp)
        duration (duration/between start until)]
    [start duration]))

(defn monthly-from-start [until-day until-time-part timestamp]
  (let [[start until] (begining-of-month-until until-day until-time-part
                                               timestamp)
        duration (duration/between start until)]
    [start duration]))
