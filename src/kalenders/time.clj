
(ns kalenders.time
  (:import [java.time ZonedDateTime Duration Instant ZoneId LocalTime DayOfWeek YearMonth MonthDay LocalDate LocalDateTime
            DateTimeException Month]
           [java.time.zone ZoneRulesException]
           [java.time.temporal ChronoUnit ChronoField WeekFields TemporalField TemporalAccessor Temporal TemporalAdjuster TemporalQueries]
           [java.time.format TextStyle]
           [java.util.regex Pattern]
           [java.util Locale])
  (:require [clojure.string :as string]
            [clojure.set :as set]))

(defn now [] ^ZonedDateTime
  (ZonedDateTime/now))

(defn has-hour? [^TemporalAccessor time]
  (.isSupported time ChronoField/HOUR_OF_DAY))

(defn has-hour-minute-second? [^TemporalAccessor time]
  (and
   (.isSupported time ChronoField/HOUR_OF_DAY)
   (.isSupported time ChronoField/MINUTE_OF_HOUR)
   (.isSupported time ChronoField/SECOND_OF_MINUTE)))

(defn hour-minute-second
  "a vector with [hours minutes seconds] values of time "
  [^TemporalAccessor time]
  (when-not (has-hour-minute-second? time)
    (throw (ex-info (str "object of type " (str (class time))
                        " does not support hour minute and second")
                   {})))
  [(.get time ChronoField/HOUR_OF_DAY)
   (.get time ChronoField/MINUTE_OF_HOUR)
   (.get time ChronoField/SECOND_OF_MINUTE)])

(defn has-minute? [^TemporalAccessor time]
  (.isSupported time ChronoField/MINUTE_OF_HOUR))

(defn has-hour-minute-second-millis? [^TemporalAccessor time]
  (and (.isSupported time ChronoField/HOUR_OF_DAY)
       (.isSupported time ChronoField/MINUTE_OF_HOUR)
       (.isSupported time ChronoField/SECOND_OF_MINUTE)
       (.isSupported time ChronoField/MILLI_OF_SECOND)))  

(defn hour-minute-second-millis
  "a vector with [hours minutes seconds millis] values of time "
  [^TemporalAccessor time]
  (when-not (has-hour-minute-second-millis? time)
    (throw (ex-info (str "object of type " (str (class time))
                         " does not support hour minute second and millis")
                    {:conflict :class})))
  [(.get time ChronoField/HOUR_OF_DAY)
   (.get time ChronoField/MINUTE_OF_HOUR)
   (.get time ChronoField/SECOND_OF_MINUTE)
   (.get time ChronoField/MILLI_OF_SECOND)])

(defn has-hour-minute-second-nanos? [^TemporalAccessor time]
  (and (.isSupported time ChronoField/HOUR_OF_DAY)
       (.isSupported time ChronoField/MINUTE_OF_HOUR)
       (.isSupported time ChronoField/SECOND_OF_MINUTE)
       (.isSupported time ChronoField/NANO_OF_SECOND)))

(defn hour-minute-second-nanos
  "a vector with [hours minutes seconds nanos] values of time "
  [^TemporalAccessor time]
  (when-not (has-hour-minute-second-nanos? time)
    (throw (ex-info (str "object of type " (str (class time))
                         " does not support hour minute second and nano")
                    {:conflict :class})))
  [(.get time ChronoField/HOUR_OF_DAY)
   (.get time ChronoField/MINUTE_OF_HOUR)
   (.get time ChronoField/SECOND_OF_MINUTE)
   (.get time ChronoField/NANO_OF_SECOND)])

(defn has-year-month? [^TemporalAccessor time]
  (and (.isSupported time ChronoField/YEAR)
       (.isSupported time ChronoField/MONTH_OF_YEAR)))

(defn has-year-month-day? [^TemporalAccessor time]
  (and (.isSupported time ChronoField/YEAR)
       (.isSupported time ChronoField/MONTH_OF_YEAR)
       (.isSupported time ChronoField/DAY_OF_MONTH)))

(defn year-month
  "a vector with [year month] values of time "
  [^TemporalAccessor time]
  (when-not (has-year-month? time)
    (throw (ex-info (str "Type " (str (class time))
                         " does not support year and month.")
                    {})))
  [(.get time ChronoField/YEAR)
   (.get time ChronoField/MONTH_OF_YEAR)])

(defn year-month-day
  "a vector with [year month day] values of time "
  [^TemporalAccessor time]
  (when-not (has-year-month-day? time)
    (throw (ex-info (str "object of type " (str (class time))
                         " does not support year month and day")
                    {})))
  [(.get time ChronoField/YEAR)
   (.get time ChronoField/MONTH_OF_YEAR)
   (.get time ChronoField/DAY_OF_MONTH)])


(defn- with-year-exact [^Temporal time ^Integer year]
  (let [[y ^Integer month day] (year-month-day time)
        year-month (YearMonth/of year month)]
    (when-not (.isValidDay year-month day)
      (throw (ex-info
              (str (LocalDate/from time)
                   " can not have year changed to " year
                   " since " day " is not a valid day of "
                   year-month)
              {:value year
               :conflict :year
               :time time})))
      
    (let [new-time (.with time ChronoField/YEAR year)]
      (if-not (has-hour? time)
        new-time
        (let [hour (. time get ChronoField/HOUR_OF_DAY)]
          (if (= hour (.get new-time ChronoField/HOUR_OF_DAY))
            new-time
            (-> (ex-info
                 (str (LocalDate/from time)
                      " can not have year changed to " year
                      " since there is a gap at "
                      (LocalTime/from time) ".")
                 {:value year
                  :conflict :year
                  :time time})
                (throw))))))

      ))

(defn with-year
  "a time with year set to value unless the remaining values would render
  it an invalid time for that year. The time will be at the later occation
  when hitting an overlap. The time is not adjusted in other ways. An
  ExceptionInfo with :conflict having value :year would be thrown on
  erroneous conditions"
  [^Temporal time ^Integer year & [{:keys [adjust]}]]
  (if-not (has-year-month-day? time)
    (if (has-year-month? time)
      (.with time ChronoField/YEAR year)
      (throw (ex-info (str "type " (class time)
                           " does not have year month or day")
                      {:value time
                       :conflict :class})))
    (if adjust
      (.with time ChronoField/YEAR year)
      (with-year-exact time year))))

(defn- with-month-exact [^Temporal time ^Integer month]
  (let [[^Integer year m day] (year-month-day time)
            year-month (YearMonth/of year month)
            hour (. time get ChronoField/HOUR_OF_DAY)]  
    (when-not (.isValidDay year-month day)
      (throw (ex-info
              (str (LocalDate/from time)
                   " can not have month changed to " month
                   " since " day " is not a valid day of "
                   year-month)
              {:value month
               :conflict :month
               :time time})))  
    (let [new-time (.with time ChronoField/MONTH_OF_YEAR month)]
      (if (= hour (.get new-time ChronoField/HOUR_OF_DAY))
        new-time
        (let [message (str (LocalDate/from time)
                           " can not have month changed to " month
                           " since there is a gap at "
                           (LocalTime/from time) ".")]
          (throw (ex-info message {:value month
                                   :conflict :month
                                   :time time})))))))

(defn with-month
  "a time with month set to value unless the remaining values would
  render it an invalid time for that month. The time will be at the
  later occation when hitting an overlap. The time is not adjusted
  in other ways. An ExceptionInfo with :conflict having value :month
  would be thrown on erroneous conditions"
  [^Temporal time ^Integer month & [{:keys [adjust]}]]
  (when (or (< 12 month) (> 1 month))
    (throw (ex-info (str month " is not a valid value for month") 
                    {:value month
                     :conflict :month})))
  (if-not (has-year-month-day? time)
    (if (has-year-month? time)
      (.with time ChronoField/MONTH_OF_YEAR month) 
      (throw (ex-info (str "type " (class time)
                           " does not have year month or day")
                        {:value time
                         :conflict :class})))    
    (if adjust
      (.with time ChronoField/MONTH_OF_YEAR month)
      (with-month-exact time month))))



(defn- with-day-exact [^Temporal time ^Integer day]
  (let [[^Integer year ^Integer month d] (year-month-day time)
        year-month (YearMonth/of year month)]
    
    (when-not (.isValidDay year-month day)
      (let [message (str (LocalDate/from time)
                                   " can not have day changed to " day
                                   " since " day " is not a valid day of "
                                   year-month)]
                  (throw (ex-info message {:value day
                                           :conflict :day
                                           :time time}))))
    
    (let [new-time (.with time ChronoField/DAY_OF_MONTH day)]
      (if-not (has-hour? time)
        new-time
        (let [hour (. time get ChronoField/HOUR_OF_DAY)]
          (if (= hour (.get new-time ChronoField/HOUR_OF_DAY))
            new-time
            (throw (ex-info
                    (str (LocalDate/from time)
                         " can not have day changed to " day
                         " since there is a gap at " (LocalTime/from time) ".")
                    {:value day
                     :conflict :day
                     :time time}))))))))

(defn- valid-day? [time day]
  (let [[^Integer y ^Integer m _] (year-month-day time)]
    (.isValidDay (YearMonth/of y m) day)))

(defn last-day-of-month [^Temporal time]
  (when-not (has-year-month-day? time)
    (throw (ex-info (str (class time) " does not have day")
                    {:conflict :class :value time})))
  (. time with ChronoField/DAY_OF_MONTH (. (YearMonth/from time) lengthOfMonth)))

(defn with-day 
  "a time with day set to value unless the remaining values would render
  it an invalid time for that day. The time will be at the later
  occation when hitting an overlap. The time is not adjusted in other
  ways. An ExceptionInfo with :conflict having value :day would be thrown
  on erroneous conditions"
  [^Temporal time ^Integer day & [{:keys [adjust]}]]
  (when (or (< 31 day) (> 1 day))
    (throw (ex-info (str day " is not a valid value for day") 
                    {:value day
                     :conflict :day})))
  (when-not (has-year-month-day? time)
    (throw (ex-info (str "type " (class time)
                         " does not have year month or day")
                    {:value time
                     :conflict :class})))
  (if adjust
    (if (valid-day? time day)
      (.with time ChronoField/DAY_OF_MONTH day)
      (last-day-of-month time))
    (with-day-exact time day)))

(defn with-hour
  "a time with hour set to value unless the remaining values would
  render it an invalid time for that hour. The time will be at the
  later occation when hitting an overlap. The time is not adjusted
  in other ways. An ExceptionInfo with :conflict having value :hour
  would be thrown on erroneous conditions"
  [^Temporal time ^Integer hour & [{:keys [adjust]}]]
  (if (or (< 23 hour) (> 0 hour))
    (let [message (str hour " is not a valid hour")]
      (throw (ex-info message {:value hour
                               :conflict :hour
                               :time time})))
    (do (when-not (has-hour? time)
          (throw (ex-info (str "Type " (class time)
                               " does not have hour field")
                          {:value time
                           :conflict :class})))
        (let [new-time (.with time ChronoField/HOUR_OF_DAY hour)]
          (if adjust
            new-time
            (if (= hour (.get new-time ChronoField/HOUR_OF_DAY))
              new-time
              (let [message (str (LocalDateTime/from time)
                                 " can not have hour changed to " hour
                                 " since there is a gap.")]
                (throw (ex-info message {:value hour
                                         :conflict :hour
                                         :time time})))))))))

(defn with-minute 
  "a time with minute set to value unless the remaining values would
  render it an invalid time for that month. The time is not adjusted 
  in other ways. An ExceptionInfo with :conflict having value 
  :month would be thrown on erroneous conditions"
  [^Temporal time ^Integer minute & [{:keys [adjust]}]]
  (if (or (< 59 minute) (> 0 minute))
    (let [message (str minute " is an invalid minute")]
          (throw (ex-info message {:value minute
                                   :conflict :minute
                                   :time time})))
    (.with time ChronoField/MINUTE_OF_HOUR minute)))

(defn with-second 
  "a time with second set to value unless the remaining values would
  render it an invalid time for that second. The time will be at the 
  later occation when hitting an overlap. The time is not adjusted 
  in other ways. An ExceptionInfo with :conflict having value 
  :second would be thrown on erroneous conditions"
  [^Temporal time ^Integer second]
  (if (or (< 59 second) (> 0 second))
    (let [message (str  second " is an invalid second")]
          (throw (ex-info message {:value second
                                   :conflict :second
                                   :time time})))
    (do (when-not (has-hour-minute-second? time)
          (throw (ex-info (str "Type " (class time)
                               " does not have hour minutes or seconds")
                          {:value time
                           :conflict :class})))
        (.with time ChronoField/SECOND_OF_MINUTE second))))

(defn with-millis 
  "a time with millis set to value unless the remaining values would
  render it an invalid time for that time. The time is not adjusted 
  in other ways. An ExceptionInfo with :conflict having value 
  :millis would be thrown on erroneous conditions. Note that this 
  represents fractions of a second and would overwrite the nanos"
  [^Temporal time ^Integer millis]
  (if (or (< 999 millis) (> 0 millis))
    (let [message (str millis " is an invalid millisecond")]
          (throw (ex-info message {:value millis
                                   :conflict :millis
                                   :time time})))
    (.with time ChronoField/MILLI_OF_SECOND millis)))

(defn with-nano
  "a time with nanos set to value unless the remaining values would
  render it an invalid time for that time. The time is not adjusted 
  in other ways. An ExceptionInfo with :conflict having value 
  :nano would be thrown on erroneous conditions. Note that this 
  represents fractions of a second and would overwrite the millis"
  [^Temporal time ^Integer nanos]
  (if (or (< 999999999 nanos) (> 0 nanos))
    (let [message (str nanos " is an invalid nanosecond")]
          (throw (ex-info message {:value nanos
                                   :conflict :nano
                                   :time time})))
    (.with time ChronoField/NANO_OF_SECOND nanos)))


(def ^ZonedDateTime epoch 
  (ZonedDateTime/ofInstant Instant/EPOCH (ZoneId/systemDefault)))

(defn of
  "Creates a time with default values on missing fields"
  ([year month day hour min sec & [options]] 
   (-> epoch
       (.withHour 0)
       (with-year year options)
       (with-month month options)
       (with-day day options) 
       (with-hour hour options) 
       (with-minute min options)
       (with-second sec)))
  ([year month day hour min]
   (of year month day hour min 0))
  ([year month day hour]
   (of year month day hour 0 0))
  ([year month day]
   (of year month day 0 0 0))
  ([year month]
   (of year month 1 0 0 0))
  ([year]
   (of year 1 1 0 0 0)))


   



(defn- order?
  ([c a b] (c 0 (compare a  b)))
  ([c a b & xs]
   (->> (cons a (cons b xs))
        (reduce
         (fn [acc x]
           (if (order? c acc x)
             x
             (reduced :no))))
        (not= :no))))

(defn ordered?
  "Is the supplied values in increasing order, including consequtive equals"
  ([] true)
  ([a] true)
  ([a b] (order? >= a b))
  ([a b & xs] (apply order? >= a b xs)))

(defn increasing?
  "Is the supplied values in increasing order"
  ([] true)
  ([a] true)
  ([a b] (order? > a b))
  ([a b & xs] (apply order? > a b xs)))

(defn reversed?
  "Is the supplied values in decreading order, including consequtive equals"
  ([] true)
  ([a] true)
  ([a b] (order? <= a b))
  ([a b & xs] (apply order? <= a b xs)))

(defn decreasing?
  "Is the supplied values in decreading order"
  ([] true)
  ([a] true)
  ([a b] (order? < a b))
  ([a b & xs] (apply order? < a b xs)))


(defn add-duration
  "a time changed by duration time"
  [^Temporal time ^Duration duration]
  (. time plus duration))

(defn add-seconds
  [^Temporal time ^Integer seconds]
  (if (.isSupported time ChronoUnit/SECONDS)
    (. time plus seconds ChronoUnit/SECONDS)
    (throw (ex-info (str "Type "  (class time)
                         " does not support seconds") {:conflict :class
                                                 :value time}))))

#_(defn remove-seconds [time seconds]
  (. time minusSeconds seconds))

(defn add-days 
  "a time with days added"
   [^Temporal time ^Integer days]
  (if (.isSupported time ChronoUnit/DAYS)
    (. time plus days ChronoUnit/DAYS)
    (throw (ex-info (str "Type "  (class time)
                         " does not support seconds")
                    {:conflict :class
                     :value time}))))


#_(defn remove-days [time days]
  (. time minusDays days))

(defn add-hours
  "a time with hours added"
  [^Temporal time ^Integer hours]
  (if (.isSupported time ChronoUnit/HOURS)
    (. time plus hours ChronoUnit/HOURS)
    (throw (ex-info (str "Type "  (class time)
                         " does not support hours")
                    {:conflict :class
                                               :value time}))))

#_(defn remove-days [time days]
  (. time minusDays days))

(defn add-minutes
  "a time with minutes added"
  [^Temporal time ^Integer minutes]
  (if (.isSupported time ChronoUnit/MINUTES)
    (. time plus minutes ChronoUnit/MINUTES)
    (throw (ex-info (str "Type " (class time)
                         " does not support minutes")
                    {:conflict :class
                                                 :value time}))))

(defn add-millis
  "a time with millis added"
    [^Temporal time ^Integer millis]
  (if (.isSupported time ChronoUnit/MILLIS)
    (. time plus millis ChronoUnit/MILLIS)
    (throw (ex-info (str "Type " (class time)
                         " does not support millis")
                    {:conflict :class
                     :value time}))))

(defn add-nanos
  "a time with nanos added"

   [^Temporal time ^Integer nanos]
  (if (.isSupported time ChronoUnit/NANOS)
    (. time plus nanos ChronoUnit/NANOS)
    (throw (ex-info  (str "Type " (class time)
                          " does not support nanos") {:conflict :class
                                                 :value time}))))

(defn add-years 
  "a time with years added"
  [^Temporal time ^Integer years]
  (if (.isSupported time ChronoUnit/YEARS)
    (. time plus years ChronoUnit/YEARS)
    (throw (ex-info (str "Type " (class time)
                     " does not support years") {:conflict :class
                                                 :value time}))))

(defn just-after [^Temporal time]
  "a time just after supplied"
  (cond (.isSupported time ChronoUnit/NANOS )
        (.plus time 1 ChronoUnit/NANOS
               )
        (.isSupported time ChronoUnit/MILLIS)
        (.plus time 1 ChronoUnit/MILLIS
               )
        (.isSupported time ChronoUnit/SECONDS)
        (.plus time 1 ChronoUnit/SECONDS
               )
        :else (throw (ex-info  (str "Type " (class time)
                                    " does not support just after")
                               {})))
  )

(defn just-before [^Temporal  time]
  (cond (.isSupported time ChronoUnit/NANOS )
        (.plus time -1 ChronoUnit/NANOS
               )
        (.isSupported time ChronoUnit/MILLIS)
        (.plus time -1 ChronoUnit/MILLIS
               )
        (.isSupported time ChronoUnit/SECONDS)
        (.plus time -1 ChronoUnit/SECONDS
               )
        :else (throw (ex-info  (str "Type " (class time)
                                    " does not support just before")
                               {}))))

#_(defn previous-day [^ZonedDateTime time]
  (. time minusDays 1))

#_(defn next-day [^ZonedDateTime time]
  (. time plusDays 1))

(defn time-part-of
  "The time part, except the date part"
  [^Temporal date-time]
  (.query date-time (TemporalQueries/localTime)))

(defn time-part
  "a time part, without date part, from its components.
  Use add-millis, add-nanos or add-duration to add fractions of a second"
  ([h m]
   (time-part h m 0))
  ([h m s]
   (cond (or (> 0 h) (< 23 h))
         (let [message (str h " is a invalid value for hour")]
           (throw (ex-info message {:value h
                                    :conflict :hour})))
         (or (> 0 m) (< 59 m))
         (let [message (str m " is a invalid value for minute")]
           (throw (ex-info message {:value m
                                    :conflict :minute})))
         (or (> 0 s) (< 59 s))
         (let [message (str s " is a invalid value for second")]
           (throw (ex-info message {:value s
                                    :conflict :second})))
         :else
         (LocalTime/of h m s))))

(defn date-part-of "date part of time, without fractions of day"
  [^Temporal time]
  (.query time (TemporalQueries/localDate)))

(defn with [^Temporal time ^TemporalAdjuster adjuster]
  (.with time adjuster))

(defn with-time-part [^Temporal time ^TemporalAdjuster time-part & [{:keys [adjust]}]]
  (let [a (. time with time-part)
        new-time-part (time-part-of a)]
    (if (= time-part new-time-part)
      a
      (if adjust
        a
        (throw (ex-info (str time-part " is not a valid time for "
                             (date-part-of time) ". It would be adjusted to "
                             new-time-part)
                        {:value time-part
                         :conflict :time-part}))))))
(defn date-part
  "date part from its components"
  [y m d & [options]]
  (date-part-of (of y m d 0 0 0 options)))

(defn with-date-part [^Temporal time ^Temporal date & [{:keys [adjust]}]]
  (if-let [dp (date-part-of date)]
    (let [a (. time with dp)]
      (if adjust
        a
        (let [new-date-part (date-part-of a)]
          (cond
            (not (= dp new-date-part))
            (throw (ex-info (str dp " is not a valid date for "
                                 time ". It would be adjusted to " (date-part-of a) ".")
                            {:value date-part
                             :conflict :date-part}))
            (not (= (time-part-of time) (time-part-of a)))
            (throw (ex-info (str dp " is not a valid date for "
                                 time ". Time part would be adjusted to " (time-part-of a) ".")
                            {:value date-part
                             :conflict :date-part}))
            :else a))))
    (ex-info (str  "supplied date " date " does not contain a date")
             {:value date
              :conflict :class})))

(defn with-time-zone [^Temporal time ^ZoneId time-zone]
  (let [date-part (.query time (TemporalQueries/localDate))
        time-part (.query time (TemporalQueries/localTime))]
    (when-not date-part
      (throw (ex-info (str "There is no date in " (class time))
                      {:value time
                       :conflict :class})))
    (when-not time-part
      (throw (ex-info (str "There is no time in " (class time))
                      {:value time
                       :conflict :class})))
    (let [value (ZonedDateTime/of date-part time-part time-zone) 
          datetime-of-value (. value toLocalDateTime)
          local-of-input (LocalDateTime/of date-part time-part)]
      (if (= local-of-input datetime-of-value)
        value
        (throw (ex-info (str local-of-input " is not valid in "
                             time-zone " and would become "
                             datetime-of-value ".")
                        {:value time-zone
                         :conflict :time-zone
                         :time time}))))))

(defn default-time-zone []
  (ZoneId/systemDefault))

(defn adjust-to-time-zone [^Temporal time ^ZoneId time-zone]
  (let [current-zone (or (.query time (TemporalQueries/zone))
                         (default-time-zone))
        
        current-date (.query time (TemporalQueries/localDate))
        current-time (.query time (TemporalQueries/localTime))]
    (when-not current-time
      (throw (ex-info (str "There is no time in " (class time))
                      {:value time
                       :conflict :class})))
    (when-not current-date
      (throw (ex-info (str "There is no date in " (class time))
                      {:value time
                       :conflict :class})))
    (let [dt (LocalDateTime/of current-date current-time)]
        (if (instance? ZoneId current-zone)
          (.withZoneSameInstant (.atZone dt ^ZoneId current-zone) time-zone)
          (.atZoneSameInstant (.atOffset dt current-zone) time-zone)))))


(defn without-time-zone [^TemporalAccessor time]
  (let [date (.query time (TemporalQueries/localDate))
        time-part (.query time (TemporalQueries/localTime))
        zone (.query time (TemporalQueries/zone))]
    (if zone
      (cond (and date time-part) (LocalDateTime/of date time-part)
            date date
            time-part time-part)
      time)
    ))

(defn ^ZoneId time-zone-of [^TemporalAccessor time]
  (.query time (TemporalQueries/zoneId)))

(defn time-zone [name]
  (try (ZoneId/of name ZoneId/SHORT_IDS)
       (catch ZoneRulesException e
         (throw (ex-info (.getMessage e)
                         {:value name
                          :conflict :time-zone})))
       (catch DateTimeException e
         (throw (ex-info (.getMessage e)
                         {:value name
                          :conflict :time-zone})))))

(defn- those-matching-all-case-insensitive [data [first-string & remaining]]
  (let [pattern (Pattern/compile first-string
                                 (bit-or Pattern/LITERAL
                                         Pattern/CASE_INSENSITIVE
                                         Pattern/UNICODE_CASE))
        matched (filter #(re-find pattern %)
                        data)]
    (if (and (not (empty? matched))
             remaining)
      (recur matched remaining)
      matched)))

(defn- single-value [[x & more]]
  (when (and x (not more))
    x))


(defn find-time-zone
  "Locate a time-zone by name. The supplied case insensitive name may be a,
  or many fractions of the name, seperated by blank space that makes the match
  unique. A text matching many time-zones renders a nil value"
  [name]

  (let [n (string/trim name)]
    (cond (string/blank? n)                   nil
          (#{"Z" "z"} n)                      (ZoneId/of n)
          (= 1 (count n))                     nil
          (#{"GMT" "UTC" "UT"} n)             (ZoneId/of n)
          (some #(string/starts-with? n %)
                ["+" "-" "UTC+" "UTC-"
                 "GMT+" "GMT-" "UT+" "UT-"]) (ZoneId/of n)
          :else
          (let [names   (string/split name #"\s+")
                data    (set/union (set (ZoneId/getAvailableZoneIds))
                                   (set (.keySet ZoneId/SHORT_IDS)))]
            (-> (those-matching-all-case-insensitive data names)
                (single-value)
                (some-> (ZoneId/of)))))))



(defn with-earlier-at-overlap
  "pick the earlier alternative if time is at overlap"
  [^ZonedDateTime  t]
  (.withEarlierOffsetAtOverlap t))

(defn with-later-at-overlap
  "pick the later alternative if time is at overlap"
  [^ZonedDateTime t]
  (.withLaterOffsetAtOverlap t))

(def monday DayOfWeek/MONDAY)
(def tuesday DayOfWeek/TUESDAY)
(def wednesday DayOfWeek/WEDNESDAY)
(def thursday DayOfWeek/THURSDAY)
(def friday DayOfWeek/FRIDAY)
(def saturday DayOfWeek/SATURDAY)
(def sunday DayOfWeek/SUNDAY)

(defn day-of-week? [object]
  (instance? DayOfWeek object))

(defn has-day-of-week? [^TemporalAccessor time]
  (.isSupported time ChronoField/DAY_OF_WEEK))

(defn day-of-week [^TemporalAccessor timestamp]
  (if (has-day-of-week? timestamp)
    (DayOfWeek/from timestamp)
    (throw (ex-info (str "Type  " (class timestamp) " does not have day of week." )
                       {:value timestamp
                        :conflict :class}))))

(defn day-of-week-nr [^DayOfWeek day-of-week]
  (.getValue day-of-week))

(defn day-of-week-from [x]
  (cond (int? x)
        (DayOfWeek/of x)
        (string? x)
        (DayOfWeek/valueOf x)))

(defn adjust-day-of-week [^Temporal time ^DayOfWeek day]
  (if (has-day-of-week? time)
    (. time with day)
    (throw (ex-info (str "Type  " (class time) " does not have day of week." )
                       {:value time
                        :conflict :class}))))

(defn has-year? [^TemporalAccessor time]
  (.isSupported time ChronoField/YEAR))

(defn has-month? [^TemporalAccessor time]
  (.isSupported time ChronoField/MONTH_OF_YEAR))

(defn days-of-month [^TemporalAccessor time]
    (-> (YearMonth/from time)
      (. lengthOfMonth)))

(defn first-day-of-month [^Temporal time]
  (when-not (has-year-month-day? time))
  (.with time ChronoField/DAY_OF_MONTH 1))



(defn begining-of-month [^Temporal time]
  (if (and (has-hour-minute-second? time)
           (has-year-month-day? time))
    (let [^Temporal wtp (with-time-part time LocalTime/MIDNIGHT)]
      (.with wtp ChronoField/DAY_OF_MONTH 1))
      (throw (ex-info (str "Type  " (class time) " does not have date and time." )
                       {:value time
                        :conflict :class}))))


(defn end-of-month [^Temporal time]
  (if (and (has-hour-minute-second? time)
           (has-year-month-day? time))
    (let [first-next-month (MonthDay/of (inc (.get time (ChronoField/MONTH_OF_YEAR)))
                                        1)]
      (-> time
          (.with ChronoField/DAY_OF_MONTH (.getDayOfMonth first-next-month))
          (.with ChronoField/MONTH_OF_YEAR (.getMonthValue first-next-month))
          (with-time-part LocalTime/MIDNIGHT)
          (just-before)))
    (throw (ex-info (str "Type  " (class time) " does not have date and time." )
                       {:value time
                        :conflict :class}))))

(defn next-same-day-of-week [^TemporalAdjuster time ^DayOfWeek day-of-week]
  (-> (. time adjustInto day-of-week)
      (add-days 7)))

(defn previous-same-day-of-week [^TemporalAdjuster time ^DayOfWeek day-of-week]
  (let [^TemporalAdjuster yesterday (add-days time -1)]
    (. yesterday adjustInto day-of-week)))

(defn transitions
  "Sequence of time transitions after time, usually DST transistions,
  in pairs of time just before and after transition"
  [^TemporalAccessor time]
  (if-let [zoneId (time-zone-of time)]
    (let [rules (.getRules zoneId)
          producer (fn producer [^ZonedDateTime t]
                     (when-let [transition (. rules nextTransition (. t toInstant))]
                       (let [localBefore (-> (. transition getDateTimeBefore)
                                             (just-before)) 
                             localAfter (. transition getDateTimeAfter)
                             before (ZonedDateTime/of localBefore zoneId)
                             after (ZonedDateTime/of localAfter,  zoneId)]
                         
                         (cons [before  after] (lazy-seq (producer after))))))]
      (producer time))
    (throw (ex-info (str "Type  " (class time) " does not have time zone." )
                       {:value time
                        :conflict :class}))))


(defn of-time-zone
  "Creates a time with default values on missing fields"
  ([year month day hour min sec time-zone & [options]] 
   (-> epoch
       (with-time-zone time-zone)
       (with-hour 0)
       (with-year year options)
       (with-month month options)
       (with-day day options) 
       (with-hour hour options) 
       (with-minute min options)
       (with-second sec)))
  ([year month day hour min time-zone]
   (of-time-zone year month day hour min 0 time-zone))
  ([year month day hour time-zone]
   (of-time-zone year month day hour 0 0 time-zone))
  ([year month day time-zone]
   (of-time-zone year month day 0 0 0 time-zone))
  ([year month time-zone]
   (of-time-zone year month 1 0 0 0 time-zone))
  ([year time-zone]
   (of-time-zone year 1 1 0 0 0 time-zone)))


(defn day-of-year
  "days since beginning of year of time, where 1 is january 1"
  [^TemporalAccessor time]
  (.get time ChronoField/DAY_OF_YEAR))

(defn week-nr
  "ISO week number of time. Commonly used in Europe"
  [^TemporalAccessor time]
   (let [^TemporalAccessor date (date-part-of time)
         weekOfYear (.weekOfWeekBasedYear (WeekFields/ISO))]
     (. date get weekOfYear)))

(defn start-of-week-nr [year week & [{:keys [adjust]}]]
  (if (< week 1)
    (throw (ex-info "Week-nr need to to be 1 or higher"
                    {:value week
                     :conflict :week}))
      (let [first-week (adjust-day-of-week (of year 1 4) monday)
            start-of-week (add-days first-week (* 7 (- week 1)))]
        (if (= week (week-nr start-of-week))
          start-of-week
          (let [last-week (-> (start-of-week-nr (+ year 1) 1)
                              (add-days -1)
                              (week-nr))
                start-of-last (start-of-week-nr year last-week)]
            (if adjust
              start-of-last
              (throw (ex-info (str "Last week of " year " is " last-week
                                   " starting at " start-of-last)
                              {:value week
                               :conflict :week}))))))))
