(ns kalenders.time
  (:import [java.time ZonedDateTime Duration Instant ZoneId LocalTime DayOfWeek YearMonth MonthDay LocalDate LocalDateTime DateTimeException ]
           [java.time.zone ZoneRulesException]
           [java.time.temporal ChronoUnit]
           [java.util.regex Pattern])
  (:require [clojure.string :as string]
            [clojure.set :as set]))

(defn now []
  (ZonedDateTime/now))

(defn hour-minute-second
  "a vector with [hours minutes seconds] values of time "
  [time]
  [(.getHour time) (.getMinute time) (.getSecond time)])

(defn hour-minute-second-millis
  "a vector with [hours minutes seconds millis] values of time "
  [time]
  [(.getHour time)
   (.getMinute time)
   (.getSecond time)
   (int (/ (.getNano time) 1000000))])

(defn hour-minute-second-nanos
  "a vector with [hours minutes seconds nanos] values of time "
  [time]
  [(.getHour time) (.getMinute time) (.getSecond time) (.getNano time)])

(defn year-month-day
  "a vector with [year month day] values of time "
  [time]
  [(.getYear time) (.getMonthValue time) (.getDayOfMonth time)])

(defn with-year
  "a time with year set to value unless the remaining values would render
  it an invalid time for that year. The time will be at the later occation
  when hitting an overlap. The time is not adjusted in other ways. An
  ExceptionInfo with :conflict having value :year would be thrown on
  erroneous conditions"
  [time year]
  (let [[y month day] (year-month-day time)
        year-month (YearMonth/of year month)
        hour (. time getHour)]
    
    (if (.isValidDay year-month day)  
      (let [new-time (.withYear time year)]
        (if (= hour (.getHour new-time))
          new-time
          (let [message (str (LocalDate/from time)
                         " can not have year changed to " year
                         " since there is a gap at " (LocalTime/from time) ".")]
            (throw (ex-info message {:value year
                                     :conflict :year
                                     :time time})))))
      (let [message (str (LocalDate/from time)
                         " can not have year changed to " year
                         " since " day " is not a valid day of "
                         year-month)]
        (throw (ex-info message {:value year
                                 :conflict :year
                                 :time time}))))))
(defn with-month
  "a time with month set to value unless the remaining values would
  render it an invalid time for that month. The time will be at the
  later occation when hitting an overlap. The time is not adjusted
  in other ways. An ExceptionInfo with :conflict having value :month
  would be thrown on erroneous conditions"
  [time month]
  (if (or (< 12 month) (> 1 month))
    (throw (ex-info (str month " is not a valid value for month") 
                    {:value month
                     :conflict :month
                     :time time}))
    (let [[year m day] (year-month-day time)
          year-month (YearMonth/of year month)
          hour (. time getHour)]
      
      (if (.isValidDay year-month day)  
        (let [new-time (.withMonth time month)]
          (if (= hour (.getHour new-time))
            new-time
            (let [message (str (LocalDate/from time)
                               " can not have month changed to " month
                               " since there is a gap at " (LocalTime/from time) ".")]
              (throw (ex-info message {:value month
                                       :conflict :month
                                       :time time})))))
        (let [message (str (LocalDate/from time)
                           " can not have month changed to " month
                           " since " day " is not a valid day of "
                           year-month)]
          (throw (ex-info message {:value month
                                   :conflict :month
                                   :time time})))))))
(defn with-day
  "a time with day set to value unless the remaining values would render
  it an invalid time for that day. The time will be at the later
  occation when hitting an overlap. The time is not adjusted in other
  ways. An ExceptionInfo with :conflict having value :day would be thrown
  on erroneous conditions"
  [time day]
  (let [[year month d] (year-month-day time)
        hour (. time getHour)
        year-month (YearMonth/of year month)]
    
    (if (.isValidDay year-month day)  
      (let [new-time (.withDayOfMonth time day)]
        (if (= hour (.getHour new-time))
          new-time
          (let [message (str (LocalDate/from time)
                         " can not have day changed to " day
                         " since there is a gap at " (LocalTime/from time) ".")]
            (throw (ex-info message {:value day
                                     :conflict :day
                                     :time time})))))
      (let [message (str (LocalDate/from time)
                         " can not have day changed to " day
                         " since " day " is not a valid day of "
                         year-month)]
        (throw (ex-info message {:value day
                                 :conflict :day
                                 :time time}))))))

(defn with-hour
  "a time with hour set to value unless the remaining values would
  render it an invalid time for that hour. The time will be at the
  later occation when hitting an overlap. The time is not adjusted
  in other ways. An ExceptionInfo with :conflict having value :hour
  would be thrown on erroneous conditions"
  [time hour]
  (if (or (< 23 hour) (> 0 hour))
    (let [message (str (LocalDateTime/from time)
                           " can not have hour changed to " hour
                           " since " hour " is an invalid hour")]
          (throw (ex-info message {:value hour
                                   :conflict :hour
                                   :time time})))
    (let [new-time (.withHour time hour)]
      (if (= hour (.getHour new-time))
        new-time
        (let [message (str (LocalDateTime/from time)
                           " can not have hour changed to " hour
                           " since there is a gap.")]
          (throw (ex-info message {:value hour
                                   :conflict :hour
                                   :time time})))))))

(defn with-minute 
  "a time with minute set to value unless the remaining values would
  render it an invalid time for that month. The time is not adjusted 
  in other ways. An ExceptionInfo with :conflict having value 
  :month would be thrown on erroneous conditions"
  [time minute]
  (if (or (< 59 minute) (> 0 minute))
    (let [message (str (LocalDateTime/from time)
                           " can not have minute changed to " minute
                           " since " minute " is an invalid minute")]
          (throw (ex-info message {:value minute
                                   :conflict :minute
                                   :time time})))
    (.withMinute time minute)))

(defn with-second
  "a time with second set to value unless the remaining values would
  render it an invalid time for that second. The time will be at the 
  later occation when hitting an overlap. The time is not adjusted 
  in other ways. An ExceptionInfo with :conflict having value 
  :second would be thrown on erroneous conditions"
  [time second]
  (if (or (< 59 second) (> 0 second))
    (let [message (str (LocalDateTime/from time)
                           " can not have second changed to " second
                           " since " second " is an invalid second")]
          (throw (ex-info message {:value second
                                   :conflict :second
                                   :time time})))
    (.withSecond time second)))

(defn with-millis 
  "a time with millis set to value unless the remaining values would
  render it an invalid time for that time. The time is not adjusted 
  in other ways. An ExceptionInfo with :conflict having value 
  :millis would be thrown on erroneous conditions. Note that this 
  represents fractions of a second and would overwrite the nanos"
  [time millis]
  (if (or (< 999 millis) (> 0 millis))
    (let [message (str (LocalDateTime/from time)
                           " can not have millis changed to " millis
                           " since " millis " is an invalid millisecond")]
          (throw (ex-info message {:value millis
                                   :conflict :millis
                                   :time time})))
    (.withNano time (* 1000000 millis))))

(defn with-nano
  "a time with nanos set to value unless the remaining values would
  render it an invalid time for that time. The time is not adjusted 
  in other ways. An ExceptionInfo with :conflict having value 
  :nano would be thrown on erroneous conditions. Note that this 
  represents fractions of a second and would overwrite the millis"
  [time nanos]
  (if (or (< 999999999 nanos) (> 0 nanos))
    (let [message (str (LocalDateTime/from time)
                           " can not have nanos changed to " nanos
                           " since " nanos " is an invalid nanosecond")]
          (throw (ex-info message {:value nanos
                                   :conflict :nano
                                   :time time})))
    (.withNano time nanos)))


(def epoch 
  (ZonedDateTime/ofInstant Instant/EPOCH (ZoneId/systemDefault)))

(defn of
  "Creates a time with default valkues on missing fields"
  ([year month day hour min sec] 
   (-> epoch
       (.withHour 0)
       (with-year year)
       (with-month month)
       (with-day day) 
       (with-hour hour) 
       (with-minute min)
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
  
  ([c a b] (c 0 (. a compareTo b)))
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
  [time duration]
  (. time plus duration))

(defn add-seconds [time seconds]
  "a time with seconds added"
  (. time plusSeconds seconds))

#_(defn remove-seconds [time seconds]
  (. time minusSeconds seconds))

(defn add-days 
  "a time with days added"
  [time days]
  (. time plusDays days))

#_(defn remove-days [time days]
  (. time minusDays days))

(defn add-hours
  "a time with hours added"
  [time hours]
  (. time plusHours hours))

#_(defn remove-days [time days]
  (. time minusDays days))

(defn add-minutes
  "a time with minutes added"
  [time minutes]
  (. time plusMinutes minutes))

(defn add-millis
  "a time with millis added"
  [time millis]
  (. time plusNanos (* 1000000 millis)))

(defn add-nanos
  "a time with nanos added"
  [time nanos]
  (. time plusNanos nanos))

(defn add-years [time years]
  "a time with years added"
  (. time plusYears years))

(defn just-after [time]
  "a time just after supplied"
  (. time plusNanos 1))

(defn just-before [time]
  "a time just before time"
  (. time minusNanos 1))

#_(defn previous-day [time]
  (. time minusDays 1))

#_(defn next-day [time]
  (. time plusDays 1))

(defn time-part-of
  "The time part, except the date part"
  [date-time]
  (.toLocalTime date-time))

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
  [time]
  (.toLocalDate time))

(defn with [time adjuster]
  (. time with adjuster))

(defn with-time-part [time time-part]
  (let [a (. time with time-part)
        new-time-part (time-part-of a)]
    (if (= time-part new-time-part)
      a
      (throw (ex-info (str time-part " is not a valid time for "
                           (date-part-of time) ". It would be adjusted to "
                           new-time-part)
                      {:value time-part
                       :conflict :time-part})))))

(defn with-date-part [time date-part]
  (let [a (. time with date-part)
        new-date-part (date-part-of a)]
    (cond
      (not (= date-part new-date-part))
      (throw (ex-info (str date-part " is not a valid date for "
                           (.toLocalDateTime time) ". It would be adjusted to " (.toLocalDateTime a) ".")
                      {:value date-part
                       :conflict :date-part}))
      (not (= (time-part-of time) (time-part-of a)))
      (throw (ex-info (str date-part " is not a valid date for "
                           (.toLocalDateTime time) ". Time part would be adjusted to " (time-part-of a) ".")
                      {:value date-part
                       :conflict :date-part})))))

(defn with-time-zone [time time-zone])
(defn adjust-to-timezone [time time-zone])

(defn default-time-zone []
  (ZoneId/systemDefault))

(defn time-zone-of [time]
  (ZoneId/of time)) #_(Need som error handling)

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

(def units #{:millenia :century :decade 
             :year :month :day 
             :hour :minute :second 
             :millis :micros})

(defn truncate
  "truncate values until suplied unit"
  [time unit]
  (condp = unit
    :millenia (of (* 1000 (int (/ (.getYear time) 1000))))
    :century (of (* 100 (int (/ (.getYear time) 100))))
    :decade  (of (* 10 (int (/ (.getYear time) 10))))
    :year (of (.getYear time))
    :month (of (.getYear time) (. time getMonthValue))
    :day (.truncatedTo time ChronoUnit/DAYS)
    :hour (.truncatedTo time ChronoUnit/HOURS)
    :minute (.truncatedTo time ChronoUnit/MINUTES)
    :second (.truncatedTo time ChronoUnit/SECONDS)
    :millis (.truncatedTo time ChronoUnit/MILLIS)
    :micros (.truncatedTo time ChronoUnit/MICROS)
    (throw (ex-info (str "Don't know how to truncate to " unit)
                    {:possible units
                     :got unit}))))



(defn date-part
  "date part from its components"
  [y m d]
  (date-part-of (of y m d)))

(defn with-earlier-at-overlap
  "pick the earlier alternative if time is at overlap"
  [t]
  (.withEarlierOffsetAtOverlap t))

(defn with-later-at-overlap
  "pick the later alternative if time is at overlap"
  [t]
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

(defn day-of-week [timestamp]
  (DayOfWeek/from timestamp))

(defn day-of-week-nr [day-of-week]
  (.getValue day-of-week))

(defn day-of-week-from [x]
  (cond (int? x)
        (DayOfWeek/of x)
        (string? x)
        (DayOfWeek/from x)))
(defn daysOfMonth [time]
  (-> (YearMonth/from time)
      (. lengthOfMonth)))
(defn first-day-of-month [time]
  (. time withDayOfMonth 1))
(defn last-day-of-month [time]
  (let [last-day (. (YearMonth/from time) lengthOfMonth)]
    (. time withDayOfMonth last-day)))
(defn begining-of-month [time]
  (-> (. time withLocalTime (LocalTime/MIDNIGHT))
      (. withDayOfMonth 1)))
(defn end-of-month [time]
  (let [first-next-month (MonthDay/of (+ 1 (.getMonthValue time)) 1)]
    (-> (. time with first-next-month)
        (. withLocalTime (LocalTime/MIDNIGHT))
        (just-before))))

(defn next-same-day-of-week [time day-of-week]
  (-> (. time adjustInto day-of-week)
      (add-days 7)))

(defn previous-same-day-of-week [time day-of-week]
  (let [yesterday (add-days time -1)]
    (. yesterday adjustInto day-of-week)))

(defn transitions
  "Sequence of time transitions after time, usually DST transistions,
  in pairs of time just before and after transition"
  [time]
  (let [zoneId (.getZone time)
        rules (.getRules zoneId)
        producer (fn producer [t]
                   (when-let [transition (. rules nextTransition (. t toInstant))]
                     (let [localBefore (-> (. transition getDateTimeBefore)
                                           (just-before)) 
                           localAfter (. transition getDateTimeAfter)
                           before (ZonedDateTime/of localBefore zoneId)
                           after (ZonedDateTime/of localAfter,  zoneId)]
                       
                       (cons [before  after] (lazy-seq (producer after))))))]
    (producer time)))


