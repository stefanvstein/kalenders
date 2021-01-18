(ns kalenders.duration
  (:import [java.time Duration]))


(defn of-seconds [^long n]
  (Duration/ofSeconds n))

(defn add-minutes [^Duration duration minutes]
  (. duration plusMinutes minutes))

(defn add-hours [^Duration duration hours]
  (. duration plusHours hours))

(defn add-seconds [^Duration duration seconds]
  (. duration plusSeconds seconds))

(defn add-millis [^Duration duration millis]
  (. duration plusMillis millis))

(defn add-nanos [^Duration duration nanos]
  (. duration plusNanos nanos))

(defn add-days [^Duration duration days]
  (. duration plusDays days))

(defn of [hours minutes seconds] #_(Måste kontolleras)
  (-> (of-seconds seconds)
      (add-minutes minutes)
      (add-hours hours)))

(defn seconds-of [^Duration duration]
  (.getSeconds duration))

(defn negative? [^Duration duration]
  (. duration isNegative))

(defn negated [^Duration duration]
  (. duration negated))

(defn multiplied-by
  ([^Duration duration ^Long n]
   (. duration multipliedBy n))
  ([^Duration duration ^Long n & ns]
   (reduce (fn [^Duration a ^Long x] (. a multipliedBy x))
           (multiplied-by duration n)
           ns)))

(defn divided-by
  ([^Duration duration ^Long n]
   (. duration dividedBy n))
  ([^Duration duration ^Long n & ns]
   (reduce (fn [^Duration a ^Long x] (. a dividedBy x))
           (divided-by duration n)
           ns)))

(defn abs [^Duration duration]
  (. duration abs))

(defn of-hours [^Duration n]
  (Duration/ofHours n))

(defn hours-of [^Duration duration]
  (.toHours duration))

(defn minutes-of [^Duration duration]
  (.toMinutes duration))

(defn millis-of [^Duration duration]
  (.toMillis duration))

(defn nanos-of [^Duration duration]
  (.toNanos duration))

(defn days-of [^Duration duration]
  (.toDays duration))

 
(defn plus
  ([^Duration a ^Duration b]
   (. a plus b))
  ([^Duration a ^Duration b & xs]
   (reduce (fn [^Duration acc ^Duration x]
             (. acc plus x))
           (. a plus b)
           xs)))

(defn minus
  ([^Duration a ^Duration b]
   (. a minus b))
  ([^Duration a ^Duration b & xs]
   (reduce (fn [^Duration acc ^Duration x] (minus acc x))
           (minus a b)
           xs)))

(defn hours-minutes-seconds-of [^Duration duration]
  (let [hours (hours-of duration)
        rem (. duration minusHours hours)
        minutes (minutes-of rem)
        rem (. rem minusMinutes minutes)
        seconds (seconds-of rem)]
    [hours minutes seconds]))

(defn days-hours-minutes-seconds-of [^Duration duration]
  (let [days (days-of duration)
        rem (. duration minusDays days)
        hours (hours-of rem)
        rem (. rem minusHours hours)
        minutes (minutes-of rem)
        rem (. rem minusMinutes minutes)
        seconds (seconds-of rem)]
     [days hours minutes seconds]))

(defn days-hours-minutes-seconds-millis-of [^Duration duration]
  (let [days (days-of duration)
        rem (. duration minusDays days)
        hours (hours-of rem)
        rem (. rem minusHours hours)
        minutes (minutes-of rem)
        rem (. rem minusMinutes minutes)
        seconds (seconds-of rem)
        rem (. rem minusSeconds seconds)
        millis (millis-of rem)]
    [days hours minutes seconds millis]))
  

(defn seconds-millis-of [^Duration duration]
  (let [seconds (seconds-of duration)
        rem (. duration minusSeconds seconds)
        millis (millis-of rem)]
    [seconds millis]))

(defn seconds-nanos-of [^Duration duration]
  (let [seconds (seconds-of duration)
        rem (. duration minusSeconds seconds)
        nanos (nanos-of rem)]
    [seconds nanos]))

(defn increasing-order
  ([] true)
  ([_] true)
  ([a b]
   (>= 0 (compare a  b)))
  ([a b & xs]
   (->> (cons a (cons b xs))
        (reduce
         (fn [acc x]
           (if (increasing-order acc x)
             x
             (reduced :no))))
        (not= :no))))
    
(defn decreasing-order
  ([] true)
  ([_] true)
  ([a b]
   (<= 0 (compare a b)))
  ([a b & xs]
   (->> (cons a (cons b xs))
        (reduce
         (fn [acc x]
           (if (decreasing-order acc x)
             x
             (reduced :no))))
        (not= :no))))

(defn between [a b]
  (Duration/between a b))

(defn none? [^Duration n]
  (.isZero n))

(def none 
  Duration/ZERO)
  
(defn to-iso8601 [^Duration duration]
  (. duration toString))

(defn from-iso8601 [text]
  (. Duration parse text))

(defn as-string [duration]
  (to-iso8601 duration))
