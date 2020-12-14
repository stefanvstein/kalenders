(ns timeing.duration
  (:import [java.time Duration]))


(defn of-seconds [n]
  (Duration/ofSeconds n))

(defn add-minutes [duration minutes]
  (. duration plusMinutes minutes))

(defn add-hours [duration hours]
  (. duration plusHours hours))

(defn add-seconds [duration seconds]
  (. duration plusSeconds seconds))

(defn add-millis [duration millis]
  (. duration plusMillis millis))

(defn add-nanos [duration nanos]
  (. duration plusNanos nanos))

(defn add-days [duration days]
  (. duration plusDays days))

(defn of [hours minutes seconds] #_(Måste kontolleras)
  (-> (of-seconds seconds)
      (add-minutes minutes)
      (add-hours hours)))

(defn seconds-of [duration]
  (.getSeconds duration))

(defn negative? [duration]
  (. duration isNegative))

(defn negated [duration]
  (. duration negated))

(defn multiplied-by
  ([duration n]
   (. duration multipliedBy n))
  ([duration n & ns]
   (reduce (fn [a x] (. a multipliedBy x))
           (multiplied-by duration n)
           ns)))

(defn divided-by
  ([duration n]
   (. duration dividedBy n))
  ([duration n & ns]
   (reduce (fn [a x] (. a dividedBy x))
           (divided-by duration n)
           ns)))

(defn abs [duration]
  (. duration abs))

(defn of-hours [n]
  (Duration/ofHours n))

(defn hours-of [duration]
  (.toHours duration))

(defn minutes-of [duration]
  (.toMinutes duration))

(defn millis-of [duration]
  (.toMillis duration))

(defn nanos-of [duration]
  (.toNanos duration))

(defn days-of [duration]
  (.toDays duration))

 
(defn plus
  ([a b]
   (. a plus b))
  ([a b & xs]
   (reduce (fn [acc x]
             (. acc plus x))
           (. a plus b)
           xs)))

(defn minus
  ([a b]
   (. a minus b))
  ([a b & xs]
   (reduce (fn [acc x] (minus acc x))
           (minus a b)
           xs)))

(defn hours-minutes-seconds-of [duration]
  (let [hours (hours-of duration)
        rem (. duration minusHours hours)
        minutes (minutes-of rem)
        rem (. rem minusMinutes minutes)
        seconds (seconds-of rem)]
    [hours minutes seconds]))

(defn days-hours-minutes-seconds-of [duration]
  (let [days (days-of duration)
        rem (. duration minusDays days)
        hours (hours-of rem)
        rem (. rem minusHours hours)
        minutes (minutes-of rem)
        rem (. rem minusMinutes minutes)
        seconds (seconds-of rem)]
     [days hours minutes seconds]))

(defn days-hours-minutes-seconds-millis-of [duration]
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
  

(defn seconds-millis-of [duration]
  (let [seconds (seconds-of duration)
        rem (. duration minusSeconds seconds)
        millis (millis-of rem)]
    [seconds millis]))

(defn seconds-nanos-of [duration]
  (let [seconds (seconds-of duration)
        rem (. duration minusSeconds seconds)
        nanos (nanos-of rem)]
    [seconds nanos]))

(defn increasing-order
  ([] true)
  ([_] true)
  ([a b]
   (>= 0 (. a compareTo b)))
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
   (<= 0 (. a compareTo b)))
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

(defn none? [n]
  (.isZero n))

(def none 
  Duration/ZERO)
  
(defn to-iso8601 [duration]
  (. duration toString))

(defn from-iso8601 [text]
  (. Duration parse text))

(defn as-string [duration]
  (to-iso8601 duration))
