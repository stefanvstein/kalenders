(ns kalenders.norwegian
  (:require [kalenders.easter :as easter]
            [kalenders.time :as time]
            [kalenders.macros :refer :all]))

(defn første-nyttårsdag? [time]
  (let [[_ m d] (time/year-month-day time)]
    (and (= 1 m) (= 1 d))))

(defn skjærtorsdag? [time]
  (let [[y m d] (time/year-month-day time)
        easter (easter/easter y)
        [_ mm dd] (time/year-month-day (time/add-days easter -2))]
    (and (= mm  m)
         (= dd d))))

(defn langfredag? [time]
  (let [[y m d] (time/year-month-day time)
        easter (easter/easter y)
        [_ mm dd] (time/year-month-day (time/add-days easter -1))]
    (and (= mm  m)
         (= dd d))))

(defn første-påskedag? [time]
  (let [[y m d] (time/year-month-day time)
        easter (easter/easter y)
        [_ mm dd] (time/year-month-day easter)]
    (and (= mm  m)
         (= dd d))))

(defn andre-påskedag? [time]
  (let [[y m d] (time/year-month-day time)
        easter (easter/easter y)
        [_ mm dd] (time/year-month-day (time/add-days easter 1))]
    (and (= mm  m)
         (= dd d))))

(defn kristi-himmelfartsdag? [time]
  (let [[y m d] (time/year-month-day time)
        easter (easter/easter y)
        [_ mm dd] (time/year-month-day (time/add-days easter 39))]
    (and (= mm  m)
         (= dd d))))

(defn første-pinsedag? [time]
  (let [[y m d] (time/year-month-day time)
        easter (easter/easter y)
        [_ mm dd] (time/year-month-day (time/add-days easter 49))]
    (and (= mm  m)
         (= dd d))))

(defn andre-pinsedag? [time]
  (let [[y m d] (time/year-month-day time)
        easter (easter/easter y)
        [_ mm dd] (time/year-month-day (time/add-days easter 50))]
    (and (= mm  m)
         (= dd d))))

(defn første-juledag? [time]
  (let [[_ m d] (time/year-month-day time)]
    (and (= 12 m) (= 25 d))))

(defn andre-juledag? [time]
  (let [[_ m d] (time/year-month-day time)]
    (and (= 12 m) (= 26 d))))

(defn helligdag? [time]
  (or (første-nyttårsdag? time)
      (skjærtorsdag? time)
      (langfredag? time)
      (første-påskedag? time)
      (andre-påskedag? time)
      (kristi-himmelfartsdag? time)
      (første-pinsedag? time)
      (andre-pinsedag? time)
      (første-juledag? time)
      (andre-juledag? time)))

(defn første-mai? [time]
  (let [[_ m d] (time/year-month-day time)]
    (and (= 5 m) (= 1 d))))

(defn syttende-mai? [time]
  (let [[_ m d] (time/year-month-day time)]
    (and (= 5 m) (= 17 d))))

(defn dymmelonsdag? [time]
  (let [[y m d] (time/year-month-day time)
        [_ mm dd] (-> (easter/easter y)
                      (time/add-days -3)
                      (time/year-month-day ))]
    (and (= mm  m)
         (= dd d))))

(defn julaften? [time]
  (let [[_ m d] (time/year-month-day time)]
    (and (= 12 m) (= 24 d))))

(defn nyårsaften? [time]
  (let [[_ m d] (time/year-month-day time)]
    (and (= 12 m) (= 31 d))))

(defn påskeaften? [time]
  (let [[y m d] (time/year-month-day time)
        [_ mm dd] (-> (easter/easter y)
                      (time/add-days -1)
                      (time/year-month-day ))]
    (and (= mm  m)
         (= dd d))))
(defn palmsøndag? [time]
  (let [[y m d] (time/year-month-day time)
        [_ mm dd] (-> (easter/easter y)
                      (time/add-days -7)
                      (time/year-month-day ))]
    (and (= mm  m)
         (= dd d))))

(defn begrenset-arbeidstid? [time]
  (or (dymmelonsdag? time)
      (julaften? time)
      (nyårsaften? time)
      (påskeaften? time)
      (palmsøndag? time)))

(defn morsdag? [time]
  (let [[_ m d] (time/year-month-day time)]
    (and
     (= 2 m)
     (| d >= 8)
     (| d <= 14)
     (= time/sunday (time/day-of-week time)))))

(defn farsdag? [time]
  (let [[_ m d] (time/year-month-day time)]
    (and
     (= 11 m)
     (| d >= 8)
     (| d <= 14)
     (= time/sunday (time/day-of-week time)))))

