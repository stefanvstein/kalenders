(ns kalenders.norwegian
  (:require [kalenders.easter :as easter]
            [kalenders.time :as time]
            [kalenders.macros :refer :all]))

(defn første-nyttårsdag? [time]
  (let [[_ m d] (time/year-month-day time)]
    (and (= 1 m) (= 1 d) :første-nyttårsdag)))

(defn skjærtorsdag? [time]
  (let [[y m d] (time/year-month-day time)
        easter (easter/easter y)
        [_ mm dd] (time/year-month-day (time/add-days easter -3))]
    (and (= mm  m)
         (= dd d)
         :skjærtorsdag)))

(defn langfredag? [time]
  (let [[y m d] (time/year-month-day time)
        easter (easter/easter y)
        [_ mm dd] (time/year-month-day (time/add-days easter -2))]
    (and (= mm  m)
         (= dd d)
         :langfredag)))

(defn første-påskedag? [time]
  (let [[y m d] (time/year-month-day time)
        easter (easter/easter y)
        [_ mm dd] (time/year-month-day easter)]
    (and (= mm  m)
         (= dd d)
         :første-påskedag)))

(defn andre-påskedag? [time]
  (let [[y m d] (time/year-month-day time)
        easter (easter/easter y)
        [_ mm dd] (time/year-month-day (time/add-days easter 1))]
    (and (= mm  m)
         (= dd d)
         :andre-påskedag)))

(defn kristi-himmelfartsdag? [time]
  (let [[y m d] (time/year-month-day time)
        easter (easter/easter y)
        [_ mm dd] (time/year-month-day (time/add-days easter 39))]
    (and (= mm  m)
         (= dd d)
         :kristi-himmelfartsdag)))

(defn første-pinsedag? [time]
  (let [[y m d] (time/year-month-day time)
        easter (easter/easter y)
        [_ mm dd] (time/year-month-day (time/add-days easter 49))]
    (and (= mm  m)
         (= dd d)
         :første-pinsedag)))

(defn andre-pinsedag? [time]
  (let [[y m d] (time/year-month-day time)
        easter (easter/easter y)
        [_ mm dd] (time/year-month-day (time/add-days easter 50))]
    (and (= mm  m)
         (= dd d)
         :andre-pinsedag)))

(defn første-juledag? [time]
  (let [[_ m d] (time/year-month-day time)]
    (and (= 12 m) (= 25 d) :første-juledag)))

(defn andre-juledag? [time]
  (let [[_ m d] (time/year-month-day time)]
    (and (= 12 m) (= 26 d) :andre-juledag)))

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
    (and (= 5 m) (= 1 d) :første-mai)))

(defn syttende-mai? [time]
  (let [[_ m d] (time/year-month-day time)]
    (and (= 5 m) (= 17 d) :syttende-mai)))

(defn dymmelonsdag? [time]
  (let [[y m d] (time/year-month-day time)
        [_ mm dd] (-> (easter/easter y)
                      (time/add-days -4)
                      (time/year-month-day ))]
    (and (= mm  m)
         (= dd d)
         :dymmelonsdag)))

(defn julaften? [time]
  (let [[_ m d] (time/year-month-day time)]
    (and (= 12 m) (= 24 d) :julaften)))

(defn nyårsaften? [time]
  (let [[_ m d] (time/year-month-day time)]
    (and (= 12 m) (= 31 d) :nyårsaften)))

(defn påskeaften? [time]
  (let [[y m d] (time/year-month-day time)
        [_ mm dd] (-> (easter/easter y)
                      (time/add-days -1)
                      (time/year-month-day ))]
    (and (= mm  m)
         (= dd d)
         :påskeaften)))

(defn palmsøndag? [time]
  (let [[y m d] (time/year-month-day time)
        [_ mm dd] (-> (easter/easter y)
                      (time/add-days -7)
                      (time/year-month-day ))]
    (and (= mm  m)
         (= dd d)
         :palmsøndag)))

(defn fridag? [time]
  (or (første-mai? time)
      (syttende-mai? time)
      (helligdag? time)))

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
     (= time/sunday (time/day-of-week time))
     :morsdag)))

(defn farsdag? [time]
  (let [[_ m d] (time/year-month-day time)]
    (and
     (= 11 m)
     (| d >= 8)
     (| d <= 14)
     (= time/sunday (time/day-of-week time))
     :farsdag)))

