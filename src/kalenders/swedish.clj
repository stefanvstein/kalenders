(ns kalenders.swedish
  (:require [kalenders.easter :as easter]
            [kalenders.time :as time]
            [kalenders.macros :refer :all]))

(defn julafton? [time]
  (let [[_ m d] (time/year-month-day time)]
    (and (= 12 m) (= 24 d))))

(defn nyårsafton? [time]
  (let [[_ m d] (time/year-month-day time)]
    (and (= 12 m) (= 31 d))))

(defn påskafton? [time]
  (let [[y m d] (time/year-month-day time)
        easter (easter/easter y)]
    (= (time/date-part-of time)
       (time/date-part-of (time/add-days easter -1)))))

(defn midsommarafton? [time]
  (let [[_ m d] (time/year-month-day time)]
    (and (= 6 m)
         (and (| d >= 19)
              (| d <= 25))
         (= time/friday (time/day-of-week time)))))

(defn alla-helgons-dag? [time]
  (let [[_ m d] (time/year-month-day time)]
    (and (= time/saturday (time/day-of-week time))
         (or (and (= 11 m)
                  (and (| d >= 1)
                       (| d <= 6)))
             (and (= 10 m)
                  (= 31 d))))))

(defn alla-helgons-afton? [time]
  (let [[_ m d] (time/year-month-day time)]
    (and (= time/friday (time/day-of-week time))
         (or (and (= 11 m)
                  (and (| d >= 1)
                       (| d <= 5)))
             (and (= 10 m)
                  (or (= 30 d)
                      (= 31 d)))))))

(defn midsommardag? [time]
  (let [[_ m d] (time/year-month-day time)]
    (and (= 6 m)
         (and (| d >= 20)
              (| d <= 26))
         (= time/saturday (time/day-of-week time)))))

(defn nyårsdag? [time]
  (let [[_ m d] (time/year-month-day time)]
    (and (= 1 m) (= 1 d))))

(defn juldag? [time]
  (let [[_ m d] (time/year-month-day time)]
    (and (= 12 m) (= 25 d))))

(defn annandag-jul? [time]
  (let [[_ m d] (time/year-month-day time)]
    (and (= 12 m) (= 26 d))))

(defn trettondag? [time]
  (let [[_ m d] (time/year-month-day time)]
    (and (= 1 m) (= 6 d))))

(defn trettondag? [time]
  (let [[_ m d] (time/year-month-day time)]
    (and (= 1 m) (= 6 d))))

(defn trettondagsafton? [time]
  (let [[_ m d] (time/year-month-day time)]
    (and (= 1 m) (= 6 d))))

(defn valborgsmässoafton? [time]
  (let [[y m d] (time/year-month-day time)]
    (and (= 4 m) (= 30 d))))

(defn första-maj? [time]
  (let [[y m d] (time/year-month-day time)]
    (and (| y >= 1939) (= 5 m) (= 1 d))))

(defn nationaldag? [time]
  (let [[y m d] (time/year-month-day time)]
    (and (| y >= 2005) (= 6 m) (= 6 d))))

(defn pingstafton? [time]
  (let [[y m d] (time/year-month-day time)
        easter (easter/easter y)
        [_ mm dd] (time/year-month-day (time/add-days easter 48))]
    (and (= mm  m)
         (= dd d))))

(defn pingstdag? [time]
  (let [[y m d] (time/year-month-day time)
        easter (easter/easter y)
        [_ mm dd] (time/year-month-day (time/add-days easter 49))]
    (and (= mm  m)
         (= dd d))))

(defn långfredag? [time]
  (let [[y m d] (time/year-month-day time)
        easter (easter/easter y)
        [_ mm dd] (time/year-month-day (time/add-days easter -1))]
    (and (= mm  m)
         (= dd d))))

(defn skärtorsdag? [time]
  (let [[y m d] (time/year-month-day time)
        easter (easter/easter y)
        [_ mm dd] (time/year-month-day (time/add-days easter -2))]
    (and (= mm  m)
         (= dd d))))

(defn påskdag? [time]
  (let [[y m d] (time/year-month-day time)
        easter (easter/easter y)
        [_ mm dd] (time/year-month-day easter)]
    (and (= mm  m)
         (= dd d))))

(defn annandag-påsk? [time]
  (let [[y m d] (time/year-month-day time)
        easter (easter/easter y)
        [_ mm dd] (time/year-month-day (time/add-days easter 1))]
    (and (= mm  m)
         (= dd d))))

(defn  kristi-himmelsfärdsdag? [time]
  (let [[y m d] (time/year-month-day time)
        easter (easter/easter y)
        [_ mm dd] (time/year-month-day (time/add-days easter 39))]
    (and (= mm  m)
         (= dd d))))

(defn helgdagsafton? [time]
  (or (nyårsafton? time)
      (julafton? time)
      (påskafton? time)
      (midsommarafton? time)
      (pingstafton? time)))

(defn borgerlig-helgdag? [time]
  (let [[y _ _] (time/year-month-day time)]
    (or (första-maj? time)
        (nationaldag? time))))

(defn kristen-helgdag? [time]
  (or (= time/sunday (time/day-of-week time))
      (nyårsdag? time)
      (midsommardag? time)
      (trettondag? time)
      (långfredag? time)
      (påskdag? time)
      (annandag-påsk? time)
      (kristi-himmelsfärdsdag? time)
      (pingstdag? time)
      (juldag? time)
      (annandag-jul? time)
      (alla-helgons-dag? time)))

(defn helgdag? [time]
  (or (kristen-helgdag? time)
      (borgerlig-helgdag? time)))

(defn helg? [time]
  (let [day (time/day-of-week time)]
    (or (= time/saturday day)
        (= time/sunday day))))

(defn söndag-enligt-semesterlagen? [time]
  (or (helgdag? time)
      (midsommarafton? time)
      (julafton? time)
      (nyårsafton? time)))

(defn halvdag? [time]
  (or (trettondagsafton? time)
      (skärtorsdag? time)
      (valborgsmässoafton? time)
      (alla-helgons-afton? time)))
