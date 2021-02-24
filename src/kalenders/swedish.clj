(ns kalenders.swedish
  (:require [kalenders.easter :as easter]
            [kalenders.time :as time]
            [kalenders.macros :refer :all]))

(defn julafton? [time]
  (let [[_ m d] (time/year-month-day time)]
    (and (= 12 m) (= 24 d)
         :julafton)))

(defn nyårsafton? [time]
  (let [[_ m d] (time/year-month-day time)]
    (and (= 12 m) (= 31 d)
         :nyårsafton)))

(defn påskafton? [time]
  (let [[y m d] (time/year-month-day time)
        easter (easter/easter y)]
    (when (= (time/date-part-of time)
             (time/date-part-of (time/add-days easter -1)))
      :påskafton)))

(defn midsommarafton? [time]
  (let [[y m d] (time/year-month-day time)]
    (and (if (| y >= 1953)
            (and (= 6 m)
                 (| d >= 19)
                 (| d <= 25)
                 (= time/friday (time/day-of-week time)))
            (and (= d 23)
                 (= m 6)))
      :midsommarafton)))

(defn alla-helgons-dag? [time]
  (let [[y m d] (time/year-month-day time)]
    (and (| y >= 1953)
         (= time/saturday (time/day-of-week time))
         (or (and (= 11 m)
                  (and (| d >= 1)
                       (| d <= 6)))
             (and (= 10 m)
                  (= 31 d)))
         :alla-helgons-dag)))

(defn alla-helgons-afton? [time]
  (let [[_ m d] (time/year-month-day time)]
    (and (= time/friday (time/day-of-week time))
         (or (and (= 11 m)
                  (and (| d >= 1)
                       (| d <= 5)))
             (and (= 10 m)
                  (or (= 30 d)
                      (= 31 d))))
         :alla-helogons-afton)))

(defn midsommardag? [time]
  (let [[y m d] (time/year-month-day time)]
    (when (if (| y >= 1953)
            (and (= 6 m)
                 (and (| d >= 20)
                      (| d <= 26))
                 (= time/saturday (time/day-of-week time)))
            (and (= 6 m)
                 (= 24 m)))
      :midsommardag)))

(defn nyårsdag? [time]
  (let [[_ m d] (time/year-month-day time)]
    (and (= 1 m) (= 1 d)
         :nyårsdag)))

(defn juldag? [time]
  (let [[_ m d] (time/year-month-day time)]
    (and (= 12 m) (= 25 d)
         :juldag)))

(defn annandag-jul? [time]
  (let [[_ m d] (time/year-month-day time)]
    (and (= 12 m) (= 26 d)
         :annandag-jul)))

(defn trettondag? [time]
  (let [[_ m d] (time/year-month-day time)]
    (and (= 1 m) (= 6 d)
         :trettondag)))

(defn trettondagsafton? [time]
  (let [[_ m d] (time/year-month-day time)]
    (and (= 1 m) (= 6 d)
         :trettondagsafton)))

(defn valborgsmässoafton? [time]
  (let [[y m d] (time/year-month-day time)]
    (and (= 4 m) (= 30 d)
         :valborgsmässoafton)))

(defn första-maj? [time]
  (let [[y m d] (time/year-month-day time)]
    (and (| y >= 1939) (= 5 m) (= 1 d)
         :första-maj)))

(defn nationaldag? [time]
  (let [[y m d] (time/year-month-day time)]
    (and (| y >= 2005) (= 6 m) (= 6 d)
         :nationaldag)))

(defn pingstafton? [time]
  (let [[y m d] (time/year-month-day time)
        easter (easter/easter y)
        [_ mm dd] (time/year-month-day (time/add-days easter 48))]
    (and (= mm  m)
         (= dd d)
         :pingstafton)))

(defn pingstdag? [time]
  (let [[y m d] (time/year-month-day time)
        easter (easter/easter y)
        [_ mm dd] (time/year-month-day (time/add-days easter 49))]
    (and (= mm  m)
         (= dd d)
         :pingstdag)))

(defn annandag-pingst? [time]
  (let [[y m d] (time/year-month-day time)]
    (and
     (| y <= 2004)
     (let [easter (easter/easter y)
           [_ mm dd] (time/year-month-day (time/add-days easter 50))]
       (and (= mm  m)
            (= dd d)))
     :annandag-pingst)))

(defn långfredag? [time]
  (let [[y m d] (time/year-month-day time)
        easter (easter/easter y)
        [_ mm dd] (time/year-month-day (time/add-days easter -2))]
    (and (= mm  m)
         (= dd d)
         :långfredag)))

(defn skärtorsdag? [time]
  (let [[y m d] (time/year-month-day time)
        easter (easter/easter y)
        [_ mm dd] (time/year-month-day (time/add-days easter -3))]
    (and (= mm  m)
         (= dd d)
         :skärtorsdag)))

(defn påskdag? [time]
  (let [[y m d] (time/year-month-day time)
        easter (easter/easter y)
        [_ mm dd] (time/year-month-day easter)]
    (and (= mm  m)
         (= dd d)
         :påskdag)))

(defn annandag-påsk? [time]
  (let [[y m d] (time/year-month-day time)
        easter (easter/easter y)
        [_ mm dd] (time/year-month-day (time/add-days easter 1))]
    (and (= mm m)
         (= dd d)
         :annandag-påsk)))

(defn  kristi-himmelsfärdsdag? [time]
  (let [[y m d] (time/year-month-day time)
        easter (easter/easter y)
        [_ mm dd] (time/year-month-day (time/add-days easter 39))]
    (and (= mm  m)
         (= dd d)
         :kristi-himmelsfärdsdag)))

(defn helgdagsafton? [time]
  (or (nyårsafton? time)
      (julafton? time)
      (påskafton? time)
      (midsommarafton? time)
      (pingstafton? time)))

(defn borgerlig-helgdag? [time]
    (or (första-maj? time)
        (nationaldag? time)))

(defn kristen-helgdag-förutom-söndag? [time]
  (or (nyårsdag? time)
      (midsommardag? time)
      (trettondag? time)
      (långfredag? time)
      (påskdag? time)
      (annandag-påsk? time)
      (kristi-himmelsfärdsdag? time)
      (pingstdag? time)
      (juldag? time)
      (annandag-jul? time)
      (alla-helgons-dag? time)
      (annandag-pingst? time)))

(defn kristen-helgdag? [time]
  (or (when (= time/sunday (time/day-of-week time))
        :söndag)
      (kristen-helgdag-förutom-söndag? time)))

(defn helgdag? [time]
  (or (kristen-helgdag? time)
      (borgerlig-helgdag? time)))

(defn helg? [time]
  (let [day (time/day-of-week time)]
    (or (when (= time/saturday day) :lördag)
        (when (= time/sunday day) :söndag))))

(defn söndag-enligt-semesterlagen? [time]
  (or (helgdag? time)
      (let [[y _ _] (time/year-month-day time)]
        (and (| y >= 1961)
             (or
              (midsommarafton? time)
              (julafton? time)
              (nyårsafton? time))))))
  
(defn halvdag? [time]
  (or (trettondagsafton? time)
      (skärtorsdag? time)
      (valborgsmässoafton? time)
      (alla-helgons-afton? time)))
