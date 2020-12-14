(ns kalenders.core
  "A library for occurrences in time. A occurrence has a begining and a duration. Overlapping occurences are merged into a larger occurence.
   The core occurences function creates sequence of occurences based on a definition and a timestamp, starting with the ongoing occurence or next future occurence in relation to the timestamp, followed by the chronologic continuing.
   A definition is a function that produces a [start-time duration] from a timestamp. Definitions can be combined to produce complex definitions of occurrence sequences"
  (:require [kalenders.daily :as daily]
            [kalenders.duration :as duration]
            [kalenders.searching :as searching]
            [kalenders.time :as time]
            [kalenders.weekly :as weekly]))

(defn combine [definitions]
  (->> (or (seq definitions)
           [searching/ZERO])
       (reduce searching/either)))



             
(defn occurrences [definition timestamp]
  (let [next-occurance (definition timestamp)]
    (if (= searching/ZERO next-occurance)
      nil
      (let [[start duration] next-occurance
            ending (time/add-duration start duration)
            earlier-timestamp (time/just-before start)
            real-beginning  (searching/starting definition earlier-timestamp start ending)
            [real-start real-duration] real-beginning
            ending (time/add-duration real-start real-duration)
            following-timestamp (time/just-after ending)]
        (cons real-beginning
              (searching/periods-forward definition following-timestamp))))))
    
(defn once [from duration]
  (fn [timestamp] (let [end (time/add-duration from duration)]
                    (if  
                      (time/increasing? end timestamp)
                      searching/ZERO
                      [from duration]))))

(defn between [from until]
   (if (time/increasing? until from)
      (between until from)
      (fn between' [timestamp]
        (if (time/increasing? until timestamp)
          searching/ZERO
          [from (duration/between from until)]))))
                    
(defn daily [from-time until-time]
  (fn [t] (daily/daily from-time until-time t)))
          
(defn weekly
  "Returns a occurance "
  [fromDay fromTime untilDay untilTime]
  (fn [t] (weekly/weekly fromDay fromTime untilDay untilTime t)))

(defn year-weekly []
  ;;Vecka 1 är veckan med 4:e januari
                                        ; LocalDate date = LocalDate.of(year, month, day);
                                        ; int weekOfYear = date.get(WeekFields.of(locale).weekOfYear());

  ) 
(defn monthly [])
(defn with [def1 def2])
(comment "Se till att alla tider är until exclusive, nano sekunden innan")

  



