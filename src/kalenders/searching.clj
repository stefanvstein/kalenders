(ns kalenders.searching
  (:require [kalenders.duration :as duration]
            [kalenders.time :as time]))

(defn- end-of [start duration]
  (time/add-duration start duration))

(defn calc  [a-start a-duration b-start b-duration]
  (if (time/increasing? b-start a-start)
    (calc b-start b-duration a-start a-duration)
    (let [a-end (end-of a-start a-duration)
          b-end (end-of b-start b-duration)]
      (cond
        (time/increasing? a-end b-start) [a-start, a-duration]
        (time/increasing? b-end a-end)  [a-start a-duration]
        :else [a-start (duration/between a-start b-end)]))))
                                        

(defn minimum
  ([a b]
    (if (>= 0 (compare a b)) a b))
  ([a b & cs] 
   (reduce minimum a (cons b cs))))


(defn maximum
  ([a b]
   (if (<= 0 (compare a b)) a b))
  ([a b & cs]
   (reduce maximum a (cons b cs))))
           
    
   
(def ZERO [time/epoch
           duration/none])   

(defn find-closest [a-definition b-definition timestamp]
  (let [[a-time a-dur :as a] (a-definition timestamp)
        [b-time b-dur :as b] (b-definition timestamp)]
    (cond
      (and (duration/none? a-dur)
           (duration/none? b-dur)) ZERO
      (duration/none? a-dur) b
      (duration/none? b-dur) a
      :otherwise (calc a-time a-dur b-time b-dur))))
                   
           
(defn discover [a-definition b-definition current timestamp]
  
    (let [[current-start current-duration] current]
      (if (duration/none? current-duration)
        ZERO
        (let [[d-start d-duration] (find-closest a-definition b-definition timestamp)]
          (if (duration/none? d-duration)
            current
            (let [current-end (time/add-duration current-start current-duration)
                  d-end (time/add-duration d-start d-duration)]
              (if (and (or (time/increasing? d-start current-end)
                           (= current-end d-start))
                       (time/increasing? current-end d-end))
                (let [new-start (minimum current-start d-start)
                      new-end (maximum current-end d-end)
                      new-duration (duration/between new-start new-end)]
                  (recur a-definition b-definition [new-start new-duration] (time/just-after timestamp)))
                current)))))))

                  
        
      

(defn either [a-definition b-definition]
  (fn
    ([timestamp]
     (let [[start duration :as first-value] (find-closest a-definition b-definition timestamp)
           end (time/add-duration start duration) 
           following (time/just-after end)]
       (discover a-definition b-definition first-value following)))
    ([start end] (apply conj (a-definition start end) (b-definition start end)))))

(defn periods-forward [definition timestamp]
  (let [ next-occurance (definition timestamp)]
    (if (= ZERO next-occurance)
      nil
      (let [[start duration] next-occurance
            after-ending (time/just-after (time/add-duration start duration))]
        (cons next-occurance (lazy-seq (periods-forward definition after-ending)))))))

(defn starting
  "Find the sa"
  [definition timestamp previous-start ending]
   (let [next-occurance (definition timestamp)]
     (if (= ZERO next-occurance)
       (throw (ex-info "Should not find ZERO while locating start" {:timestamp timestamp :ending ending}))
       
       (let [[start duration] next-occurance 
             new-ending (time/add-duration start duration)
             stil-not-found (and (= ending new-ending)
                                 (not (= previous-start start)))]
         (if stil-not-found
           (recur definition (time/just-before start) start ending)
           [previous-start (duration/between previous-start ending)])))))

(defn hits-within [definition start end]
  (->> (periods-forward definition start)
       (take-while
        (fn [[start' duration']]
          (let [end' (time/add-duration start' duration')]
            (time/ordered? start' end' end))))
       (filter (fn [[start']]
                 (time/ordered? start start')))))
