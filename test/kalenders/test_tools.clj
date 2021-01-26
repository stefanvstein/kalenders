(ns kalenders.test-tools
  (:require  [clojure.test :as t]
              [clojure.string :as str]))



(defmethod t/assert-expr 'ex-info-msg-data? [msg form]
  (let [re (nth form 1)
        fun (nth form 2)
        body (nthnext form 3)]
    `(try ~@body
          (t/do-report {:type :fail, :message ~msg, :expected '~form, :actual nil})
          (catch clojure.lang.ExceptionInfo e#
            (let [m# (ex-message e#)
                  d# (ex-data e#)
                  message-fail# (when-not (re-find ~re m#)
                                  (with-out-str
                                    (print " Message: ")
                                    (prn m#)
                                    (print "  did not match: ")
                                    (pr '~re)))
                  pred-fail# (when-not (~fun d#)
                               (with-out-str
                                 (print " Predicate: ") (prn '~fun)
                                 (print "  failed on: ") (pr d#)))]
              (if (or message-fail# pred-fail#)
                (t/do-report {:type :fail,
                              :message (->> [~msg message-fail# pred-fail#]
                                            (filter (complement str/blank?))
                                            (interpose (with-out-str (println)))
                                            (apply str)),
                              :expected '~form,
                              :actual e#})
                (t/do-report {:type :pass, :message ~msg,
                              :expected '~form, :actual e#})))

            e#))))

(defmethod t/assert-expr 'ex-info-msg? [msg form]
    (let [re (nth form 1)
          body (nthnext form 2)]
    `(try ~@body
          (t/do-report {:type :fail, :message ~msg, :expected '~form, :actual nil})
          (catch clojure.lang.ExceptionInfo e#
            (let [m# (ex-message e#)
                  
                  message-fail# (when-not (re-find ~re m#)
                                  (with-out-str
                                    (print " Message: ")
                                    (prn m#)
                                    (print "  did not match: ")
                                    (pr '~re)))
                  ]
              (if message-fail#
                (t/do-report {:type :fail,
                              :message (->> [~msg message-fail#]
                                              (filter (complement str/blank?))
                                              (interpose (with-out-str (println)))
                                              (apply str)),
                              :expected '~form,
                              :actual e#})
                (t/do-report {:type :pass, :message ~msg,
                              :expected '~form, :actual e#})))

            e#))))

(defmethod t/assert-expr 'ex-info-data? [msg form]
    (let [fun (nth form 1)
          body (nthnext form 2)]
    `(try ~@body
          (t/do-report {:type :fail, :message ~msg, :expected '~form, :actual nil})
          (catch clojure.lang.ExceptionInfo e#
            (let [d# (ex-data e#)
                  pred-fail# (when-not (~fun d#)
                               (with-out-str
                                 (print " Predicate: ") (prn '~fun)
                                 (print "  failed on: ") (pr d#)))
                  
                  ]
              (if pred-fail#
                (t/do-report {:type :fail,
                              :message (->> [~msg pred-fail#]
                                              (filter (complement str/blank?))
                                              (interpose (with-out-str (println)))
                                              (apply str)),
                              :expected '~form,
                              :actual e#})
                (t/do-report {:type :pass, :message ~msg,
                              :expected '~form, :actual e#})))

            e#))))


