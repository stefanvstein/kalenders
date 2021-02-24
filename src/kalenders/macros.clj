(ns kalenders.macros)

(defmacro | [a x & b]
  (apply list x a b))

(defmacro apply-first-value
  "Takes a value v and a set of function pairs t f & tfs. For each pair
  the second function f is applied by the value of the function t of v.
  Only true values will be applied to f, and the next pair will be used
  instead if t happens to return nil or false. The first match will be
  returned or nil.  

  Typically used to treat a map as tagged union data type. Usfull to
  represent error in computation without throwing exceptions. Something
  that is conceptually smaller than a result monad.

  (defn compute [v] 
    (if v
      {:ok (inc v)}
      {:err \"there is no value\"}))
  
  (apply-first-value
    (compute nil)
    :ok (fn [v] (str \"Got a \" v))
    :err (fn [e] (str \"Weird, \" e)))" 
  [v t f & tfs]
  `(let [v# ~v]
     ((fn res# [t# f# tfs#]
        (if-let [r# (t# v#)]
            (f# r#)
            (let [[t'# f'# & tfs'#] tfs#]
              (if (and t'# f'#)
                (recur t'# f'# tfs'#)))))
      ~t ~f (vector ~@tfs))))
