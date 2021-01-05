(ns kalenders.macros)

(defmacro | [a x & b]
  (apply list x a b))
