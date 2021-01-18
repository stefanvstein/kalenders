(ns kalenders.easter-macros)

(defmacro !mod [a b] (list 'unchecked-remainder-int a b))
(defmacro !div [a b] (list 'unchecked-divide-int a b))
(defmacro !+ [a b] (list 'unchecked-add-int a b))
(defmacro !- [a b] (list 'unchecked-subtract-int a b))
(defmacro !inc [a] (list 'unchecked-inc-int a))
(defmacro !* [a b] (list 'unchecked-multiply-int a b))

