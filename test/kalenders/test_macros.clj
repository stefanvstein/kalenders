(ns kalenders.test-macros
  (:require  [clojure.test :as t]))

(defmacro is=
  ([a b]
   (list 'clojure.test/is (list '= a b)))
  ([a b msg]
   (list 'clojure.test/is (list '= a b) msg)))


