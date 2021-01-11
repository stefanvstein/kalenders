(ns kalenders.easter
  (:require [kalenders.time :as time]
            [kalenders.easter-macros :refer :all]))

#_
"let internal easter year =
  let a     = year%19
  let (b,c) = year/%100
  let (d,e) = b/%4
  let f     = (b+8)/25
  let g     = (b-f+1)/3
  let h     = (19*a+b-d-g+15)%30
  let (i,k) = c/%4
  let l     = (32+2*e+2*i-h-k)%7
  let m     = (a+11*h+22*l)/451
  let (n,p) = (h+l+7*m+114)/%31
  new System.DateTime(year,n,p+1)"

(defn easterfn
  
  "Computes the date of Easter for a given year beginning from 1583.  
  Method is that described in 'Butcher's Ecclesiastical Calendar' (1876),
  and as given in: 'Practical Astronomy With Your Calculator'
  by Peter Duffett-Smith (Cambridge, 1981)."

  [^long year]
  (let [a (!mod year 19)
        b (!div year 100)
        c (!mod year 100)
        d (!div b 4)
        e (!mod b 4)
        f (!div (!+ 8 b) 25)
        g (!div (!- (!inc b) f) 3)
        
        h (!mod (!- (!+ (!* 19 a) (!+ b 15) )
                    (!+ d g))
                30)
        i (!div c 4)
        k (!mod c 4)
        l (!mod (!- (!+ 32 (!+ (!* 2 e)
                               (!* 2 i)))
                    (!+ h k))
                7)
        m (!div (!+ a
                   (!+ (!* 11 h)
                       (!* 22 l)))
               451)
        n  (!div (!+ h
                    (!+ l
                        (!+ (!* 7 m)
                            114)))
                31)
        p (!mod (!+ h
                    (!+ l
                        (!+ (!* 7 m)
                            114)))
                31)]
    (time/of year n (!inc p))))

(def easter (memoize easterfn))


