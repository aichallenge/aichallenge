(ns aichallenge-ants-clj.test.my-bot
  (:use [aichallenge-ants-clj.ants] :reload)
  (:use [aichallenge-ants-clj.my-bot] :reload)
  (:use [clojure.test]))

(def manual-world
  (-> (make-world)
    (volatile)
    (prop! "turntime" 2000000000)
    ; no trap, go north
    (unit! [1 1] 0) ; -> :N -> [0 1]
    ; trap 1 -> go east
    (water! [2 2])
    (unit! [3 2] 0) ; -> :E [3 3]
    ; trap 2 -> go south
    (water! [2 5])
    (water! [3 6])
    (unit! [3 5] 0) ; -> :S -> [4 5]
    ; trap 3 -> go west
    (water! [4 2])
    (water! [5 3])
    (water! [6 2])
    (unit! [5 2] 0) ; -> :W -> [5 1]
    ; trap 4 -> stay put
    (water! [7 1])
    (water! [8 2])
    (water! [9 1])
    (water! [8 0])
    (unit! [8 1] 0)
    (freeze! :handy)))

(def expected-outs (hash-set "o 1 1 N" "o 3 2 E" "o 3 5 S" "o 5 2 W"))

(def t-bone (make-bot))

(def actual-output (into (hash-set)
                         (seq (.split #"\r?\n"
                                      (with-out-str (do-turn t-bone
                                                              manual-world))))))

(deftest test-do-turn
  (is (= expected-outs actual-output) "Actions expected of sample bot."))

  
