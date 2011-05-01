(ns aichallenge-ants-clj.test.ants
  (:use [aichallenge-ants-clj.ants] :reload)
  (:use [clojure.test]))

(def setup-input
"turn 0
loadtime 2000
turntime 2000
rows 200
cols 200
turns 500  
viewradius2 93  
attackradius2 6  
spawnradius2 6  
ready
this-does-not-happen")

(def turn-input
"turn 1
f 6 5
w 7 6
a 7 9 0 
a 10 8 1
a 10 9 1
go
this-does-not-happen")


(print "creating world -> ")
(def bare-world (time (make-world)))

(print "updating it with settings -> ")
(def init-world (time (feed-world bare-world setup-input)))

(print "updating it with turn data -> ")
(def turn-world (time (feed-world init-world turn-input)))

(deftest test-bare
  (is (= "initial" (option bare-world :phase)) "Phase option.")
  (is (= false (option bare-world :volatile)) "We are not volatile.")
  (is (= -1 (option bare-world :turn)) "Turn option.")
  (is (= 2000 (option bare-world :loadtime)) "Default loadtime.")
  (is (= 2000 (option bare-world :turntime)) "Default turntime.")
  (is (= 500 (option bare-world :turns)) "Default turn count.")
  (is (= 93 (option bare-world :viewradius2)) "Default view radius sq.")
  (is (= 6 (option bare-world :attackradius2)) "Default attack radius sq.")
  (is (= 6 (option bare-world :spawnradius2)) "Default spawn radius sq.")
  (is (= 20 (option bare-world :rows)) "Default rows.")
  (is (= 20 (option bare-world :cols)) "Default cols.")
  (is (= (count (coord-space bare-world)) (* (option bare-world :cols)
                                             (option bare-world :rows)))
      "Count of coords is equal to cols x rows.")
  (is (nil? (seq (friendly bare-world))) "No friendlies.")
  (is (nil? (seq (enemy bare-world))) "No enemies.")
  (is (nil? (seq (food bare-world))) "No foods.")
  (is (= 0 (distance bare-world [0 0] [0 0])) "Distance tests, ZERO.")
  (is (= 1 (distance bare-world [0 0] [19 0])) "Distance tests, ONE.")
  (is (= 1 (distance bare-world [0 0] [19 1])) "Distance tests, ONE.")
  (is (= 1 (distance bare-world [0 0] [0 1])) "Distance tests, ONE.")
  (is (= 1 (distance bare-world [0 0] [1 1])) "Distance tests, ONE.")
  (is (= 1 (distance bare-world [0 0] [1 0])) "Distance tests, ONE.")
  (is (= 1 (distance bare-world [0 0] [1 19])) "Distance tests, ONE.")
  (is (= 1 (distance bare-world [0 0] [0 19])) "Distance tests, ONE.")
  (is (= 1 (distance bare-world [0 0] [19 19])) "Distance tests, ONE.")
  (is (= [] (direction bare-world [0 0] [0 0])) "No directions.")
  (is (= [:N] (direction bare-world [0 0] [19 0])) "North.")
  (is (= [:N :W] (direction bare-world [0 0] [19 1])) "North/west.")
  (is (= [:W] (direction bare-world [0 0] [0 1])) "West.")
  (is (= [:S :W] (direction bare-world [0 0] [1 1])) "South/west.")
  (is (= [:S] (direction bare-world [0 0] [1 0])) "South.")
  (is (= [:S :E] (direction bare-world [0 0] [1 19])) "South/east.")
  (is (= [:E] (direction bare-world [0 0] [0 19])) "East.")
  (is (= [:N :E] (direction bare-world [0 0] [19 19])) "North/east.")
  ;(is (= [1 2] (locate bare-world [2 2] :N)) "Going North from [2 2]")
  ;(is (= [2 3] (locate bare-world [2 2] :E)) "Going East from [2 2]")
  ;(is (= [3 2] (locate bare-world [2 2] :S)) "Going South from [2 2]")
  ;(is (= [2 1] (locate bare-world [2 2] :W)) "Going West from [2 2]")
  (is (every? true? (map (fn [c] (= :fog (terrain bare-world c)))
                         (coord-space bare-world)))
      "All tiles are fogged.")
  (is (every? false? (map (fn [c] (seen? bare-world c))
                          (coord-space bare-world)))
      "Have not seen anything yet.")
  (is (every? false? (map (fn [c] (visible? bare-world c))
                          (coord-space bare-world)))
      "Have not seen anything this un-turn."))

(deftest test-init
  (is (= "ready" (option init-world :phase)) "Phase option.")
  (is (= false (option init-world :volatile)) "We are not volatile.")
  (is (= 0 (option init-world :turn)) "Turn option.")
  (is (= 2000 (option init-world :loadtime)) "Set loadtime.")
  (is (= 2000 (option init-world :turntime)) "Set turntime.")
  (is (= 500 (option init-world :turns)) "Set turn count.")
  (is (= 93 (option init-world :viewradius2)) "Set view radius sq.")
  (is (= 6 (option init-world :attackradius2)) "Set attack radius sq.")
  (is (= 6 (option init-world :spawnradius2)) "Set spawn radius sq.")
  (is (= 200 (option init-world :rows)) "Set rows.")
  (is (= 200 (option init-world :cols)) "Set cols.")
  (is (= (count (coord-space init-world)) (* (option init-world :cols)
                                             (option init-world :rows)))
      "Count of coords is equal to cols x rows.")
  (is (nil? (seq (friendly init-world))) "No friendlies.")
  (is (nil? (seq (enemy init-world))) "No enemies.")
  (is (nil? (seq (food init-world))) "No foods."))

(deftest test-turn
  (is (= "go" (option turn-world :phase)) "Phase option.")
  (is (= false (option turn-world :volatile)) "We are not volatile.")
  (is (= 1 (option turn-world :turn)) "Turn option.")
  (is (= 2000 (option turn-world :loadtime)) "Set loadtime.")
  (is (= 2000 (option turn-world :turntime)) "Set turntime.")
  (is (= 500 (option turn-world :turns)) "Set turn count.")
  (is (= 93 (option turn-world :viewradius2)) "Set view radius sq.")
  (is (= 6 (option turn-world :attackradius2)) "Set attack radius sq.")
  (is (= 6 (option turn-world :spawnradius2)) "Set spawn radius sq.")
  (is (= 200 (option turn-world :rows)) "Set rows.")
  (is (= 200 (option turn-world :cols)) "Set cols.")
  (is (= (count (coord-space turn-world)) (* (option turn-world :cols)
                                             (option turn-world :rows)))
      "Count of coords is equal to cols x rows.")
  (is (= [[7 9]] (vec (friendly turn-world))) "Friendlies.")
  (is (= [[10 8] [10 9]] (vec (enemy turn-world))) "Enemies.")
  (is (= #{[6 5]} (food turn-world)) "Foods.")
  (is (= :water (terrain turn-world [7 6])) "That field is a water.")
  (is (every? true? (map (fn [c] (= :land (terrain turn-world c)))
                         [[7 9] [10 8] [10 9] [6 5]]))
      "Implied land tiles are land tiles."))

(deftest issue-test
  (is (= "o 0 0 N\n" (with-out-str (issue turn-world [0 0] :N)))
      "Issuing order.")
  (is (= "go\n" (with-out-str (end-turn turn-world)))
      "Ending turn."))

