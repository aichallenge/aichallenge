(ns aichallenge-ants-clj.ants
  [:gen-class])


;; # AI Google Challenge -- Ants!

;; ## Ants constants and contracts.
(def contracts {:landscape (hash-set :land :water :fog)
                :direction (hash-map :N [-1 0], :E [0  1],
                                     :S [ 1 0], :W [0 -1])
                :unit      (hash-set :friend :enemy)
                :event     (hash-set :food)
                :coords    [:row :col]})

;; ## Parsing helpers.
(def tokens {:splitter   (partial re-seq #"\S+")
             :terminate? (hash-set "ready" "go" "end")
             :water?     (hash-set "w" "W")
             :unit?      (hash-set "a" "A")
             :dead?      (hash-set "d" "D")
             :food?      (hash-set "f" "F")})

;; ## AntsBot Protocol
;;
;; What we expect from AntsBot implementation.
(defprotocol AntsBot
  ;; do-turn :: AntsBot -> AntsWorld -> AntsBot
  (do-turn [this ants] "Run one turn against world, return updated bot."))

;; ## VolatileAntsWorld Protocol
;;
;; World with transient fields -- for update only!
(defprotocol VolatileAntsWorld
  ;; freeze! :: VolatileAntsWorld -> String -> AntsWorld
  (freeze! [this token] "Return persisntent World.")
  ;; land! :: VolatileAntsWorld -> [int, int] -> VolatileAntsWorld
  (land! [this coords] "Mark land.")
  ;; water! :: VolatileAntsWorld -> [int, int] -> VolatileAntsWorld
  (water! [this coords] "Mark water.")
  ;; unit! :: VolatileAntsWorld -> [int, int] -> int -> VolatileAntsWorld
  (unit! [this coords owner] "Mark unit.")
  ;; dead! :: VolatileAntsWorld -> [int, int] -> int -> VolatileAntsWorld
  (dead! [this coords owner] "Mark corpse.")
  ;; dead! :: VolatileAntsWorld -> [int, int] -> VolatileAntsWorld
  (food! [this coords] "Mark food.")
  ;; prop! :: VolatileAntsWorld -> sym -> a -> VolatileAntsWorld 
  (prop! [this a-key a-val] "Mark property."))

;; ## AntsWorld Protocol
;;
;; What we expect from AntsWorld implementation.
(defprotocol AntsWorld
  ;;
  ;; ### Core functionality.
  ;;
  ;; volatile :: AntsWorld -> AntsWorld
  (volatile [this] "Return transient-ified version for refreshing.")
  ;; refresh :: AntsWorld -> AntsWorld
  (refresh [this] "Do IO, register changes.")
  ;; update :: AntsWorld -> AntsWorld
  (update [this] "Update internal AntsWorld state if needed.")
  ;; end-turn :: AntsWorld -> AntsWorld
  (end-turn [this] "Send end-turn to the game.")
  ;; run :: AntsWorld -> AntsBot -> [AntsWorld, AntsBot]
  (run [this bot] "Run the bot against reality.")
  ;;
  ;; ### Helper functions.
  ;;
  ;; issue :: AntsWorld -> [int, int] -> String -> AntsWorld
  (issue [this coords cmd] "Issue order to the game.")
  ;; option :: AntsWorld -> sym -> val
  (option [this sym] "Get setting under symbol.")
  ;; friendly :: AntsWorld -> [[int, int]]
  (friendly [this] "Return list of friendly ants coords.")
  ;; enemy :: AntsWorld -> [[int, int]]
  (enemy [this] "Return list of enemy ants coords.")
  ;; food :: AntsWorld -> [[int, int]]
  (food [this] "Return list of food containing coords.")
  ;; terrain :: AntsWorld -> [int, int] -> land 
  (terrain [this coords] "Get landscape information.")
  ;; info :: AntsWorld -> [int, int] -> hash-set
  (info [this coords] "Get complete landscape information.")
  ;; distance2 :: AntsWorld -> [int, int] -> [int, int] -> int
  (distance2 [this orig dest] "Shortest squared distance on the torus.")
  ;; distance :: AntsWorld -> [int, int] -> [int, int] -> int
  (distance [this orig dest] "Shortest distance on the torus.")
  ;; direction :: AntsWorld -> [int, int] -> [int, int] -> [direction]
  (direction [this orig dest] "Direction on the torus.")
  ;; locate :: AntsWorld -> [int, int] -> Direction -> [int, int]
  (locate [this coords dir] "Return coords of destination.")
  ;; visible? :: AntsWorld -> [int, int] -> Bool
  (visible? [this coords] "Coords seen in this turn?")
  ;; seen? :: AntsWorld -> [int, int] -> Bool
  (seen? [this coords] "Coords seen since beginning?")
  ;; passable? :: AntsWorld -> [int, int] -> Bool
  (passable? [this coords] "Can coords be entered?")
  ;; remaining :: AntsWorld -> long
  (remaining [this] "Time remaining since last 'go'.")
  ;; coord-space :: AntsWorld -> [[int, int]]
  (coord-space [this] "Return a sequence of valid coords."))

;; ## World implementation.
;;
;; `board` -- landscape type sparse matrix (hash), [row col] => land.
;;
;; `units` -- current round units update, [row col] => owner:
;;
;; `frags` -- hits registry, #{[row col owner]};
;;
;; `foods` -- food #{[row col]};
;; 
;; `intel` -- coordinates updated in this round;
;;
;; `props` -- turn, phase, timestamp, etc.
(defrecord World [board     ;;  {[row col] land}
                  units     ;;  {[row col] owner}
                  frags     ;; #{[row col owner]}
                  foods     ;; #{[row col]}
                  intel     ;; #{[row col]}
                  props]    ;;  {:sym value}

  VolatileAntsWorld
  
  ;; freeze! :: World -> String -> World
  (freeze! [this token]
    (let [ts (System/currentTimeMillis)
          props (assoc! props :phase token :ts ts :volatile false)]
      (into this {:board (persistent! board)
                  :units (persistent! units)
                  :frags (persistent! frags)
                  :foods (persistent! foods)
                  :intel (persistent! intel)
                  :props (persistent! props)})))

  ;; land! :: World -> [int, int] -> World
  (land! [this coords]
    (into this {:board (assoc! board coords :land)
                :intel (conj! intel coords)}))

  ;; water! :: World -> [int, int] -> World
  (water! [this coords]
    (into this {:board (assoc! board coords :water)
                :intel (conj! intel coords)}))

  ;; unit! :: World -> [int, int] -> int -> World
  (unit! [this coords owner]
    (into this {:board (assoc! board coords :land)
                :units (assoc! units coords owner)
                :intel (conj! intel coords)}))

  ;; dead! :: World -> [int, int] -> int -> World
  (dead! [this coords owner]
    (into this {:board (assoc! board coords :land)
                :frags (conj! frags (conj coords owner))
                :intel (conj! intel coords)}))

  ;; food! :: World -> [int, int] -> World
  (food! [this coords]
    (into this {:board (assoc! board coords :land)
                :foods (conj! foods coords)
                :intel (conj! intel coords)}))

  ;; prop! :: World -> sym -> a -> World
  (prop! [this a-key a-val]
    (let [kw (keyword a-key)]
      (into this {:props (assoc! props kw a-val)
                  :intel (conj! intel kw)})))

  AntsWorld

  ;; volatile :: World -> World
  (volatile [this]
    (into this {:board (transient board)
                :units (transient (empty units))
                :frags (transient (empty frags))
                :foods (transient (empty foods))
                :intel (transient (empty intel))
                :props (transient (assoc props :volatile true))}))

  ;; "MONSTROSITY! How do you even test that?!" -- you ask.
  ;; "Switch and bait, my good sir/madam!" -- I respond swiftly.
  ;;
  ;; (binding [*in* (java.io.BufferedReader. (java.io.StringReader test-inp))]
  ;;   (assert-state (refresh world)))
  ;;
  ;; "Where assert-state is left as an excercise to the reader." -- I add.
  ;; 
  ;; refresh :: World -> World
  (refresh [this]
    (let [this! (volatile this)
          {:keys [splitter terminate? water? unit? dead? food?]} tokens]
      (loop [[X r c o & junk] (splitter (.toLowerCase ^String (read-line)))]
        (if (terminate? X) (freeze! this! X) ;; assess view, freeze.
          (let [row       (when-not (nil? r) (Integer/parseInt r))
                col       (when-not (nil? c) (Integer/parseInt c))
                owner     (when-not (nil? o) (Integer/parseInt o))
                coords    [row col]]
            (cond
              (water? X) (water! this! coords)
              (unit? X)  (unit! this! coords owner)
              (dead? X)  (dead! this! coords owner)
              (food? X)  (food! this! coords)
              (nil? X) nil
              :else (prop! this! X row))
            (recur (splitter (.toLowerCase ^String (read-line)))))))))
  
  ;; update :: World -> World
  (update [this]
    (let [this-n (refresh this)]
      (do ;; something here perhaps?
        this-n)))
  
  ;; issue :: World -> [int, int] -> String -> World
  (issue [this coords cmd]
    (let [[row col] coords]
      (do (println (format "o %s %s %s" row col (name cmd)))
          this)))

  ;; end-turn :: World -> World
  (end-turn [this]
    (do (println "go") (.flush System/out) this))

  ;; run :: World -> AntsBot -> [World, AntsBot]
  (run [this bot]
    (let [this-n (end-turn (update this))]
      (loop [bot-n bot this-n this-n]
        (if (not= "end" (option this-n :phase))
          (recur (do-turn bot-n this-n) (end-turn this-n))
          [this-n bot-n]))))
 
  ;; option :: World -> sym -> a
  (option [this sym]
    (get props sym))

  ;; friendly :: World -> [[int, int]]
  (friendly [this]
    (map key (filter (comp zero? val) units)))

  ;; enemy :: World -> [[int, int]]
  (enemy [this]
    (map key (filter (comp not zero? val) units)))

  ;; food :: World -> [[int, int]]
  (food [this]
    foods)
  
  ;; terrain :: World -> [int, int] -> land 
  (terrain [this coords]
    (get board coords :fog))
  
  ;; info :: World -> [int, int] -> hash-set
  (info [this coords]
    (let [[row col] coords
          ef (fn ef [[r c e]] (and (== row r) (== col c)))
          kf (fn kf [[r c]] (and (== row r) (== col c)))
          te (terrain this coords)]
      (if (#{:fog :water} te) (hash-set te)
        (disj (into (hash-set) [te
                                (when (first (filter kf (friendly this)))
                                  :friend)
                                (when (first (filter kf (enemy this)))
                                  :enemy)
                                (when (first (filter kf (food this)))
                                  :food)])
              nil))))
  
  ;; distance2 :: World -> [int, int] -> [int, int] -> int
  (distance2 [this orig dest]
    (let [[sr sc] orig
          [tr tc] dest
          rows (option this :rows)
          cols (option this :cols)
          dr (min (Math/abs ^Double (- sr tr))
                  (- rows (Math/abs ^Double (- sr tr))))
          dc (min (Math/abs ^Double (- sc tc))
                  (- cols (Math/abs ^Double (- sc tc))))]
      (int (+ (Math/pow dr 2) (Math/pow dc 2)))))
  
  ;; distance :: World -> [int, int] -> [int, int] -> int
  (distance [this orig dest]
    (let [[sr sc] orig
          [tr tc] dest
          rows (option this :rows)
          cols (option this :cols)
          dr (min (Math/abs ^Double (- sr tr))
                  (- rows (Math/abs ^Double (- sr tr))))
          dc (min (Math/abs ^Double (- sc tc))
                  (- cols (Math/abs ^Double (- sc tc))))]
      (int (Math/sqrt (+ (Math/pow dr 2) (Math/pow dc 2))))))
  
  ;; direction :: World -> [int, int] -> [int, int] -> sym
  (direction [this orig dest]
    (let [[sr sc] orig
          [tr tc] dest
          crow (int (/ (option this :rows) 2))
          ccol (int (/ (option this :cols) 2))
          tr-sr (- tr sr)
          tc-sc (- tc sc)
          sr-tr (- sr tr)
          sc-tc (- sc tc)]
    (vec (concat (cond (zero? tr-sr) []
                       (== crow tr-sr) [:N :S]
                       (pos? sr-tr) (if (> tr-sr crow) [:S] [:N])
                       :else (if (> tr-sr crow) [:N] [:S]))
                 (cond (zero? tc-sc) []
                       (== ccol tc-sc) [:E :W]
                       (pos? sc-tc) (if (> tc-sc ccol) [:W] [:E])
                       :else (if (> tc-sc ccol) [:E] [:W]))))))
 
  ;; locate :: World -> [int, int] -> Direction -> [int, int]
  (locate [this coords dir]
    (let [{:keys [direction]} contracts]
      (if (not (contains? direction dir))
        (throw (Exception. (format "Unknown direction: %s."
                                   dir)))
        (let [max-r (option this :rows)
              max-c (option this :cols)
              [sr sc] coords
              [dr dc] (get direction dir)
              tr (+ sr dr)
              tr (if (< tr 0) (+ max-r tr) (mod tr max-r))
              tc (+ sc dc)
              tc (if (< tc 0) (+ max-c tc) (mod tc max-c))]
          [tr tc]))))

  ;; visible? :: World -> [int, int] -> Bool
  (visible? [this coords]
    (contains? update coords))
  
  ;; seen? :: World -> [int, int] -> Bool
  (seen? [this coords]
    (or (and (find board coords) true) false))
  
  ;; passable? :: World -> [int, int] -> Bool
  (passable? [this coords]
    (not= (get board coords) :water))
  
  ;; remaining :: World -> long
  (remaining [this]
    (if-let [tt (option this :turntime)]
      (if-let [ts (option this :ts)]
        (- (+ ts tt) (System/currentTimeMillis))
        tt) ;; hmm
      (long 0)))

  ;; coord-space :: World -> [[int, int]]
  (coord-space [this]
    (let [rows (option this :rows)
          cols (option this :cols)]
      (for [idx (range (* rows cols))] [(quot idx rows) (mod idx cols)]))))

;; make-world :: World
(defn make-world []
  (let [defaults {:ts (System/currentTimeMillis)
                  :phase "initial"
                  :volatile false
                  :loadtime 2000
                  :turntime 2000
                  :turns 500
                  :viewradius2 93
                  :attackradius2 6
                  :spawnradius2 6
                  :turn -1
                  :rows 20
                  :cols 20}]
    (World. {} {} #{} #{} #{} defaults)))

;; feed-world :: World -> String -> World
(defn feed-world [world input]
  (binding [*in* (java.io.BufferedReader. (java.io.StringReader. input))]
    (update world)))

