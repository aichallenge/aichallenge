(ns aichallenge-ants-clj.my-bot
  [:gen-class]
  (:use aichallenge-ants-clj.ants))

;; ## MyBot sample.
;;
;; Single method defrecord for now.
(defrecord MyBot []
  AntsBot
  ;; do-turn :: MyBot -> AntsWorld -> MyBot
  (do-turn [this world]
    (let [dirs [:N :E :S :W]]
      (doseq [f (friendly world) ; enumerate friendly coords
              :while (> (remaining world) 10)] ; if enough time
        (if-let [dir (first (filter (comp (partial passable? world)
                                          (partial locate world f)) dirs))]
          (issue world f dir)))
      this)))

;; make-bot :: MyBot
(defn make-bot [] (MyBot.))

