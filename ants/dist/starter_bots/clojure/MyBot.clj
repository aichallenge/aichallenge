(ns MyBot
  (:use ants))

(def directions [:north :east :west :south])

(defn simple-bot []
  (doseq [ant (my-ants)]
    (let [dir (first (filter #(valid-move? ant %) directions))]
      (when dir
        (move ant dir)))))

(start-game simple-bot)

