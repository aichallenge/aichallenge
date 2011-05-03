(ns aichallenge-ants-clj.core
  [:gen-class]
  [:use aichallenge-ants-clj.ants aichallenge-ants-clj.my-bot])

;; ## Main loop.
;;
;; Entry point of the program. If you keep to the template this does
;; not need to be changed.
;;
;; -main :: [[String]] -> nil
(defn -main [& args]
  (let [[world bot] (run (make-world) (make-bot))
        last-world (update world)]
    (do
      (println world)
      (println bot)
      (println last-world))))

