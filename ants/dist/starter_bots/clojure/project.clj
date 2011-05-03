;; # Project file.
;;
;; Please feel free to adjust settings below to match your project,
;; but it's important you:
;; 1. Don't change `:disable-implicit-clean`;
;; 2. Run `lein deps` before you zip the bot for contest.
;;
;; Thanks to this the ZIP package will contain all deps, so they won't
;; be fetched during build (which may not be possible in the sandbox).
(defproject aichallenge-ants-clj "0.0.2"
  :description "AI Challenge 'Ants' -- Winter/Spring 2011"
  :dependencies [[org.clojure/clojure "1.2.1"]
                 [org.clojure/clojure-contrib "1.2.0"]]
  :dev-dependencies [[marginalia "0.5.0"]]
  :disable-implicit-clean true
  :main aichallenge-ants-clj.core
  :aot [aichallenge-ants-clj.ants
        aichallenge-ants-clj.my-bot
        aichallenge-ants-clj.core]
  :jar-name "MyBot-light.jar"
  :uberjar-name "MyBot.jar"
  ; feel free to change this for deployment
  :warn-on-reflection true)

