(ns connect4.core
  (:gen-class)
  (:require [irclj.core :as irc]))

(declare privmsgHandler)

(def connection 
  (irc/connect "irc.socs.uoguelph.ca" 6667 "SomeKindaBot" :callbacks {
      :privmsg (fn [irc collection & s] (privmsgHandler collection))
  })
)

(defn privmsgHandler
  "I do a thing!"
  [collection]
  (prn collection)
  (prn (get collection :target))
  (if (= (get collection :text) "HANDSHAKE")
    (irc/message connection (get collection :target) "I shake your hand good sir.")
    (irc/message connection (get collection :target) "I am gonna ignore you."))
)



(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Welcome to Clojure Connect 4!")
  (irc/join connection "#meta")
)
