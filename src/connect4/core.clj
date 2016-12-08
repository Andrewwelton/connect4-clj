(ns connect4.core
  (:gen-class)
  (:require [irclj.core :as irc])
  (:require [clojure.string :as str]))

(declare privmsgHandler)
(declare printControls)
(declare receiveAcceptChallenge)

(def validCharacters
  (map char (concat 
    (range 48 58) ; 0-9
    (range 65 91) ; A-Z
    (range 97 123) ; a-z
  ))
)

(defn genUniqueId
  "Generate a unique id for messages"
  [length]
  (apply str (repeatedly length #(char (rand-nth validCharacters))))
)

; player 0 is empty, player 1 is this bot, player 2 is opponent.
; ALWAYS
(def player [" ", "X", "O"])

(def empty-board
  "Empty board with 6 rows and 7 columns."
  (vec (repeat 6 (vec (repeat 7 (player 0)))))
)

(def game (atom {
  :my-challenge-id (genUniqueId 4)
  :open-challenge 0
  :board empty-board
  :my-turn 0
  :game-in-progress 0
}))

(defn get-y
  "Determines y-coordinate for given x-coordinate."
  [board x]
  (first (filter #(= (get-in board [% x]) (player 0))
                 (range 5 -1 -1))
  )
)

(defn print-board
  [board]
  (println [1 2 3 4 5 6 7])
  (doseq [row board] (println row))
)

(defn insert
  "Inserts symbol for given player (either 1 or 2) at specified x."
  [board x player-num]
  (let [y (get-y board x)]
    (assoc-in board [y x] (player player-num))
  )
)

;(def connection 
;  (irc/connect "irc.socs.uoguelph.ca" 6667 "SomeKindaBot" :callbacks {
;      :privmsg (fn [irc collection & s] (privmsgHandler collection))
;  })
;)

(def connection
  (irc/connect "chat.freenode.net" 6667 "SomeKindaBot" :callbacks {
    :privmsg (fn [irc collection & s] (privmsgHandler collection))
  })
)

(defn privmsgHandler
  "I do a thing!"
  [collection]
  (prn collection)
  (prn (get collection :target))
  (if (= (get collection :text) (str "ACCEPT_CHALLENGE:" (get @game :my-challenge-id)))
    (receiveAcceptChallenge)
  )
)

(defn receiveAcceptChallenge
  "Handles receiving a challenge"
  []
  (do 
    (swap! game
      (fn [current-state]
        (merge-with + current-state {:game-in-progress 1 :my-turn 1})
      )
    )
    (println "Challenge Accepted!")
    (println "Starting game, you go first!")
    (print-board (get @game :board))
    (printControls)
  )
)

(defn playPiece
  "Plays a piece on the board if it is your turn"
  []
  (if (= (get @game :my-turn) 1)
    (do
      (println "Enter your move:")
      (let [rawCommand (Integer. (read-line))]
        (do
          (swap! game
            (fn [current-state]
              (let [board (get @game :board)]
                (merge-with + current-state {:my-turn 0})
              )
            )
          )
          (let [board (get @game :board)]
            (swap! game assoc-in [:board] (insert board (dec rawCommand) 1))
          )
        )
        (println (get @game :board))
      )
    )
    (println "It is not your turn!")
  )
)

(defn myTurn
  "Tells you if it is your turn or not"
  []
  (if (= (get @game :my-turn) 1)
    (println "It is your turn!")
    (println "Not your turn!")
  )
)

(defn openChallenge
  "Opens a challenge via IRC"
  []
  (if (= (get @game :open-challenge) 0)
    (do
      (irc/message connection "#andrewircschooltest" (str "OPEN_CHALLENGE:V1:" (get @game :my-challenge-id)))
      (swap! game
        (fn [current-state]
          (merge-with + current-state {:open-challenge 1})
        )
      )
    )
    (println "You already have an open challenge. Cancel it to open another.")
  )
)

(defn cancelChallenge
  "Cancels an open challenge via IRC"
  []
  (if (= (get @game :open-challenge) 1)
    (do
      (irc/message connection "#andrewircschooltest" (str "CANCEL_CHALLENGE:" (get @game :my-challenge-id)))
      (swap! game
        (fn [current-state]
          (merge-with + current-state {:open-challenge 0 :my-challenge-id (genUniqueId 4)})
        )
      )
    )
    (println "You have no open challenges to cancel.")
  )
)

(defn acceptChallenge
  "Accepts a challenge"
  []
  ;; TODO:: Accept a challenge
)

(defn handleIt
  "Handles it"
  [command]
  (if (= (get @game :game-in-progress) 0)
    (do
      (case command
        1 (openChallenge)
        2 (cancelChallenge)
        3 (acceptChallenge)
      )
    )
    (do
      (case command
        1 (playPiece)
        2 (print-board (get @game :board))
        3 (myTurn)
      )
    )
  )
)

(defn printControls
  "Prints out controls for Connect 4"
  []
  (println "Your options: ")
  (if (= (get @game :game-in-progress) 0)
    (do
      (println "1. Open Challenge")
      (println "2. Cancel Challenge")
      (println "3. Accept Challenge")
    )
    (do
      (println "1. Play Piece")
      (println "2. Print Board")
      (println "3. My turn?")
    )
  )
)

(defn -main
  "Main control loop"
  [& args]
  (println "Welcome to Clojure Connect 4!")
  (println "Joining IRC server...")
  (irc/join connection "#andrewircschooltest")
  (printControls)
  (loop [rawCommand (read-line)]
    (handleIt (Integer. rawCommand))  
    (recur (read-line))
  )
)
