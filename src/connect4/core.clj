; Andrew Welton
; CIS 4900/4910 - Functional Programming
; F16 Semester
; Game logic courtesy of: http://programmablelife.blogspot.ca/2012/09/clojure-connect-four-1-checking-winner.html
; Link above used to help me "think" functionally and focus on the IRC communication aspect rather than invent my own way to play connect 4
; Prompted a rewrite of most logic I had here originally
; This is not properly protected so it assumes the user of the bot isn't going to intentionally try to break it.
(ns connect4.core
  (:gen-class)
  (:require [irclj.core :as irc])
  (:require [clojure.string :as str]))

(declare priv-msg-handler)
(declare print-controls)
(declare receive-accept-challenge)
(declare receive-play-command)
(declare receive-win-agree)
(declare receive-win-command)
(declare reset-game)
(declare resign-game)

(def valid-characters
  (map char (concat 
    (range 48 58) ; 0-9
    (range 65 91) ; A-Z
    (range 97 123) ; a-z
  ))
)

(defn gen-unique-id
  "Generate a unique id for messages"
  [length]
  (apply str (repeatedly length #(char (rand-nth valid-characters))))
)

; player 0 is empty, player 1 is this bot, player 2 is opponent.
; ALWAYS
(def player [" ", "X", "O"])

(def empty-board
  "Empty board with 6 rows and 7 columns."
  (vec (repeat 6 (vec (repeat 7 (player 0)))))
)

(def game (atom {
  :my-challenge-id (gen-unique-id 4)
  :open-challenge 0
  :board empty-board
  :my-turn 0
  :game-in-progress 0
  :current-game-id ""
  :waiting-for-win-agree 0
  :win-dispute 0
  :current-irc-channel "#andrewircschooltest"
}))

(defn get-y
  "Determines y-coordinate for given x-coordinate."
  [board x]
  (first 
    (filter 
      #(= (get-in (get @game :board) [% x]) (player 0))
      (range 5 -1 -1)
    )
  )
)

(defn print-board
  [board]
  (println [0 1 2 3 4 5 6])
  (doseq [row board] (println row))
)

(defn insert
  "Inserts symbol for given player (either 1 or 2) at specified x."
  [board x player-num]
  (let [y (get-y board x)]
    (assoc-in board [y x] (player player-num))
  )
)

(defn get-diagonals
  "Gets diagonals of a start position using the given function"
  [startPosition diagonal-function]
  (for [position startPosition]
    (take 4 (iterate diagonal-function position))
  )
)

(def win-conditions
  "Contains all the possible win conditions for connect 4"
  (let 
    [
      rows
        (for [y (range 6), x (range 4)]
          (for [z (range 4)]
            [y (+ x z)]
          )
        )
      columns
        (for [x (range 7), y (range 3)]
          (for [z (range 4)]
            [(+ z y) x]
          )
        )
      diagonals
        (concat
          (get-diagonals 
            (for [y (range 3), x (range 4)]
              [y x]
            )
            (partial mapv inc)
          )
          (get-diagonals
            (for [y (range 3), x (range 3 7)]
              [y x]
            )
            (fn [[y x]] [(inc y) (dec x)])
          )
        )
    ]
    (concat rows columns diagonals)
  )
)

(defn check-win
  "Check whether the move results in a win"
  [board x]
  ; inc to get the ACTUAL value of Y for the piece that was inserted
  (let [y (inc (get-y board x))]
    (let 
      [
        win
          (first 
            (drop-while 
              (fn [z]
                (apply not= (map #(get-in board %) z))
              )
              (filter #(some #{[y x]} %) win-conditions)
            )
          )
      ]
      (if (empty? win) 
        false
        true
      )
    )
  )
)

;(def connection 
;  (irc/connect "irc.socs.uoguelph.ca" 6667 "SomeKindaBot" :callbacks {
;      :privmsg (fn [irc collection & s] (privmsgHandler collection))
;  })
;)

(def connection
  (irc/connect "chat.freenode.net" 6667 "SomeKindaBot" :callbacks {
    :privmsg (fn [irc collection & s] (priv-msg-handler collection))
  })
)

(defn priv-msg-handler
  "I do a thing!"
  [collection]
  (prn collection)
  (prn (get collection :target))
  (if (= (get collection :text) (str "ACCEPT_CHALLENGE:" (get @game :my-challenge-id)))
    (receive-accept-challenge (get @game :my-challenge-id))
  )
  (if (boolean (re-find #"PLAY:" (get collection :text)))
    (receive-play-command (str/split (get collection :text) #":"))
  )
  (if (= (get collection :text) (str "WIN_AGREE"))
    (receive-win-agree)
  )
  (if (boolean (re-find #"WIN:" (get collection :text)))
    (do
      (receive-play-command (str/split (get collection :text) #":"))
      (swap! game assoc :win-dispute 1)
      (println "Your opponent has claimed they won the game with that move.")
      (print-board (get @game :board))
      (println "Did your opponent win?")
      (print-controls)
    )
  )
  (if (boolean (re-find #"OPEN_CHALLENGE:V1" (get collection :text)))
    (do
      (let [command (str/split (get collection :text) #":")]
        (println (str "A challenge has been opened with the id: " (get command 2)))
      )
    )
  )
  (if (boolean (re-find #"CANCEL_CHALLENGE:" (get collection :text)))
    (do
      (let [command (str/split (get collection :text) #":")]
        (println (str "A challenge has been cancelled with the id: " (get command 1)))
      )
    )
  )
)

(defn receive-win-command
  "Handles receiving a win command"
  []
  
  (println "1. Agree")
  (println "2. Disagree")
  (flush)
  (let [rawCommand (Integer. (read-line))]
    (println "Winning Play Dispute")
    (println rawCommand)
    (if (= rawCommand 1)
      (do
        
      )
      
    )
  )
)

(defn reset-game
  "Resets the game based on a win or loss"
  [win]
  (if (= win true)
    (do
      (println "Congratulations! You've won the game!")
      (println "The winning board!")
      (print-board (get @game :board))
      (println "Ending game...")
      (reset! game {
        :my-challenge-id (gen-unique-id 4)
        :open-challenge 0
        :board empty-board
        :my-turn 0
        :game-in-progress 0
        :current-game-id ""
        :waiting-for-win-agree 0
        :win-dispute 0
      })
      (print-controls)
    )
    (do
      (println "Darn! You've lost the game!")
      (println "The winning board!")
      (print-board (get @game :board))
      (println "Ending game...")
      (reset! game {
        :my-challenge-id (gen-unique-id 4)
        :open-challenge 0
        :board empty-board
        :my-turn 0
        :game-in-progress 0
        :current-game-id ""
        :waiting-for-win-agree 0
        :win-dispute 0
      })
      (print-controls)
    )
  )
)

(defn receive-win-agree
  "Handles receiving a win agree"
  []
  (if (= (get @game :waiting-for-win-agree) 1)
    (reset-game true)
  )
)

(defn receive-play-command
  "Handles receiving a play command"
  [command]
  (if (= (get @game :current-game-id) (get command 1))
    (do
      (if (and (= (get @game :my-turn) 0) (= (get @game :game-in-progress) 1))
        (do
          (let [board (get @game :board)]
            (swap! game assoc-in [:board] (insert board (Integer. (get command 2)) 2))
          )
          (swap! game assoc :my-turn 1)
          (print-board (get @game :board))
        )
      ) 
    )
  )
)

(defn receive-accept-challenge
  "Handles receiving a challenge"
  [gameId]
  (do
    (swap! game
      (fn [current-state]
        (merge-with + current-state {:game-in-progress 1 :my-turn 1})
      )
    )
    (swap! game assoc :current-game-id gameId)
    (println "Challenge Accepted!")
    (println "Starting game, you go first!")
    (print-board (get @game :board))
    (print-controls)
  )
)

(defn play-piece
  "Plays a piece on the board if it is your turn"
  []
  (if (= (get @game :my-turn) 1)
    (do
      (println "Enter your move:")
      (let [rawCommand (Integer. (read-line))]
        (swap! game assoc :my-turn 0)
        (let [board (get @game :board)]
          (swap! game assoc-in [:board] (insert board rawCommand 1))
          (if (= (check-win (get @game :board) rawCommand) true)
            (do
              (irc/message connection (get @game :current-irc-channel) (str "WIN:" (get @game :current-game-id) ":" rawCommand))
              (swap! game assoc :waiting-for-win-agree 1)
            )
            (irc/message connection (get @game :current-irc-channel) (str "PLAY:" (get @game :current-game-id) ":" rawCommand))
          )
        )  
      )
    )
    (println "It is not your turn!")
  )
)

(defn my-turn
  "Tells you if it is your turn or not"
  []
  (if (= (get @game :my-turn) 1)
    (println "It is your turn!")
    (println "Not your turn!")
  )
)

(defn open-challenge
  "Opens a challenge via IRC"
  []
  (if (= (get @game :open-challenge) 0)
    (do
      (irc/message connection (get @game :current-irc-channel) (str "OPEN_CHALLENGE:V1:" (get @game :my-challenge-id)))
      (swap! game
        (fn [current-state]
          (merge-with + current-state {:open-challenge 1})
        )
      )
    )
    (println "You already have an open challenge. Cancel it to open another.")
  )
)

(defn cancel-challenge
  "Cancels an open challenge via IRC"
  []
  (if (= (get @game :open-challenge) 1)
    (do
      (irc/message connection (get @game :current-irc-channel) (str "CANCEL_CHALLENGE:" (get @game :my-challenge-id)))
      (swap! game assoc :open-challenge 0)
      (swap! game assoc :my-challenge-id (gen-unique-id 4))
    )
    (println "You have no open challenges to cancel.")
  )
)

(defn accept-challenge
  "Accepts a challenge"
  []
  (println "Enter the id of the challenge you want to accept. Please ensure the challenge is active before accepting.")
  (let [rawCommand (read-line)]
    (do
      (swap! game
        (fn [current-state]
          (merge-with + current-state {:game-in-progress 1 :my-turn 0})
        )
      )
      (swap! game assoc :current-game-id rawCommand)
      (println "Challenge Accepted!")
      (println "Starting game, your opponent goes first!")
      (print-board (get @game :board))
      (print-controls)
    )
  )
)

(defn win-agree
  "Handles agreeing to a win from opponent"
  []
  (irc/message connection (get @game :current-irc-channel) (str "WIN_AGREE"))
  (reset-game false)
)

(defn win-disagree
  "Handle disagreeing to a win from opponent and calling them a cheater"
  []
  (irc/message connection (get @game :current-irc-channel) (str "CHEAT:WIN_DISAGREE"))
  (swap! game assoc :win-dispute 0)
  (print-controls)
)

(defn change-channel
  "Changes the IRC channel"
  []
  (println "Enter the name of the channel you want to join. Don't include the # symbol!")
  (let [rawCommand (read-line)]
    (do
      (irc/part connection (get @game :current-irc-channel))
      (swap! game assoc :current-irc-channel (str "#" rawCommand))
      (irc/join connection (get @game :current-irc-channel))
      (print-controls)
    )
  )
)

(defn resign-game
  "Resigns the current game"
  []
  (if (= (get @game :game-in-progress) 1)
    (do
      (irc/message connection (get @game :current-irc-channel) (str "RESIGN:" (get @game :current-game-id)))
      (reset-game false)
    )
  )
)

(defn handle-it
  "Handles it"
  [command]
  (if (= (get @game :game-in-progress) 0)
    (do
      (case command
        1 (open-challenge)
        2 (cancel-challenge)
        3 (accept-challenge)
        4 (change-channel)
      )
    )
    (do
      (if (= (get @game :win-dispute) 0)
        (do
          (case command
            1 (play-piece)
            2 (print-board (get @game :board))
            3 (my-turn)
            4 (resign-game)
          )
        )
        (do
          (case command
            1 (win-agree)
            2 (win-disagree)
          )
        )
      )
    )
  )
)

(defn print-controls
  "Prints out controls for Connect 4"
  []
  (println "Your options: ")
  (if (= (get @game :game-in-progress) 0)
    (do
      (println "1. Open Challenge")
      (println "2. Cancel Challenge")
      (println "3. Accept Challenge")
      (println "4. Change Channels")
    )
    (do
      (if (= (get @game :win-dispute) 0)
        (do
          (println "1. Play Piece")
          (println "2. Print Board")
          (println "3. My turn?")
          (println "4. Resign Current Game")
        )
        (do
          (println "1. Agree")
          (println "2. Disagree")
        )
      )
    )
  )
)

(defn -main
  "Main control loop"
  [& args]
  (println "Welcome to Clojure Connect 4!")
  (println "Joining IRC server...")
  (irc/join connection (get @game :current-irc-channel))
  (println (str "Currently in the " (get @game :current-irc-channel) " channel"))
  (print-controls)
  (loop [rawCommand (read-line)]
    (handle-it (Integer. rawCommand))  
    (recur (read-line))
  )
)
