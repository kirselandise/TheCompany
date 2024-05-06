; CS353 Functional Programming | Trinity McCann
; Hello, here is my final project, inspired by Lethal Company

#lang racket

; Method to get user input
(define (get-user-input)
    (read-line (current-input-port) 'any))

; String equals symbol method
(define/contract (string-symbol-ci=? str symbol)
    (string? symbol? . -> . boolean?)
    (equal? (string->symbol (string-upcase str)) symbol))

; Items that can placed in inventory
(define laser-blaster '(LASER-BLASTER "> Laser Blaster" "A high-powered laser weapon for self-defense (the Company does not condone the use of blasters outside of self defense)"))
(define data-core-1 '(DATA-CORE-1 "> Data Core #1" "An encrypted data core containing valuable intel"))
(define data-core-2 '(DATA-CORE-2 "> Data Core #2" "Encrypted data core with coordinates to a safe facility"))
(define data-core-3 '(DATA-CORE-3 "> Data Core #3" "The last piece of intel needed to access the safe facility"))

; Item getter methods
(define (get-item-id item)
    (first item))
(define (get-item-name item)
    (second item))
(define (get-item-description item)
    (third item))
(define (get-item-print-string item)
    (~a (get-item-name item) ": " (get-item-description item)))
(define (get-item-by-name name)
    (cond
        [(string-ci=? name "Laser Blaster") laser-blaster]
        [(string-ci=? name "Data Core #1") data-core-1]
        [(string-ci=? name "Data Core #2") data-core-2]
        [(string-ci=? name "Data Core #3") data-core-3]))

; Locator method
(define (create-room index room-id room-name adj-rooms items)
    `(,index ,room-id ,room-name ,adj-rooms ,items))

; Defines each room with a numeric value, a name, adjacent rooms and their connections, and any items within
(define room-1  `(0 ROOM-1 "Entrance Bay" ( (ROOM-5 AFT) ) ()))
(define room-2  `(1 ROOM-2 "Crew Quarters" ( (ROOM-5 PORT) (ROOM-3 AFT) ) ()))
(define room-3  `(2 ROOM-3 "Engineering" ( (ROOM-2 FORE) (ROOM-6 PORT) (ROOM-4 AFT) ) ()))
(define room-4  `(3 ROOM-4 "Storage Bay" ( (ROOM-3 FORE) ) (,data-core-1)))
(define room-5  `(4 ROOM-5 "Central Corridor" ( (ROOM-1 FORE) (ROOM-7 PORT) (ROOM-6 AFT) (ROOM-2 STARBOARD)) (,laser-blaster)))
(define room-6  `(5 ROOM-6 "Maintenance Bay" ( (ROOM-3 STARBOARD) (ROOM-5 FORE)) ()))
(define room-7  `(6 ROOM-7 "Communications" ( (ROOM-5 STARBOARD) (ROOM-8 AFT)) (,data-core-2)))
(define room-8  `(7 ROOM-8 "Science Lab" ( (ROOM-7 FORE) (ROOM-11 PORT) (ROOM-9 AFT)) ()))
(define room-9  `(8 ROOM-9 "Weapons Bay" ( (ROOM-8 FORE) (ROOM-12 PORT)) ()))
(define room-10 `(9 ROOM-10 "Escape Pods" ( (ROOM-11 AFT)) ()))
(define room-11 `(10 ROOM-11 "Bridge" ( (ROOM-10 FORE) (ROOM-8 STARBOARD)) ()))
(define room-12 `(11 ROOM-12 "Captain's Quarters" ( (ROOM-9 STARBOARD)) (,data-core-3)))

; Room getter methods
(define (get-room-index room)
    (first room))
(define (get-room-id room)
    (second room))
(define (get-room-name room)
    (third room))
(define (get-adj-room-descriptors room)
    (fourth room))
(define (get-items-from-room room)
    (fifth room))
(define (get-room-by-id room-id ship-map)
    (first (filter (λ (room) (equal? (get-room-id room) room-id)) ship-map)))
(define (get-adj-room-by-direction room direction ship-map)
    (define target-room-id (first (first
                                        (filter 
                                            (λ (adj-room-descriptor) (equal? (second adj-room-descriptor) direction)) 
                                            (get-adj-room-descriptors room)))))
    (get-room-by-id target-room-id ship-map))
    
; Methods that add and remove items from a room, and then updates that room
(define (add-item-to-room item room)
    (create-room 
        (get-room-index room) 
        (get-room-id room) 
        (get-room-name room)
        (get-adj-room-descriptors room)
        (append (get-items-from-room room) `(,item))))
(define (remove-item-from-room item room)
    (create-room
        (get-room-index room)
        (get-room-id room)
        (get-room-name room)
        (get-adj-room-descriptors room)
        (remove item (get-items-from-room room))))
(define (replace-room old-room new-room game-state)
    (define ship-map (get-ship-map game-state))
    (define new-ship-map (list-set ship-map (get-room-index old-room) new-room))
    (cond
        [(equal? (get-current-room game-state) old-room) `(,new-room ,new-ship-map)]
        [else `(,(get-current-room game-state) ,new-ship-map)]))

; Defines every room numerically
(define ship-map `(
    ,room-1
    ,room-2
    ,room-3
    ,room-4
    ,room-5
    ,room-6
    ,room-7
    ,room-8
    ,room-9
    ,room-10
    ,room-11
    ,room-12))

; Used in updating the state of the game
(define game-state `(,room-1 ,ship-map))

; Getter methods for game state
(define (get-current-room game-state)
    (first game-state))
(define (get-ship-map game-state)
    (second game-state))

; Used in tracking item collection
(define (item-discovered? item discovered-list)
    (cond
        [(equal? (get-item-id item) 'LASER-BLASTER) (first discovered-list)]
        [(equal? (get-item-id item) 'DATA-CORE-1) (second discovered-list)]
        [(equal? (get-item-id item) 'DATA-CORE-2) (third discovered-list)]
        [(equal? (get-item-id item) 'DATA-CORE-3) (fourth discovered-list)]))
(define (discover-item item discovered-list)    
    (cond
        [(equal? (get-item-id item) 'LASER-BLASTER) (flatten `(#t ,(rest discovered-list)))]
        [(equal? (get-item-id item) 'DATA-CORE-1) (flatten `(,(first discovered-list) #t ,(drop discovered-list 2)))]
        [(equal? (get-item-id item) 'DATA-CORE-2) (flatten `(,(take discovered-list 2) #t ,(last discovered-list)))]
        [(equal? (get-item-id item) 'DATA-CORE-3) (flatten `(,(take discovered-list 3) #t))]))

; Converts static variables(all caps case) to reading text
(define (get-direction-name direction)
    (cond
        [(equal? direction 'FORE) "Fore"] ;Fore is the front of the ship
        [(equal? direction 'PORT) "Port"] ;Port is the left of the ship
        [(equal? direction 'AFT) "Aft"] ;Aft is the back of the ship
        [(equal? direction 'STARBOARD) "Starboard"])) ;Starboard is the right of the ship

; Menu provided to the user to change game state
(define (display-user-options)
    (displayln "> Input T to travel to a different room")
    (displayln "> Input R to report the current room")
    (displayln "> Input D to drop item")
    (displayln "> Input P to pick up item")
    (displayln "> Input I to see the inventory")
    (displayln "> Input S to search the room")
    (displayln "> Input E to describe the room")
    (displayln "> Input H if you are unsure what to do.\n"))

; Returns new gamestate
(define (move-to user-choice game-state)
    (define current-room (get-current-room game-state))
    (define ship-map (get-ship-map game-state))
    (define new-room (get-adj-room-by-direction current-room (string->symbol (string-upcase user-choice)) ship-map))
    `(,new-room ,ship-map))

; Updates game state
(define (travel game-state)
    (displayln "\n> You see doors leading:\n")
    (define adj-room-descriptors (get-adj-room-descriptors (get-current-room game-state)))
    (for-each displayln (map 
                            (λ (adj-room-descriptor) (get-direction-name (second adj-room-descriptor))) 
                            adj-room-descriptors))

    (displayln "\n> Please input the direction you would like to go\n")
    (define user-choice (get-user-input))
    (cond
        [(ormap 
            (λ (adj-room-descriptor) (string-symbol-ci=? user-choice (second adj-room-descriptor))) 
            adj-room-descriptors)
            (move-to user-choice game-state)]
        [else 
            (displayln "> Invalid input, try again\n")
            game-state]))

; Prints room
(define (report-current-room game-state)
    (displayln (~a "\n> You are at " (get-room-name (get-current-room game-state)) "\n")))

; Search function
(define (describe-current-room game-state discovered-list)
    (define current-room (get-current-room game-state))
    (define found-items (filter (λ (item) (item-discovered? item discovered-list)) (get-items-from-room current-room)))
    (cond 
        [(empty? found-items)
            (displayln "> It appears no important items are nearby; no monsters though either")]
        [else
            (displayln "> You find something!\n")
            (for-each (λ (item) (displayln (get-item-print-string item))) found-items)
            (display "")]))

; Inventory function
(define (list-inventory inventory)
    (cond
        [(empty? inventory)
             (displayln "> You have no items\n")]
        [else
            (displayln "> You have the following items:\n")
            (for-each (λ (item) (displayln (get-item-print-string item))) inventory)
            (displayln "")]))

; Pick-up function
(define (pick-up-item game-state inventory discovered-list)
    (displayln "> What would you like to pick-up?\n")
    (define items (get-items-from-room (get-current-room game-state)))
    (define found-items (filter (λ (item) (item-discovered? item discovered-list)) items))
    (define user-choice (get-user-input))
    (cond  
        [(empty? (filter (λ (item) (string-ci=? user-choice (get-item-name item))) found-items))
            (displayln "\n> Invalid input\n")
            `(,game-state ,inventory)]
        [else 
            (define current-room (get-current-room game-state))
            (define item (get-item-by-name user-choice))
            (define updated-inventory (append inventory `(,item)))
            (define updated-room (remove-item-from-room item current-room))
            (define updated-game-state (replace-room current-room updated-room game-state))
            (displayln (~a "> You picked up the " (get-item-print-string item) "\n"))
            `(,updated-game-state ,updated-inventory)]))

; Drop function
(define (drop-item game-state inventory)
    (displayln "> What would you like to drop?\n")
    (define user-choice (get-user-input))

    (cond
        [(empty? (filter (λ (item) (string-ci=? user-choice (get-item-name item))) inventory))
            (displayln "\n> Invalid input\n")
            `(,game-state ,inventory)]
        [else  
            (define current-room (get-current-room game-state))
            (define item (get-item-by-name user-choice))
            (define updated-inventory (remove item inventory))
            (define updated-room (add-item-to-room item current-room))
            (define updated-game-state (replace-room current-room updated-room game-state))
            (displayln (~a "> You dropped the " (get-item-print-string item)))
            `(,updated-game-state ,updated-inventory)]))
            
; Search function
(define (search game-state discovered-list) 
    (define items (get-items-from-room (get-current-room game-state)))
    (define unfound-items (filter (λ (item) (not (item-discovered? item discovered-list))) items))
    (cond
        [(empty? unfound-items)
            (displayln "> No new items\n")
            discovered-list]
        [else 
            (displayln "> You've found \n")
            (for-each (λ (unfound-item) (displayln (get-item-print-string unfound-item))) unfound-items)
            (displayln "")
            (foldl discover-item discovered-list unfound-items)]))

; Help function: location, inventory, available actions
(define (help game-state inventory)
    (report-current-room game-state)
    (list-inventory inventory)
    (display "> You may move rooms, report the room you're in, pick up an item, see your inventory, search the room, and describe the room.\n"))

; Sets end goal of game ending when the data cores are dropped at the escape pods
(define (game-ended? game-state)
    (define ending-room (get-room-by-id 'ROOM-10 (get-ship-map game-state)))
    (define items (get-items-from-room ending-room))
    (cond  
        [(and 
            (ormap (λ (item) (equal? (get-item-id item) 'DATA-CORE-1)) items)
            (ormap (λ (item) (equal? (get-item-id item) 'DATA-CORE-2)) items)
            (ormap (λ (item) (equal? (get-item-id item) 'DATA-CORE-3)) items))
            #t]
        [else 
            #f]))

; Plays the game, welcome to the Company!
(define (play-game game-state [inventory '()])
    (displayln "> Welcome new Recruit! The Company welcomes you to your-\n> welcomes you to your-\n> your- your- your-\n> Apologies, the SS5126 has become comprimised.\n> Please locate the 3 data cores and present them to the escape pods.\n> The Company looks forward to seeing you come home!\n> (please remember anything not outlined in your will is considered to be Company property)\n")
    (define (play-game-loop game-state inventory [discovered-list '(#f #f #f #f)])
        (unless (game-ended? game-state)
            (display-user-options)
            (define user-choice (get-user-input))
            (cond
                [(string-ci=? user-choice "T") 
                    (play-game-loop (travel game-state) inventory discovered-list)]

                [(string-ci=? user-choice "R")
                    (report-current-room game-state)
                    (play-game-loop game-state inventory discovered-list)]

                [(string-ci=? user-choice "S") 
                    (play-game-loop game-state inventory (search game-state discovered-list))]

                [(string-ci=? user-choice "P") 
                    (define result (pick-up-item game-state inventory discovered-list))
                    (play-game-loop (first result) (second result) discovered-list)]

                [(string-ci=? user-choice "I") 
                    (list-inventory inventory)
                    (play-game-loop game-state inventory discovered-list)]

                [(string-ci=? user-choice "D")
                    (define result (drop-item game-state inventory))
                    (play-game-loop (first result) (second result) discovered-list)]
                
                [(string-ci=? user-choice "E")
                    (describe-current-room game-state discovered-list)
                    (play-game-loop game-state inventory discovered-list)]

                [(string-ci=? user-choice "H")
                    (help game-state inventory)
                    (play-game-loop game-state inventory discovered-list)]

                [else 
                    (displayln "> Invalid option! Please enter again\n")
                    (play-game-loop game-state inventory)])))
    (play-game-loop game-state inventory)
    (displayln "> You have escaped the starship. The Company is very please to see you alive, please take this golden star as a token of our appreciation ☆.\n"))

(play-game game-state)
