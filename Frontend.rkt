#lang racket/gui

; #######
; IMPORTS
; #######

; Required library.
(require racket/draw
         net/url)

; Requiered file.
(require "Backend.rkt")

; Main Window
(define my-window (new frame%
                       [label "BlaCE Jack"]
                       [width 1000]
                       [height 700]
                       [style '(no-resize-border)]
                       [alignment '(left top)]))

; #########
; FUNCTIONS
; #########

; Function name:Add Names
; Description: sets the names of the players in the respective label of the GUI
; Input: number of players
; Output: void
(define (add-names num-of-players)
  (cond ( (= num-of-players 1)
          (send player1-name set-label player1-name-variable))
        
        ( (= num-of-players 2)
          (send player1-name set-label player1-name-variable)
          (send player2-name set-label player2-name-variable)
          (send hor-pane-2 add-child ver-pane-2.2))
        
        ( (= num-of-players 3)
          (send player1-name set-label player1-name-variable)
          (send player2-name set-label player2-name-variable)
          (send hor-pane-2 add-child ver-pane-2.2)
          (send player3-name set-label player3-name-variable)
          (send hor-pane-2 add-child ver-pane-2.3))))

; ####################
; VARIABLES DEFINITION
; ####################

; Variable name: Players-List.
; Description: list of all the players.
(define players-list
  null)

; Variable name: Shuffled-Deck.
; Description: list of all shuffled cards.
(define shuffled-deck
  (shuffle original-deck))

; Variable name: Player1-Name-Variable.
; Description: name of the first player.
(define player1-name-variable
  null)

; Variable name: Player2-Name-Variable.
; Description: name of the second player.
(define player2-name-variable
  null)

; Variable name: Player1-Name-Variable.
; Description: name of the third player.
(define player3-name-variable
  null)

; Variable name: current-player.
; Description: shows the player who has the current turn.
(define current-player
  0)

; Variable first card
; Description: saves the value of the first card of the crupier
(define first-card
  "")

; ################
; BUTTON RESPONSES
; ################

; Function name: Next-Button-Response.
; Description: this is the action of the next-button and is responsible for increasing the value of the current-player. 
; Input: a button instance and a clicked event.
; Output: void.
(define (next-button-response button event)

  ; define current player
  (cond ((>= current-player (- (length players-list) 1))
         (set! current-player 0))
        (else
         (set! current-player (+ current-player 1))))

  ; auxiliar funtions for changing the turn
  (add-current-player-name)
  (enable-after-next)
  (set-current-total current-player)
  (hide-last-total)

  ; checks if the game has finished
  (cond ( (everyone-stay? players-list)
          (send next-button enable #f)
          (send stay-button enable #f)
          (send take-button enable #f)
          (send turn-message set-label "Game Over")
          (show-hidden-card)
          (send-everyone-total players-list 0)
          (check-results))))
          
; Function name: Show Hidden Card.
; Description: turns the crupier's hidden card around (happens at the end of the game).
; Input: void.
; Output: void.
(define (show-hidden-card)
  (send hor-pane-1.2.1 delete-child (car (send hor-pane-1.2.1 get-children)))
  (add-card 0 first-card))

; Function name: Send Everyone Total.
; Description: changes the respective labels with the player's
; total score (happens at the end of the game)
; Input: the list of players and the number of the player to update.
; Output. void.
(define (send-everyone-total list number)
  (cond ( (null? list))
        ( else
          (set-current-total number)
          (send-everyone-total (cdr list) (+ number 1))))
  (send crupier-total set-label
                (string-append "Total: " (number->string
                                          (check-score (get-crupier players-list))))))

; Function name: Check Results.
; Description: determines the final results of the game, based on the game rules,
; the results are shown in the GUI with the words "Tied", "Lost" or "Won"
; Input: void.
; Output: void.
(define (check-results)
  ; Definition of important variables
  ; Scores
  (define crupier-score (check-score (get-crupier players-list)))
  (define player1-score (check-score (get-player1 players-list)))
  ; Perfect Matches (an A with a 10 value card)
  (define crupier-perfect? (perfect-match? (get-crupier players-list)))
  (define player1-perfect? (perfect-match? (get-player1 players-list)))
  ; Individual results
  (define player1-result (check-result-aux crupier-score player1-score crupier-perfect? player1-perfect?))

  ; Gets the result for player 1
  (cond ( (equal? player1-result "Crupier")
          (send player1-state set-label "You Lost"))
        ( (equal? player1-result "Player")
          (send player1-state set-label "You Won"))
        ( (equal? player1-result "Tie")
          (send player1-state set-label "You Tied")))
  
  ; Gets the result for player 2 (if there is so)
  (cond ( (> (length players-list) 2)
          (define player2-score (check-score (get-player2 players-list)))
          (define player2-perfect? (perfect-match? (get-player2 players-list)))
          (define player2-result (check-result-aux crupier-score player2-score crupier-perfect? player2-perfect?))
          (cond ( (equal? player2-result "Crupier")
                  (send player2-state set-label "You Lost"))
                ( (equal? player2-result "Player")
                  (send player2-state set-label "You Won"))
                ( (equal? player2-result "Tie")
                  (send player2-state set-label "You Tied")))))
  
  ; Gets the result for player 3 (if there is so)
  (cond ( (> (length players-list) 3)
          (define player3-score (check-score (get-player3 players-list)))
          (define player3-perfect? (perfect-match? (get-player3 players-list)))
          (define player3-result (check-result-aux crupier-score player3-score crupier-perfect? player3-perfect?))
          (cond ( (equal? player3-result "Crupier")
                  (send player3-state set-label "You Lost"))
                ( (equal? player3-result "Player")
                  (send player3-state set-label "You Won"))
                ( (equal? player3-result "Tie")
                  (send player3-state set-label "You Tied"))))))
        
          
; Function name: Check Results Auxiliar.
; Description: Gicen the total scores and the perfect matches, compares
; the given player with the crupier to determine who won.
; Inputs: total score of the crupier, total score of the given player,
; boolean that indicates if the crupier has a perfect play and another boolean
; for the player.
; Output: A string that indicates if the player won, lost or tied the match.
(define (check-result-aux cr-total pl-total cr-perfect pl-perfect)

  (cond ( (> cr-total pl-total)
          (cond ( (and (> cr-total 21) (> pl-total 21))
                  "Tie")
                ( (> cr-total 21)
                  "Player")
                ( else "Crupier")))
        ( (< cr-total pl-total)
          (cond ( (and (> cr-total 21) (> pl-total 21))
                  "Tie")
                ( (> pl-total 21)
                  "Crupier")
                ( else "Player")))
        ( else
          (cond ( (and cr-perfect pl-perfect)
                  "Tie")
                ( cr-perfect "Crupier")
                ( pl-perfect "Player")
                ( else "Tie")))))
                  
; Function name: Perfect Match?
; Description: Determines if a player got a perfect match (a 21 using
; and A card and a card with value 10).
; Input: a list with the player's information.
; Output: a boolean that says if there is a perfect match.
(define (perfect-match? player)
  (cond ( (and (= (check-score player) 21)
               (= (length (cadr player)) 2))
          #t)
        ( else #f)))

; Function name: Everyone Stay?
; Description: indicates if every player in the game is in the stay condition.
; if true the game must stop.
; Input: player's list with their information.
; Output: a boolean with the result.
(define (everyone-stay? list)
  (cond ( (null? list)
          #t)
        ( (not (stay? players-list (caar list)))
          #f)
        ( else
          (everyone-stay? (cdr list)))))
          
; Function name: Add Current Player Name.
; Description: updates the current player label.
; Input: void.
; Output: void.
(define (add-current-player-name)
  (cond ( (= current-player 0)
          (send turn-message set-label "Turn of Crupier"))
        ( (= current-player 1)
          (send turn-message set-label
                (string-append "Turn of " (get-player-name (get-player1 players-list)))))
        ( (= current-player 2)
          (send turn-message set-label
                (string-append "Turn of " (get-player-name (get-player2 players-list)))))
        ( (= current-player 3)
          (send turn-message set-label
                (string-append "Turn of " (get-player-name (get-player3 players-list)))))))

; Function name: Hide Last Total
; Description: hides the total score of the last player after
; hitting the next button.
; Input: void.
; Output: void.
(define (hide-last-total)
  (cond ( (= current-player 0)
          (send player3-total set-label "Total: --"))
        ( (= current-player 1)
          (send crupier-total set-label "Total: --"))
        ( (= current-player 2)
          (send player1-total set-label "Total: --"))
        ( (= current-player 3)
          (send player2-total set-label "Total: --"))))

; Function name: Enable After Next.
; Description: enables and disables the necesary buttons after
; hitting the next buttons, this helps the flow of the game.
; Input: void.
; Output: void.
(define (enable-after-next)
  (cond ( (stay? players-list current-player)
          (send stay-button enable #f)
          (send take-button enable #f)
          (send next-button enable #t))
        ( else
          (send next-button enable #f)
          (send stay-button enable #t)
          (send take-button enable #t)))
  (cond ( (= current-player 0)
          (send stay-button enable #f))))        

; Function name: Take-Button-Response.
; Description: this is the action of the take-button and is responsible for giving new cards to the players and delete them from the deck. 
; Input: a button instance and a clicked event.
; Output: void.
(define (take-button-response button event)

  ; checks if it corresponds to the hidden card.
  (cond ( (and (= current-player 0) (= (length (cadar players-list)) 0))
          (add-card current-player "hidden-card")
          (set! first-card (car shuffled-deck))
          (set! players-list (add-card-to-player players-list current-player first-card)))
        
        ( else
          (set! players-list (add-card-to-player players-list current-player (car shuffled-deck)))
          (add-card current-player (get-card players-list current-player))))

  ; Auxiliar functions after hitting the take button.
  (set! shuffled-deck (delete-card shuffled-deck))
  (update-deck-label)  
  (set-current-total current-player)
  (enable-after-take)
  (set-stay-over21))

; Function Name: Update Deck Label.
; Description: updates the text of the cards left (decreses one card
; every time someone takes a card).
; Input: void.
; Output: void. 
(define (update-deck-label)
  (send cards-left set-label
        (string-append (number->string (length shuffled-deck)) " cards left")))

; Function Name: Set Stay Over21?
; Description: sets the stay state for a player that gets more than 21 as a total.
; Input: void
; Output: void. 
(define (set-stay-over21)
  (cond ( (= current-player 0)
          (cond ( (> (check-score (get-crupier players-list)) 21)
                  (set! players-list (set-stay-to-player players-list current-player))
                  (set-stayed-label))
                
                ( (= (check-score (get-crupier players-list)) 21)
                  (set! players-list (set-stay-to-player players-list current-player))
                  (set-stayed-label))
                
                ( (>= (check-score (get-crupier players-list)) 17)
                  (set! players-list (set-stay-to-player players-list current-player))
                  (set-stayed-label))))
        
        ( (= current-player 1)
          (cond ( (> (check-score (get-player1 players-list)) 21)
                  (set! players-list (set-stay-to-player players-list current-player))
                  (set-stayed-label)
                  (send player1-state set-label "Over 21"))
                
                ( (= (check-score (get-player1 players-list)) 21)
                  (set! players-list (set-stay-to-player players-list current-player))
                  (set-stayed-label)
                  (send player1-state set-label "You got 21!"))))
        
        ( (= current-player 2)
          (cond ( (> (check-score (get-player2 players-list)) 21)
                  (set! players-list (set-stay-to-player players-list current-player))
                  (set-stayed-label)
                  (send player2-state set-label "Over 21"))
                
                ( (= (check-score (get-player2 players-list)) 21)
                  (set! players-list (set-stay-to-player players-list current-player))
                  (set-stayed-label)
                  (send player2-state set-label "You got 21!"))))
        
        ( (= current-player 3)
          (cond ( (> (check-score (get-player3 players-list)) 21)
                  (set! players-list (set-stay-to-player players-list current-player))
                  (set-stayed-label)
                  (send player3-state set-label "Over 21"))
                
                ( (= (check-score (get-player3 players-list)) 21)
                  (set! players-list (set-stay-to-player players-list current-player))
                  (set-stayed-label)
                  (send player3-state set-label "You got 21!"))))))

; Function name: Set Currente Total.
; Description: adds the label of the total for the curent player.
; Input: the number of the current player.
; Output: void.
(define (set-current-total current-player)
  (cond ( (= current-player 0)
          (send crupier-total set-label
                (string-append "Total: Crupier's game is private")))
        ( (= current-player 1)
          (send player1-total set-label
                (string-append "Total: " (number->string
                                          (check-score (get-player1 players-list))))))
        ( (= current-player 2)
          (send player2-total set-label
                (string-append "Total: " (number->string
                                          (check-score (get-player2 players-list))))))
        ( (= current-player 3)
          (send player3-total set-label
                (string-append "Total: " (number->string
                                          (check-score (get-player3 players-list))))))))

; Function name: Enable After Take.
; Description: enables or disables the necessary
; buttons after hitting the take button.
; Input: void.
; Output: void.
(define (enable-after-take)
  (send take-button enable #f)
  (send stay-button enable #f)
  (send next-button enable #t))


; Function name: Stay-Button-Response.
; Description: this is the action of the stay-button and is responsible for assigning the stay position of the current player. 
; Input: a button instance and a clicked event.
; Output: void.
(define (stay-button-response button event)

  (set! players-list (set-stay-to-player players-list current-player))
  (enable-after-take)
  (set-stayed-label))

; Function name: Set Stayed Label.
; Description: activates the stayed label for the current player.
; Input: void.
; Output: void.
(define (set-stayed-label)
  (cond ( (= current-player 0)
          (send ver-pane-1.2 add-child crupier-state))
        ( (= current-player 1)
          (send ver-pane-2.1 add-child player1-state))
        ( (= current-player 2)
          (send ver-pane-2.2 add-child player2-state))
        ( (= current-player 3)
          (send ver-pane-2.3 add-child player3-state))))


; ##########
; GUI PANELS
; ##########

; Horizontal Pane 1
(define hor-pane-1 (new horizontal-pane%
                        [parent my-window]
                        [vert-margin 10]
                        [horiz-margin 10]
                        [alignment '(left top)]))

; Vertical Pane 1.1
(define ver-pane-1.1 (new vertical-pane%
                          [parent hor-pane-1]
                          [vert-margin 10]
                          [horiz-margin 10]
                          [alignment '(left top)]))

; Vertical Pane 1.2
(define ver-pane-1.2 (new vertical-panel%
                          [parent hor-pane-1]
                          [vert-margin 10]
                          [horiz-margin 10]
                          [alignment '(center center)]
                          [style '(border)]
                          [min-width 200]))

; Crupier Cards Pane 1.2.1
(define hor-pane-1.2.1 (new horizontal-pane%
                        [parent ver-pane-1.2]
                        [vert-margin 10]
                        [horiz-margin 10]
                        [alignment '(center center)]))

; Vertical Pane 1.3
(define ver-pane-1.3 (new vertical-pane%
                          [parent hor-pane-1]
                          [vert-margin 10]
                          [horiz-margin 10]
                          [alignment '(center center)]))

; Horizontal Pane 2
(define hor-pane-2 (new horizontal-pane%
                        [parent my-window]
                        [vert-margin 10]
                        [horiz-margin 10]
                        [alignment '(left top)]))

; Vertical Pane 2.1
(define ver-pane-2.1 (new vertical-panel%
                          [parent hor-pane-2]
                          [vert-margin 10]
                          [horiz-margin 10]
                          [alignment '(center center)]
                          [style '(border)]))

; Player 1 Cards Pane 2.1.1
(define hor-pane-2.1.1 (new horizontal-pane%
                        [parent ver-pane-2.1]
                        [vert-margin 10]
                        [horiz-margin 10]
                        [alignment '(center center)]))

; Vertical Pane 2.2
(define ver-pane-2.2 (new vertical-panel%
                          [parent hor-pane-2]
                          [vert-margin 10]
                          [horiz-margin 10]
                          [alignment '(center center)]
                          [style '(border deleted)]))

; Player 1 Cards Pane 2.2.1
(define hor-pane-2.2.1 (new horizontal-pane%
                        [parent ver-pane-2.2]
                        [vert-margin 10]
                        [horiz-margin 10]
                        [alignment '(center center)]))

; Vertical Pane 2.3
(define ver-pane-2.3 (new vertical-panel%
                          [parent hor-pane-2]
                          [vert-margin 10]
                          [horiz-margin 10]
                          [alignment '(center center)]
                          [style '(border deleted)]))

; Player 1 Cards Pane 2.3.1
(define hor-pane-2.3.1 (new horizontal-pane%
                        [parent ver-pane-2.3]
                        [vert-margin 10]
                        [horiz-margin 10]
                        [alignment '(center center)]))
                        
; ############
; GUI ELEMENTS
; ############

; Turn label
(define turn-message (new message%
                          [parent ver-pane-1.1]
                          [label "Turn of Crupier"]
                          [font (make-object font% 25 'default 'normal 'normal)]
                          [stretchable-width #t])) 

; Next Player Button
(define next-button (new button%
                         [parent ver-pane-1.1]
                         [label "Next"]
                         [min-width 200]
                         [min-height 30]
                         [font (make-object font% 15 'default 'normal 'normal)]
                         [callback next-button-response]
                         [enabled #f]))

; Take Card Button
(define take-button (new button%
                         [parent ver-pane-1.1]
                         [label "Take Card"]
                         [min-width 200]
                         [min-height 30]
                         [font (make-object font% 15 'default 'normal 'normal)]
                         [callback take-button-response]))

; Stay With Current Cards Button
(define stay-button (new button%
                         [parent ver-pane-1.1]
                         [label "Stay"]
                         [min-width 200]
                         [min-height 30]
                         [font (make-object font% 15 'default 'normal 'normal)]
                         [callback stay-button-response]
                         [enabled #f]))

; Crupier name
(define crupier-name (new message%
                          [parent ver-pane-1.2]
                          [label "Crupier"]
                          [font (make-object font% 15 'default 'normal 'normal)]))

; Crupier total
(define crupier-total (new message%
                          [parent ver-pane-1.2]
                          [label "Total : --"]
                          [font (make-object font% 15 'default 'normal 'normal)]
                          [stretchable-width #t]))

; Crupier state
(define crupier-state (new message%
                           [parent ver-pane-1.2]
                           [label "Stayed"]
                           [font (make-object font% 15 'default 'normal 'normal)]
                           [style '(deleted)]
                           [stretchable-width #t]))

; Player 1 name
(define player1-name (new message%
                          [parent ver-pane-2.1]
                          [label "Player 1"]
                          [font (make-object font% 15 'default 'normal 'normal)]))

; Player 1 total
(define player1-total (new message%
                          [parent ver-pane-2.1]
                          [label "Total : --"]
                          [font (make-object font% 15 'default 'normal 'normal)]
                          [stretchable-width #t]))

; Player 1 state
(define player1-state (new message%
                           [parent ver-pane-2.1]
                           [label "Stayed"]
                           [font (make-object font% 15 'default 'normal 'normal)]
                           [style '(deleted)]
                           [stretchable-width #t]))

; Player 2 name
(define player2-name (new message%
                          [parent ver-pane-2.2]
                          [label "Player 2"]
                          [font (make-object font% 15 'default 'normal 'normal)]))

; Player 2 total
(define player2-total (new message%
                          [parent ver-pane-2.2]
                          [label "Total : --"]
                          [font (make-object font% 15 'default 'normal 'normal)]
                          [stretchable-width #t]))
; Player 2 state
(define player2-state (new message%
                           [parent ver-pane-2.2]
                           [label "Stayed"]
                           [font (make-object font% 15 'default 'normal 'normal)]
                           [style '(deleted)]
                           [stretchable-width #t]))

; Player 3 name
(define player3-name (new message%
                          [parent ver-pane-2.3]
                          [label "Player 3"]
                          [font (make-object font% 15 'default 'normal 'normal)]))

; Player 3 total
(define player3-total (new message%
                          [parent ver-pane-2.3]
                          [label "Total : --"]
                          [font (make-object font% 15 'default 'normal 'normal)]
                          [stretchable-width #t]))

; Player 3 state
(define player3-state (new message%
                           [parent ver-pane-2.3]
                           [label "Stayed"]
                           [font (make-object font% 15 'default 'normal 'normal)]
                           [style '(deleted)]
                           [stretchable-width #t]))

; Deck image
(define deck (read-bitmap (get-pure-port
                                (string->url "file:/Images/deck.png"))))

(void (new message%
           [parent ver-pane-1.3]
           [label deck]))

; Cards in deck
(define cards-left (new message%
                          [parent ver-pane-1.3]
                          [label "52 cards left"]
                          [font (make-object font% 25 'default 'normal 'normal)]))


; Function name: add-card.
; Description: This function adds a card to the player box in the GUI based on the cards saved on the list.
; Input: The number of the player and the name of the card.
; Output: void.
(define (add-card player-number card-txt)
  (cond ( (equal? player-number 0)
          (add-card-aux hor-pane-1.2.1 card-txt))
        ( (equal? player-number 1)
          (add-card-aux hor-pane-2.1.1 card-txt))
        ( (equal? player-number 2)
          (add-card-aux hor-pane-2.2.1 card-txt))
        ( (equal? player-number 3)
          (add-card-aux hor-pane-2.3.1 card-txt))
        ))

; Auxiliar function of add-card
(define (add-card-aux pane card)
  (void (new message%
             [parent pane]
             [label (card-image card)])))

; Function name: card-image
; Description: Returns the image of the card given the name of it.
; Input: card name as a a text.
; Output: a bitmap that represents the card image.
(define (card-image card-txt)
  (cond ( (equal? card-txt "a-c")
          a-clubs)
        ( (equal? card-txt "2-c")
          2-clubs)
        ( (equal? card-txt "3-c")
          3-clubs)
        ( (equal? card-txt "4-c")
          4-clubs)
        ( (equal? card-txt "5-c")
          5-clubs)
        ( (equal? card-txt "6-c")
          6-clubs)
        ( (equal? card-txt "7-c")
          7-clubs)
        ( (equal? card-txt "8-c")
          8-clubs)
        ( (equal? card-txt "9-c")
          9-clubs)
        ( (equal? card-txt "10-c")
          10-clubs)
        ( (equal? card-txt "j-c")
          j-clubs)
        ( (equal? card-txt "q-c")
          q-clubs)
        ( (equal? card-txt "k-c")
          k-clubs)
        ( (equal? card-txt "a-l")
          a-leaves)
        ( (equal? card-txt "2-l")
          2-leaves)
        ( (equal? card-txt "3-l")
          3-leaves)
        ( (equal? card-txt "4-l")
          4-leaves)
        ( (equal? card-txt "5-l")
          5-leaves)
        ( (equal? card-txt "6-l")
          6-leaves)
        ( (equal? card-txt "7-l")
          7-leaves)
        ( (equal? card-txt "8-l")
          8-leaves)
        ( (equal? card-txt "9-l")
          9-leaves)
        ( (equal? card-txt "10-l")
          10-leaves)
        ( (equal? card-txt "j-l")
          j-leaves)
        ( (equal? card-txt "q-l")
          q-leaves)
        ( (equal? card-txt "k-l")
          k-leaves)
        ( (equal? card-txt "a-d")
          a-diamonds)
        ( (equal? card-txt "2-d")
          2-diamonds)
        ( (equal? card-txt "3-d")
          3-diamonds)
        ( (equal? card-txt "4-d")
          4-diamonds)
        ( (equal? card-txt "5-d")
          5-diamonds)
        ( (equal? card-txt "6-d")
          6-diamonds)
        ( (equal? card-txt "7-d")
          7-diamonds)
        ( (equal? card-txt "8-d")
          8-diamonds)
        ( (equal? card-txt "9-d")
          9-diamonds)
        ( (equal? card-txt "10-d")
          10-diamonds)
        ( (equal? card-txt "j-d")
          j-diamonds)
        ( (equal? card-txt "q-d")
          q-diamonds)
        ( (equal? card-txt "k-d")
          k-diamonds)
        ( (equal? card-txt "a-h")
          a-hearts)
        ( (equal? card-txt "2-h")
          2-hearts)
        ( (equal? card-txt "3-h")
          3-hearts)
        ( (equal? card-txt "4-h")
          4-hearts)
        ( (equal? card-txt "5-h")
          5-hearts)
        ( (equal? card-txt "6-h")
          6-hearts)
        ( (equal? card-txt "7-h")
          7-hearts)
        ( (equal? card-txt "8-h")
          8-hearts)
        ( (equal? card-txt "9-h")
          9-hearts)
        ( (equal? card-txt "10-h")
          10-hearts)
        ( (equal? card-txt "j-h")
          j-hearts)
        ( (equal? card-txt "q-h")
          q-hearts)
        ( (equal? card-txt "k-h")
          k-hearts)
        ( (equal? card-txt "hidden-card")
          hidden-card)
        ))

; ######################
; CARD IMAGES DEFINITION
; ######################

; LEAVES
(define a-leaves (read-bitmap (get-pure-port
                                (string->url "file:/Images/A-leaves.png"))))
(define 2-leaves (read-bitmap (get-pure-port
                                (string->url "file:/Images/2-leaves.png"))))
(define 3-leaves (read-bitmap (get-pure-port
                                (string->url "file:/Images/3-leaves.png"))))
(define 4-leaves (read-bitmap (get-pure-port
                                (string->url "file:/Images/4-leaves.png"))))
(define 5-leaves (read-bitmap (get-pure-port
                                (string->url "file:/Images/5-leaves.png"))))
(define 6-leaves (read-bitmap (get-pure-port
                                (string->url "file:/Images/6-leaves.png"))))
(define 7-leaves (read-bitmap (get-pure-port
                                (string->url "file:/Images/7-leaves.png"))))
(define 8-leaves (read-bitmap (get-pure-port
                                (string->url "file:/Images/8-leaves.png"))))
(define 9-leaves (read-bitmap (get-pure-port
                                (string->url "file:/Images/9-leaves.png"))))
(define 10-leaves (read-bitmap (get-pure-port
                                (string->url "file:/Images/10-leaves.png"))))
(define j-leaves (read-bitmap (get-pure-port
                                (string->url "file:/Images/J-leaves.png"))))
(define q-leaves (read-bitmap (get-pure-port
                                (string->url "file:/Images/Q-leaves.png"))))
(define k-leaves (read-bitmap (get-pure-port
                                (string->url "file:/Images/K-leaves.png"))))

; DIAMONDS
(define a-diamonds (read-bitmap (get-pure-port
                                (string->url "file:/Images/A-diamonds.png"))))
(define 2-diamonds (read-bitmap (get-pure-port
                                (string->url "file:/Images/2-diamonds.png"))))
(define 3-diamonds (read-bitmap (get-pure-port
                                (string->url "file:/Images/3-diamonds.png"))))
(define 4-diamonds (read-bitmap (get-pure-port
                                (string->url "file:/Images/4-diamonds.png"))))
(define 5-diamonds (read-bitmap (get-pure-port
                                (string->url "file:/Images/5-diamonds.png"))))
(define 6-diamonds (read-bitmap (get-pure-port
                                (string->url "file:/Images/6-diamonds.png"))))
(define 7-diamonds (read-bitmap (get-pure-port
                                (string->url "file:/Images/7-diamonds.png"))))
(define 8-diamonds (read-bitmap (get-pure-port
                                (string->url "file:/Images/8-diamonds.png"))))
(define 9-diamonds (read-bitmap (get-pure-port
                                (string->url "file:/Images/9-diamonds.png"))))
(define 10-diamonds (read-bitmap (get-pure-port
                                (string->url "file:/Images/10-diamonds.png"))))
(define j-diamonds (read-bitmap (get-pure-port
                                (string->url "file:/Images/J-diamonds.png"))))
(define q-diamonds (read-bitmap (get-pure-port
                                (string->url "file:/Images/Q-diamonds.png"))))
(define k-diamonds (read-bitmap (get-pure-port
                                (string->url "file:/Images/K-diamonds.png"))))

; CLUBS
(define a-clubs (read-bitmap (get-pure-port
                                (string->url "file:/Images/A-clubs.png"))))
(define 2-clubs (read-bitmap (get-pure-port
                                (string->url "file:/Images/2-clubs.png"))))
(define 3-clubs (read-bitmap (get-pure-port
                                (string->url "file:/Images/3-clubs.png"))))
(define 4-clubs (read-bitmap (get-pure-port
                                (string->url "file:/Images/4-clubs.png"))))
(define 5-clubs (read-bitmap (get-pure-port
                                (string->url "file:/Images/5-clubs.png"))))
(define 6-clubs (read-bitmap (get-pure-port
                                (string->url "file:/Images/6-clubs.png"))))
(define 7-clubs (read-bitmap (get-pure-port
                                (string->url "file:/Images/7-clubs.png"))))
(define 8-clubs (read-bitmap (get-pure-port
                                (string->url "file:/Images/8-clubs.png"))))
(define 9-clubs (read-bitmap (get-pure-port
                                (string->url "file:/Images/9-clubs.png"))))
(define 10-clubs (read-bitmap (get-pure-port
                                (string->url "file:/Images/10-clubs.png"))))
(define j-clubs (read-bitmap (get-pure-port
                                (string->url "file:/Images/J-clubs.png"))))
(define q-clubs (read-bitmap (get-pure-port
                                (string->url "file:/Images/Q-clubs.png"))))
(define k-clubs (read-bitmap (get-pure-port
                                (string->url "file:/Images/K-clubs.png"))))

; HEARTS
(define a-hearts (read-bitmap (get-pure-port
                                (string->url "file:/Images/A-hearts.png"))))
(define 2-hearts (read-bitmap (get-pure-port
                                (string->url "file:/Images/2-hearts.png"))))
(define 3-hearts (read-bitmap (get-pure-port
                                (string->url "file:/Images/3-hearts.png"))))
(define 4-hearts (read-bitmap (get-pure-port
                                (string->url "file:/Images/4-hearts.png"))))
(define 5-hearts (read-bitmap (get-pure-port
                                (string->url "file:/Images/5-hearts.png"))))
(define 6-hearts (read-bitmap (get-pure-port
                                (string->url "file:/Images/6-hearts.png"))))
(define 7-hearts (read-bitmap (get-pure-port
                                (string->url "file:/Images/7-hearts.png"))))
(define 8-hearts (read-bitmap (get-pure-port
                                (string->url "file:/Images/8-hearts.png"))))
(define 9-hearts (read-bitmap (get-pure-port
                                (string->url "file:/Images/9-hearts.png"))))
(define 10-hearts (read-bitmap (get-pure-port
                                (string->url "file:/Images/10-hearts.png"))))
(define j-hearts (read-bitmap (get-pure-port
                                (string->url "file:/Images/J-hearts.png"))))
(define q-hearts (read-bitmap (get-pure-port
                                (string->url "file:/Images/Q-hearts.png"))))
(define k-hearts (read-bitmap (get-pure-port
                                (string->url "file:/Images/K-hearts.png"))))

; HIDDEN CARD
(define hidden-card (read-bitmap (get-pure-port
                                (string->url "file:/Images/hidden-card.png"))))

; #############
; MAIN FUNCTION
; #############

; Function name: BCEJ.
; Description: this function starts the game with the names
; of the players.
; Restriction: There must be 1, 2 or 3 players in the game.
; Input: a list with the names of the players.
; Output: void.
(define (bCEj players-names)  
  (bCEj-aux players-names my-window))

; Auxiliar function of BCEJ.
(define (bCEj-aux players-names frame)
  (cond ((null? players-names)
         "Error: no players added")
        ((> (length players-names) 3)
         "Error: Can't begin the game with more than 3 players")
        ( else
          ; create the list of the players.
          (set! players-list (create-players-list players-names))

          ; set the names variables.
          (cond ((equal? (length players-names) 1)
                 (set! player1-name-variable (car players-names)))

                ((equal? (length players-names) 2)
                 (set! player1-name-variable (car players-names))
                 (set! player2-name-variable (cadr players-names)))

                (else
                 (set! player1-name-variable (car players-names))
                 (set! player2-name-variable (cadr players-names))
                 (set! player3-name-variable (caddr players-names))))     
  
          (add-names (length players-names))
          (send frame show #t))))
      