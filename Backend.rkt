#lang racket

; #########
; CONSTANTS
; #########

; Constant name: Deck.
; Description: list of all the different cards.
; c => Clubs
; l => Leaves
; d => Diamonds
; h => Hearts
; Value-Symbol
(provide original-deck)
(define original-deck
  '("a-c" "2-c" "3-c" "4-c" "5-c" "6-c" "7-c" "8-c" "9-c" "10-c" "j-c" "q-c" "k-c"
    "a-l" "2-l" "3-l" "4-l" "5-l" "6-l" "7-l" "8-l" "9-l" "10-l" "j-l" "q-l" "k-l"
    "a-d" "2-d" "3-d" "4-d" "5-d" "6-d" "7-d" "8-d" "9-d" "10-d" "j-d" "q-d" "k-d"
    "a-h" "2-h" "3-h" "4-h" "5-h" "6-h" "7-h" "8-h" "9-h" "10-h" "j-h" "q-h" "k-h"))

; Constant name: A-Cards.
; Description: list of all A cards in the deck.
(define a-cards
  '("a-c" "a-l" "a-d" "a-h"))

; Constant name:2-Cards.
; Description: list of all 2 cards in the deck.
(define 2-cards
  '("2-c" "2-l" "2-d" "2-h"))

; Constant name: 3-Cards.
; Description: list of all 3 cards in the deck.
(define 3-cards
  '("3-c" "3-l" "3-d" "3-h"))

; Constant name: 4-Cards.
; Description: list of all 4 cards in the deck.
(define 4-cards
  '("4-c" "4-l" "4-d" "4-h"))

; Constant name: 5-Cards.
; Description: list of all 5 cards in the deck.
(define 5-cards
  '("5-c" "5-l" "5-d" "5-h"))

; Constant name: 6-Cards.
; Description: list of all 6 cards in the deck.
(define 6-cards
  '("6-c" "6-l" "6-d" "6-h"))

; Constant name: 7-Cards.
; Description: list of all 7 cards in the deck.
(define 7-cards
  '("7-c" "7-l" "7-d" "7-h"))

; Constant name: 8-Cards.
; Description: list of all 8 cards in the deck.
(define 8-cards
  '("8-c" "8-l" "8-d" "8-h"))

; Constant name: 9-Cards.
; Description: list of all A cards in the deck.
(define 9-cards
  '("9-c" "9-l" "9-d" "9-h"))

; Constant name: 10-Cards.
; Description: list of all 10 cards in the deck.
(define 10-cards
  '("10-c" "10-l" "10-d" "10-h"))

; Constant name: J-Cards.
; Description: list of all J cards in the deck.
(define j-cards
  '("j-c" "j-l" "j-d" "j-h"))

; Constant name: Q-Cards.
; Description: list of all Q cards in the deck.
(define q-cards
  '("q-c" "q-l" "q-d" "q-h"))

; Constant name: K-Cards.
; Description: list of all K cards in the deck.
(define k-cards
  '("k-c" "k-l" "k-d" "k-h"))

; #########
; FUNCTIONS
; #########

; Function name: Length.
; Description: this function is in charge of count how many elements a given list has.
; Input: a list.
; Output: an integer.
(provide length)
(define (length list)

  (cond ((null? list)
         0)

        (else
         (+ 1 (length (cdr list))))))

; Function name: Get-Crupier.
; Description: this function returns the first element of the given list.
; Input: a list.
; Output: a list.
(provide get-crupier)
(define (get-crupier players-list)
  (car players-list))

; Function name: Get-Player1.
; Description: this function returns the second element of the given list.
; Input: a list.
; Output: a list.
(provide get-player1)
(define (get-player1 players-list)
  (cadr players-list))

; Function name: Get-Player2.
; Description: this function returns the third element of the given list.
; Input: a list.
; Output: a list.
(provide get-player2)
(define (get-player2 players-list)
  (caddr players-list))

; Function name: Get-Player3
; Description: this function returns the fourth element of the given list.
; Input: a list.
; Output: a list.
(provide get-player3)
(define (get-player3 players-list)
  (cadddr players-list))

; Function name: Get-Player-Number.
; Description: this function returns the first element of the given list.
; Input: a list.
; Output: an integer.
(provide get-player-number)
(define (get-player-number player)
  (car player))

; Function name: Get-Player-Cards.
; Description: this function returns the second element of the given list.
; Input: a list.
; Output: a list.
(provide get-player-cards)
(define (get-player-cards player)
  (cadr player))

; Function name: Get-Player-Stay-Bit.
; Description: this function returns the third element of the given list.
; Input: a list.
; Output: an integer.
(define (get-player-stay-bit player)
  (caddr player))

; Function name: Get-Player-Name.
; Description: this function returns the fourth element of the given list.
; Input: a list.
; Output: a string.
(provide get-player-name)
(define (get-player-name player)
  (cadddr player))

; Function name: Create-Players-List.
; Description: this function returns a list of lists with all required players parameters.
; Input: a list.
; Output: a string or a list.
(provide create-players-list)
(define (create-players-list players-list)
    
  (cond ((equal? (length players-list) 1)
         (list (list 0 '() 0 'Crupier)
               (list 1 '() 0 (get-player1 (append '("-") players-list)))))

        ((equal? (length players-list) 2)
         (list (list 0 '() 0 'Crupier)
               (list 1 '() 0 (get-player1 (append '("-") players-list)))
               (list 2 '() 0 (get-player2 (append '("-") players-list)))))

        (else
         (list (list 0 '() 0 'Crupier)
               (list 1 '() 0 (get-player1 (append '("-") players-list)))
               (list 2 '() 0 (get-player2 (append '("-") players-list)))
               (list 3 '() 0 (get-player3 (append '("-") players-list)))))))

; Function name: Add-Card-To-Player.
; Description: this function adds the given card to the specified player.
; Input: a list, an integer and a string.
; Output: a list.
(provide add-card-to-player)
(define (add-card-to-player players-list player-number card)
  (add-card-to-player-aux players-list player-number (list card)))

(define (add-card-to-player-aux players-list player-number card)

  (cond ((null? players-list)
         '())

        ((equal? player-number (get-player-number (car players-list)))
         (cons (list (get-player-number (car players-list))
                     (append card
                             (get-player-cards (car players-list)))
                     (get-player-stay-bit (car players-list))
                     (get-player-name (car players-list)))
               (add-card-to-player-aux (cdr players-list) player-number card)))

        (else
         (cons (car players-list)
               (add-card-to-player-aux (cdr players-list) player-number card)))))

; Function name: Delete-Card.
; Description: this function deletes the first element of the given list.
; Input: a list.
; Output: a list.
(provide delete-card)
(define (delete-card deck)
  (cdr deck))

; Function name: Member?.
; Description: this function checks if the given card is in the received list.
; Input: a string and a list.
; Output: a boolean.
(define (member? card card-list)
  
  (cond ((null? card-list)
         #f)

        ((equal? card (car card-list))
         #t)

        (else
         (member? card (cdr card-list)))))

; Function name: Set-Stay-To-Player.
; Description: this function change the stay bit of the plsyer to 1.
; Input: a list and an integer.
; Output: a list.
(provide set-stay-to-player)
(define (set-stay-to-player players-list player-number)

  (cond ((null? players-list)
         '())

        ((equal? player-number (get-player-number (car players-list)))
         (cons (list (get-player-number (car players-list))
                     (get-player-cards (car players-list))
                     1
                     (get-player-name (car players-list)))
               (set-stay-to-player (cdr players-list) player-number)))

        (else
         (cons (car players-list)
               (set-stay-to-player (cdr players-list) player-number)))))

; Function name: Get-Card.
; Description: this function returns the last card given to a player.
; Input: a list and an integer.
; Output: a string.
(provide get-card)
(define (get-card players-list player-number)

  (cond ((equal? player-number (get-player-number (car players-list)))
         (car (get-player-cards (car players-list))))

        (else
         (get-card (cdr players-list) player-number))))

; Function name: Get-Card.
; Description: this function checks if the received player is already stay.
; Input: a list and an integer.
; Output: a boolean.
(provide stay?)
(define (stay? players-list player-number)

  (cond ((equal? (get-player-number (car players-list)) player-number)

         (cond ((equal? (get-player-stay-bit (car players-list)) 1)
                #t)

               (else
                #f)))

         (else
          (stay? (cdr players-list) player-number))))

; Function name: Organize-Cards.
; Description: this function is in charge of organizing the received card-list so that the As cards end up at the end of the list. 
; Input: a list.
; Output: a list.
(define (organize-cards card-list)
  (organize-cards-aux card-list '()))

(define (organize-cards-aux card-list organized-list)

  (cond ((null? card-list)
         organized-list)

        ((member? (car card-list) a-cards)
         (organize-cards-aux (cdr card-list)
                             (append organized-list
                                     (list (car card-list)))))

        (else
         (organize-cards-aux (cdr card-list)
                             (append (list (car card-list))
                                     organized-list)))))
                                
; Function name: Check-Score.
; Description: this function returns the score from the received player based on his cards. 
; Input: a list.
; Output: an integer.
(provide check-score)
(define (check-score player)
  (check-score-aux (organize-cards (get-player-cards player)) 0))

(define (check-score-aux card-list score)

  (cond ((null? card-list)
         score)
        
        ((member? (car card-list) a-cards)

         (cond ((> (+ score 11) 21)
                (check-score-aux (cdr card-list) (+ 1 score)))

               (else
                (check-score-aux (cdr card-list) (+ 11 score)))))        

        ((member? (car card-list) 2-cards)
         (check-score-aux (cdr card-list) (+ 2 score)))        

        ((member? (car card-list) 3-cards)
         (check-score-aux (cdr card-list) (+ 3 score)))

        ((member? (car card-list) 4-cards)
         (check-score-aux (cdr card-list) (+ 4 score)))

        ((member? (car card-list) 5-cards)
         (check-score-aux (cdr card-list) (+ 5 score)))

        ((member? (car card-list) 6-cards)
         (check-score-aux (cdr card-list) (+ 6 score)))

        ((member? (car card-list) 7-cards)
         (check-score-aux (cdr card-list) (+ 7 score)))

        ((member? (car card-list) 8-cards)
         (check-score-aux (cdr card-list) (+ 8 score)))

        ((member? (car card-list) 9-cards)
         (check-score-aux (cdr card-list) (+ 9 score)))

        ((or (member? (car card-list) 10-cards)
             (member? (car card-list) j-cards)
             (member? (car card-list) q-cards)
             (member? (car card-list) k-cards))
         (check-score-aux (cdr card-list) (+ 10 score)))))
