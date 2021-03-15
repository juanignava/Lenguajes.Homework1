#lang racket/gui

; Constant name: Deck.
; Description: list of all the different cards.
; c => Clubs
; l => Leaves
; d => Diamonds
; h => Hearts
; Symbol-Value
(define deck
  '(a-c, 2-c, 3-c, 4-c , 5-c, 6-c, 7-c, 8-c, 9-c, 10-c, j-c, q-c, k-c,
    a-l, 2-l, 3-l, 4-l , 5-l, 6-l, 7-l, 8-l, 9-l, 10-l, j-l, q-l, k-l,
    a-d, 2-d, 3-d, 4-d , 5-d, 6-d, 7-d, 8-d, 9-d, 10-d, j-d, q-d, k-d,
    a-h, 2-h, 3-h, 4-h , 5-h, 6-h, 7-h, 8-h, 9-h, 10-h, j-h, q-h, k-h))


; Function name: Length.
; Description: this function is in charge of count how many elements a
;              given list has.
; Input: a list.
; Output: an integer.
(provide length)
(define (length list)

  (cond ((null? list)
         0)

        (else
         (+ 1 (length (cdr list))))))


; Function name: Get-Player1.
; Description: this function returns the first element of the given list.
; Input: a list.
; Output: a list.
(define (get-player1 players-list)
  (car players-list))


; Function name: Get-Player2.
; Description: this function returns the second element of the given list.
; Input: a list.
; Output: a list.
(define (get-player2 players-list)
  (cadr players-list))


; Function name: Get-Player3
; Description: this function returns the third element of the given list.
; Input: a list.
; Output: a list.
(define (get-player3 players-list)
  (caddr players-list))


; Function name: Get-Player-Number.
; Description: this function returns the first element of the given list.
; Input: a list.
; Output: an integer.
(define (get-player-number player)
  (car player))


; Function name: Get-Player-Cards.
; Description: this function returns the second element of the given list.
; Input: a list.
; Output: a list.
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
(define (get-player-name player)
  (cadddr player))







(define (add-card-to-player players-list player card)

  (cond ((equal? (get-player1 players-list) player)
         (append (get-player-cards player) card)))

  players-list)

        













(provide create-players-list)
(define (create-players-list players-list)

  (cond ((null? players-list)
         "Error: no hay suficientes jugadores")

        ((> (length players-list) 3)
         "Error: solo se permiten 3 jugadores")        

        ((equal? (length players-list) 1)
         (list 1 '() 0 (get-player1 players-list)))

        ((equal? (length players-list) 2)
         (list (list 1 '() 0 (get-player1 players-list))
               (list 2 '() 0 (get-player2 players-list))))

        (else
         (list (list 1 '() 0 (get-player1 players-list))
               (list 2 '() 0 (get-player2 players-list))
               (list 3 '() 0 (get-player3 players-list))))))
  
  









  



















  






  


        

  






  


  

  