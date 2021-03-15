#lang racket

; #######
; IMPORTS
; #######

; Required libraries.
(require racket/gui)
(require racket/draw
           net/url)

; Main Window
(define my-window (new frame%
                       [label "BlaCE Jack"]
                       [width 1000]
                       [height 700]
                       [style '(no-resize-border)]
                       [alignment '(left top)]))

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
                          [style '(border)]))

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
                          [style '(border)]))

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
                          [style '(border)]))

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
                          [label "Turn of No.1"]
                          [font (make-object font% 25 'default 'normal 'normal)])) 

; Next Player Button
(define next-button (new button%
                         [parent ver-pane-1.1]
                         [label "Next"]
                         [min-width 200]
                         [min-height 30]
                         [font (make-object font% 15 'default 'normal 'normal)]))

; Take Card Button
(define take-button (new button%
                         [parent ver-pane-1.1]
                         [label "Take Card"]
                         [min-width 200]
                         [min-height 30]
                         [font (make-object font% 15 'default 'normal 'normal)]))

; Stay With Current Cards Button
(define stay-button (new button%
                         [parent ver-pane-1.1]
                         [label "Stay"]
                         [min-width 200]
                         [min-height 30]
                         [font (make-object font% 15 'default 'normal 'normal)]))

; Crupier name
(define crupier-name (new message%
                          [parent ver-pane-1.2]
                          [label "Crupier"]
                          [font (make-object font% 15 'default 'normal 'normal)]))

; Crupier total
(define crupier-total (new message%
                          [parent ver-pane-1.2]
                          [label "Total : ___"]
                          [font (make-object font% 15 'default 'normal 'normal)]))

; Palyer 1 name
(define player1-name (new message%
                          [parent ver-pane-2.1]
                          [label "Player 1"]
                          [font (make-object font% 15 'default 'normal 'normal)]))

; Player 1 total
(define player1-total (new message%
                          [parent ver-pane-2.1]
                          [label "Total : ___"]
                          [font (make-object font% 15 'default 'normal 'normal)]))

; Palyer 2 name
(define player2-name (new message%
                          [parent ver-pane-2.2]
                          [label "Player 2"]
                          [font (make-object font% 15 'default 'normal 'normal)]))

; Player 2 total
(define player2-total (new message%
                          [parent ver-pane-2.2]
                          [label "Total : ___"]
                          [font (make-object font% 15 'default 'normal 'normal)]))
; Palyer 3 name
(define player3-name (new message%
                          [parent ver-pane-2.3]
                          [label "Player 3"]
                          [font (make-object font% 15 'default 'normal 'normal)]))

; Player 3 total
(define player3-total (new message%
                          [parent ver-pane-2.3]
                          [label "Total : ___"]
                          [font (make-object font% 15 'default 'normal 'normal)]))

; Deck image
(define deck (read-bitmap (get-pure-port
                                (string->url "file:/Images/deck.png"))))

(void (new message%
           [parent ver-pane-1.3]
           [label deck]))

; Cards in deck
(define cards-left (new message%
                          [parent ver-pane-1.3]
                          [label "__ cards left"]
                          [font (make-object font% 25 'default 'normal 'normal)]))


; Add Cards to a Player
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

(define (add-card-aux pane card)
  (void (new message%
             [parent pane]
             [label (card-image card)])))

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
        ))


; Make window visible
(send my-window show #t)


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





