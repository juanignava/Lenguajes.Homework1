#lang racket

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
(define ver-pane-1.2 (new vertical-pane%
                          [parent hor-pane-1]
                          [vert-margin 10]
                          [horiz-margin 10]
                          [alignment '(center center)]))

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
(define ver-pane-2.1 (new vertical-pane%
                          [parent hor-pane-2]
                          [vert-margin 10]
                          [horiz-margin 10]
                          [alignment '(center center)]))

; Vertical Pane 2.2
(define ver-pane-2.2 (new vertical-pane%
                          [parent hor-pane-2]
                          [vert-margin 10]
                          [horiz-margin 10]
                          [alignment '(center center)]))

; Vertical Pane 2.3
(define ver-pane-2.3 (new vertical-pane%
                          [parent hor-pane-2]
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


; Add Cards to a Player
(define (add-card player-number card-txt)
  (cond ( (equal? player-number 0)
          (add-card-aux hor-pane-1.2.1 card-txt))))

(define (add-card-aux pane card)
  (void (new message%
             [parent pane]
             [label (card-image card)])))

(define (card-image card-txt)
  (cond ( (equal? card-txt "a-c")
          a-clubs)))


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
(define J-leaves (read-bitmap (get-pure-port
                                (string->url "file:/Images/J-leaves.png"))))
(define Q-leaves (read-bitmap (get-pure-port
                                (string->url "file:/Images/Q-leaves.png"))))
(define K-leaves (read-bitmap (get-pure-port
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
(define J-diamonds (read-bitmap (get-pure-port
                                (string->url "file:/Images/J-diamonds.png"))))
(define Q-diamonds (read-bitmap (get-pure-port
                                (string->url "file:/Images/Q-diamonds.png"))))
(define K-diamonds (read-bitmap (get-pure-port
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
(define J-clubs (read-bitmap (get-pure-port
                                (string->url "file:/Images/J-clubs.png"))))
(define Q-clubs (read-bitmap (get-pure-port
                                (string->url "file:/Images/Q-clubs.png"))))
(define K-clubs (read-bitmap (get-pure-port
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
(define J-hearts (read-bitmap (get-pure-port
                                (string->url "file:/Images/J-hearts.png"))))
(define Q-hearts (read-bitmap (get-pure-port
                                (string->url "file:/Images/Q-hearts.png"))))
(define K-hearts (read-bitmap (get-pure-port
                                (string->url "file:/Images/K-hearts.png"))))

; DECK

(define deck (read-bitmap (get-pure-port
                                (string->url "file:/Images/deck.png"))))


