;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |tic tac toe starter|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)
(require racket/list) ; you might want this

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; the SA will mess with the SIZE constant, be sure it is used
;; throughout your program where needed
(define SIZE 500) ;; can range from [300,900]
(define MTS (empty-scene SIZE SIZE))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define BOARD_SIZE (* SIZE .9))
(define OUTLINE (/ (- SIZE BOARD_SIZE) 2))
(define LINE_THICKNESS (floor (* SIZE (/ 1 40))))
(define TEXT_SIZE (floor (* SIZE (/ 1 5))))
(define X_IMAGE (text "X" TEXT_SIZE "indigo"))
(define O_IMAGE (text "O" TEXT_SIZE "magenta"))
(define END_TEXT_SIZE (floor (* SIZE (/ 1 4))))
;
;; image -> WS
;; draws the game board
;; !!! check-expects
;; !!! Change to a constant with size BOARD_SIZE
(define (draw-board image)

  (add-line
   (add-line
    (add-line
     (add-line
      image
      0 (* (/ 1 3) BOARD_SIZE)
      BOARD_SIZE (* (/ 1 3) BOARD_SIZE)
      (make-pen "black" LINE_THICKNESS "solid" "butt" "round"))
     0 (* (/ 2 3) BOARD_SIZE)
     BOARD_SIZE (* (/ 2 3) BOARD_SIZE)
     (make-pen "black" LINE_THICKNESS "solid" "butt" "round"))
    (* (/ 2 3) BOARD_SIZE) 0
    (* (/ 2 3) BOARD_SIZE) BOARD_SIZE
    (make-pen "black" LINE_THICKNESS "solid" "butt" "round"))
   (* (/ 1 3) BOARD_SIZE) 0
   (* (/ 1 3) BOARD_SIZE) BOARD_SIZE
   (make-pen "black" LINE_THICKNESS "solid" "butt" "round")))
(define BOARD (draw-board (rectangle BOARD_SIZE BOARD_SIZE "solid" "white")))


;; vars for readability of false, X and O
(define X "X")
(define O "O")
(define F false)


;; TODO: change name
;; game-state is a natural
;; game-state is either one of
;; 0 - player X's turn
;; 1 - player O's turn
;; 2 - player X won
;; 3 - player O won
;; 4 - draw
(define PX_TURN 0)
(define PO_TURN 1)
(define PX_WIN 2)
(define PO_WIN 3)
(define DRAW 4)

;; Board is a (listOfChar|false)
;; Char is either "X" or "O"
;; false means char is not curerently occupying a spot on the board and the boolean false is
;; intepretation
;; - Board is a tic-tac-toe board that is a list with a length of 9, meaning
;;   9 spots for Char to be placed. Board starts with all positions filled with false
(define BD0 (list F F F 
                  F F F
                  F F F))
(define BD1 (list X O F  ;; X wins
                  F X F
                  O F X)) 
(define BD2 (list F X O  ;; O wins
                  X F O
                  F F O))
(define BD3 (list O X O  ;; draw
                  X O X
                  X O X))
(define BD4 (list X X F  ;; one left
                  F O F
                  F F F))
(define BD5 (list X F F  ;; one left
                  X F F
                  F F F))
(define BD6 (list O O F  ;; one left
                  F F F
                  F F F))
(define BD7 (list O F F  ;; one left
                  F F F
                  F F O))
(define BD8 (list X F F  ;; one left
                  F F F
                  F F F))
(define BD9 (list X F F  ;; one left
                  F F F
                  O F X))

;; Pos is a natural [0, 8]
;; - Pos is the position of Char or false on the board
;; the position of Char|false on the board is given by
;;     - the row is calculated using (quotient Pos 3)
;;     - the column is calculated using (modulo Pos 3)


;; units is a list of positions of length 3
;; units contains all of the winning conditions

;; positions of all the rows, coulumns, and boxes

(define ROWS
  (list (list 0 1 2)
        (list 3 4 5)
        (list 6 7 8)))

(define COLUMNS
  (list (list 0 3 6)
        (list 1 4 7)
        (list 2 5 8)))

(define DIAGONALS
  (list (list 0 4 8)
        (list 2 4 6)))

(define UNITS (append ROWS COLUMNS DIAGONALS))


;; ws is a(make-ws Board Natural Natural)
;; interp: (make-ws bd gs difficulty) with
;; bd the current game board - a list?
;; game-state the current state of the game i.e. player 1 has won, turn etc.
;; difficulty how advanced the computer's intelligence is
(define-struct ws (bd gs difficulty))
(define START (make-ws BD0 PX_TURN 0))

(define (main ws)
  (big-bang ws
    (on-tick tick)
    (on-mouse handle-mouse)
    (on-key change-difficulty)
    (to-draw render)
    ;(stop-when stopper)
    ))

;; WS -> Image
;; renders the game
(define (render ws)
  (cond
    [(= PX_WIN (ws-gs ws))
     (overlay
      (text "X Wins!" END_TEXT_SIZE "black")
      (draw-moves BOARD (ws-bd ws))
      MTS)]
    [(= PO_WIN (ws-gs ws))

     (overlay
      (text "O Wins!" END_TEXT_SIZE "black")
      (draw-moves BOARD (ws-bd ws))
      MTS)]
    [(= DRAW (ws-gs ws))
     (overlay
      (text "Draw :(" END_TEXT_SIZE "black")
      (draw-moves BOARD (ws-bd ws))
      MTS)]
    [else
     (overlay
      (draw-moves BOARD (ws-bd ws))
      MTS)]
    ; (draw-gs ws)
    ))

;; WS -> WS
;; updates the game

(define (tick ws)
  (cond [(winner-x (ws-bd ws)) (make-ws (ws-bd ws) PX_WIN (ws-difficulty ws))]
        [(winner-o (ws-bd ws)) (make-ws (ws-bd ws) PO_WIN (ws-difficulty ws))]
        [(draw? (ws-bd ws)) (make-ws (ws-bd ws) DRAW (ws-difficulty ws))]
        [else
         (if (= (ws-gs ws) PO_TURN)
             (make-ws (Minimax (ws-bd ws) (ws-difficulty ws)) PX_TURN (ws-difficulty ws))
             ;    (update-list ws (+ OUTLINE (random BOARD_SIZE)) (+ OUTLINE (random BOARD_SIZE)))
             ws
      
             )]))

;(define (update-list ws x y)


;  (define (Minimax board)
;    (argmax min-value (get-next-moves PO_TURN board))
;    )
;
;  (define (max-value board)
;    (cond [(winner-x board) -1]
;          [(winner-o board) 1]
;          [(draw? board) 0]
;          [else
;           (apply max (map min-value (get-next-moves PO_TURN board)))]))
;
;
;  (define (min-value board)
;    (cond [(winner-x board) -1]
;          [(winner-o board) 1]
;          [(draw? board) 0]
;          [else
;           (apply min (map max-value (get-next-moves PX_TURN board)))]))

;; Board Natural -> Board
;; determines the next best move for the AI

(check-expect (member? (Minimax BD0 0)
                       (get-next-moves PO_TURN BD0)) true) ;; checks for a random move

(check-expect (member? (Minimax BD9 9) (get-next-moves PO_TURN BD9)) true) ;; checks next move is
                                                                           ;; valid

(check-expect (Minimax BD4 9) (list X X O  ;; checks for a block
                                    F O F
                                    F F F))

(check-expect (Minimax BD8 9) (list X F F  ;; checks for first center move
                                    F O F
                                    F F F))


(define (Minimax board0 difficulty)
  (local [
          ;; Board -> Natural
          ;; determines the number of spots empty on the board
          (define (num-squares-available board) (length (filter false? board)))
          (define cutoff (- (num-squares-available board0) difficulty))
          ;; board -> num
          ;; determines the max utility of a board
          (define (max-value board)
            (cond [(winner-x board) (+ -1 (* (random 10) .01))]
                  [(winner-o board) (+ 1 (* (random 10) .01))]
                  [(draw? board) (* (random 10) .01)]
                  [(<= (num-squares-available board) cutoff) (* (random 10) .01)]
                  [else
                   (apply max (map min-value (get-next-moves PO_TURN board)))]))
          ;; board -> num
          ;; determines the min utility of a board
          (define (min-value board)
            (cond [(winner-x board) (+ -1 (* (random 10) .01))]
                  [(winner-o board) (+ 1 (* (random 10) .01))]
                  [(draw? board) (* (random 10) .01)]
                  [(<= (num-squares-available board) cutoff) (* (random 10) .01)]
                  [else
                   (apply min (map max-value (get-next-moves PX_TURN board)))]))]
    (argmax min-value (get-next-moves PO_TURN board0)))) ;; trampoline
  
         
  







;(check-expect (get-next-moves PO_TURN BD0) 

(define (get-next-moves gs board)
  (map (lambda (pos) (list-set board pos (if (= gs PO_TURN) O X))) 
       (filter
        (lambda (pos)
          (available? board pos))
        (build-list 9 identity)))) ;;TODO!!! change so values is map function

;; board -> Boolean
;; determines if the given board is a draw
(check-expect (draw? BD0) false)
(check-expect (draw? BD1) false)
(check-expect (draw? BD3) true)

(define (draw? board)
  (and (not (winner-x board)) (not (winner-o board)) (andmap (lambda (pos)
                                                               (not (available? board pos)))
                                                             (build-list 9 identity))))
       
;; (X -> Boolean) -> (ListOfX -> Boolean)
;; determines if a given board has 3 elements in a row/column/diagonal that match

(define (winner board-val-matches-player?)
  (lambda (bd) (
                local [ ;; ListOfX -> Boolean
                        ;; determine if a row/column/diagonal contains all the same value
                       (define (unit-all-val? u)
                         (andmap (lambda (pos) (board-val-matches-player? (list-ref bd pos))) u))
                       ;; ListOfX -> Boolean
                       ;; determine if any one row/column/diagonal has all the same value
                       (define (any-unit-all-val? bd)
                         (ormap unit-all-val? UNITS))
          
                       ]
                 (any-unit-all-val? bd))))

;; Board -> Boolean
;; determines if x is the winner using higher order function winner

(check-expect (winner-x BD0) false)
(check-expect (winner-x BD1) true)
(check-expect (winner-x BD2) false)

(define (winner-x bd)
  ((winner (lambda (val) (equal? X val))) bd)
  )

;; Board -> Boolean
;; determines if o is the winner using higher order function winner

(check-expect (winner-o BD0) false)
(check-expect (winner-o BD1) false)
(check-expect (winner-o BD2) true)

(define (winner-o bd)
  ((winner (lambda (val) (equal? O val))) bd)
  )

                         

;; TODO: change to use map function
; Image Board -> Image
; Draws where the Xs and Os are on the board

(define (draw-moves image bd0)
  ;; pos: Natural; the position in the original list !!! check this
  (local 
    [
     (define (draw-moves-inner image bd pos)
       (cond [(empty? bd) image]
             [else 
              (draw-moves-inner (draw-move image bd0 pos)
                                (rest bd)
                                (add1 pos))]))]
    (draw-moves-inner image bd0 0))
  )


;; Image Board Pos-> Image
;; draws the move in pos p on the board
(define (draw-move image bd pos)
  (local [(define row (quotient pos 3)) 
          (define col (modulo pos 3))
          ;          (define row0 (* (/ 1 6) BOARD_SIZE))
          ;          (define row1 (* (/ 1 2) BOARD_SIZE))
          ;          (define row2 (* (/ 5 6) BOARD_SIZE))
          ;          (define col0 (* (/ 1 6) BOARD_SIZE))
          ;          (define col1 (* (/ 1 2) BOARD_SIZE))
          ;          (define col2 (* (/ 5 6) BOARD_SIZE))
          (define row-x-cord (* (+ (/ 1 6) (* row (/ 1 3))) BOARD_SIZE))
          (define col-y-cord (* (+ (/ 1 6) (* col (/ 1 3))) BOARD_SIZE))
          ]
    (cond
      [(equal? "X" (list-ref bd pos))
       (place-image X_IMAGE col-y-cord row-x-cord image)]
      [(equal? "O" (list-ref bd pos))
       (place-image O_IMAGE col-y-cord row-x-cord image)]
      [else image]
      )
    
    )
  )



;; Natural Natural -> Boolean
;; determines if the x and y of the mouse are on the board
(check-expect (on-board? (/ SIZE 2) (/ SIZE 2)) true)
(check-expect (on-board? (- OUTLINE 1) (/ SIZE 2)) false)
(check-expect (on-board? OUTLINE OUTLINE) false)
(check-expect (on-board? (+ OUTLINE SIZE) OUTLINE) false)
(check-expect (on-board? OUTLINE (+ OUTLINE SIZE)) false)
(check-expect (on-board? (+ OUTLINE SIZE) (+ OUTLINE SIZE)) false)


(define (on-board? x y)
  (and (> x OUTLINE) (< x (- SIZE OUTLINE))
       (> y OUTLINE) (< y (- SIZE OUTLINE))))


;; Natural Natural -> ListOfNatural
;; returns the row and column of the x and y coordinates
(check-expect (where-on-board? (+ OUTLINE (/ BOARD_SIZE 6)) (+ OUTLINE (/ BOARD_SIZE 6)))
              (list 0 0))
(check-expect (where-on-board? (+ OUTLINE (/ BOARD_SIZE 6)) (+ OUTLINE (/ BOARD_SIZE 2)))
              (list 1 0))
(check-expect (where-on-board? OUTLINE OUTLINE) (list 0 0))
(check-expect (where-on-board? OUTLINE (+ (* BOARD_SIZE (/ 1 3)) OUTLINE)) (list 1 0))
(check-expect (where-on-board? OUTLINE (+ (* BOARD_SIZE (/ 2 3)) OUTLINE)) (list 2 0))
(check-expect (where-on-board? (+ (* BOARD_SIZE (/ 1 3)) OUTLINE) OUTLINE) (list 0 1))
(check-expect (where-on-board? (+ (* BOARD_SIZE (/ 1 3)) OUTLINE) (+ (* BOARD_SIZE (/ 1 3))
                                                                     OUTLINE)) (list 1 1))
(check-expect (where-on-board? (+ (* BOARD_SIZE (/ 1 3)) OUTLINE) (+ (* BOARD_SIZE (/ 2 3))
                                                                     OUTLINE)) (list 2 1))
(check-expect (where-on-board? (+ (* BOARD_SIZE (/ 2 3)) OUTLINE) OUTLINE) (list 0 2))
(check-expect (where-on-board? (+ (* BOARD_SIZE (/ 2 3)) OUTLINE) (+ (* BOARD_SIZE (/ 1 3))
                                                                     OUTLINE)) (list 1 2))
(check-expect (where-on-board? (+ (* BOARD_SIZE (/ 2 3)) OUTLINE) (+ (* BOARD_SIZE (/ 2 3))
                                                                     OUTLINE)) (list 2 2))


(define (where-on-board? x y)
  (local [ (define (find-position p)
             (floor (/ p (/ BOARD_SIZE 3))))]
    (cons (find-position (- y OUTLINE)) (cons (find-position (- x OUTLINE)) empty)))
  )
  
         
;; WS Natural Natural String -> WS
;; updates the world state based on the mouse

(check-expect (handle-mouse START (+ OUTLINE (/ BOARD_SIZE 2)) (+ OUTLINE (/ SIZE 2)) "button-down")
              (update-list START (+ OUTLINE (/ BOARD_SIZE 2)) (+ OUTLINE (/ BOARD_SIZE 2))))
(check-expect (handle-mouse START (+ OUTLINE (/ BOARD_SIZE 4)) BOARD_SIZE "button-down")
              (update-list START (+ OUTLINE (/ BOARD_SIZE 4)) BOARD_SIZE))
(check-expect (handle-mouse START (+ OUTLINE (/ BOARD_SIZE 2)) (+ OUTLINE (/ BOARD_SIZE 2)) "button-up")
              START)


(define (handle-mouse ws x y mouse-event)
  (if (and (mouse=? mouse-event "button-down") (on-board? x y) (= (ws-gs ws) PX_TURN))
      (update-list ws x y)
      ws))



;; WS Natural Natural -> WS
;; updates the list in the given world state and changes the turn

(define (update-list ws x y)
  (if 
   (available? (ws-bd ws) (find-index
                           (list-ref (where-on-board? x y) 0)
                           (list-ref (where-on-board? x y) 1)))
   ;   (false?                  ;; is the spot available?
   ;    (list-ref (ws-bd ws)   
   ;              (find-index
   ;               (list-ref (where-on-board? x y) 0)
   ;               (list-ref (where-on-board? x y) 1))))
   (make-ws (list-set (ws-bd ws)  
                      (find-index
                       (list-ref (where-on-board? x y) 0)
                       (list-ref (where-on-board? x y) 1)) (cond [(= PX_TURN (ws-gs ws)) X]
                                                                 [(= PO_TURN (ws-gs ws)) O]
                                                                 [else
                                                                  "this code is broken??"]))
            (cond [(= PX_TURN (ws-gs ws)) PO_TURN]  ;; switches the player's turn
                  [(= PO_TURN (ws-gs ws)) PX_TURN]
                  [else
                   "this code is broken??"])
            (ws-difficulty ws))
   ws))

;; board Natural? -> boolean
;; determines if a spot in the board is available

(check-expect (available? BD0 0) true)
(check-expect (available? BD1 2) true)
(check-expect (available? BD2 1) false)
(check-expect (available? BD3 5) false) ;; all positions filled
(check-expect (available? BD4 8) true)
(check-expect (available? BD5 6) true)

(define (available? bd pos)
  (false?                  ;; is the spot available?
   (list-ref bd  
             pos)))                                 

                                                                                 
; natural natural -> natural
; find the index of the specified row and column in the list                                                                                   

(check-expect (find-index 0 0) 0)
(check-expect (find-index 0 1) 1)
(check-expect (find-index 0 2) 2)
(check-expect (find-index 1 0) 3)
(check-expect (find-index 1 1) 4)
(check-expect (find-index 1 2) 5)
(check-expect (find-index 2 0) 6)
(check-expect (find-index 2 1) 7)
(check-expect (find-index 2 2) 8)

(define (find-index row col)
  (+ (* row 3) col))


;; WS string -> WS
;; Changes the difficulty of the game
(check-expect (change-difficulty START "0") (make-ws BD0 PX_TURN 0))
(check-expect (change-difficulty START "1") (make-ws BD0 PX_TURN 1))
(check-expect (change-difficulty START "2") (make-ws BD0 PX_TURN 2))
(check-expect (change-difficulty START "3") (make-ws BD0 PX_TURN 3))
(check-expect (change-difficulty START "4") (make-ws BD0 PX_TURN 4))
(check-expect (change-difficulty START "5") (make-ws BD0 PX_TURN 5))
(check-expect (change-difficulty START "6") (make-ws BD0 PX_TURN 6))
(check-expect (change-difficulty START "7") (make-ws BD0 PX_TURN 7))
(check-expect (change-difficulty START "8") (make-ws BD0 PX_TURN 8))
(check-expect (change-difficulty START "9") (make-ws BD0 PX_TURN 9))
(check-expect (change-difficulty START "a") (make-ws BD0 PX_TURN (ws-difficulty START)))

(define (change-difficulty ws key-event)
  (cond [(key=? key-event "0") (make-ws (ws-bd ws) (ws-gs ws) 0)]
        [(key=? key-event "1") (make-ws (ws-bd ws) (ws-gs ws) 1)]
        [(key=? key-event "2") (make-ws (ws-bd ws) (ws-gs ws) 2)]
        [(key=? key-event "3") (make-ws (ws-bd ws) (ws-gs ws) 3)]
        [(key=? key-event "4") (make-ws (ws-bd ws) (ws-gs ws) 4)]
        [(key=? key-event "5") (make-ws (ws-bd ws) (ws-gs ws) 5)]
        [(key=? key-event "6") (make-ws (ws-bd ws) (ws-gs ws) 6)]
        [(key=? key-event "7") (make-ws (ws-bd ws) (ws-gs ws) 7)]
        [(key=? key-event "8") (make-ws (ws-bd ws) (ws-gs ws) 8)]
        [(key=? key-event "9") (make-ws (ws-bd ws) (ws-gs ws) 9)]
        [else
         ws]))







