; ********************************************************************* 
; Function Name: gameRound
; Purpose: carry out a round
; Parameters: 
;        board - board state passed from file or initialized 
;        human - list of human captures and tournament score 
;        comp - list of computer captures and tournament scofindPatternre 
;        nextplayer - list of next player and their stone color 
; Return Value: none
; Algorithm:
;        1. If board is in an initial state, decide who plays first
;           based on score. If scores are equal, call coin toss.
;        2. Start/resume play of the game.
; Assistance Received: none 
; ***********************************************************************
(defun gameRound (board human comp nextplayer)
        ;; Game in initial state, determine who plays first
  (cond ((equal board (boardInit 19))
            (let ((humancap (first human))
                  (humanscore (first (rest human)))
                  (compcap (first comp))
                  (compscore (first(rest comp))))

                    (cond ((= humanscore compscore)
                          (princ "Coin toss to determine who plays first.")
                          (terpri)
                          (play board '(0 0) '(0 0) (coinToss)))

                          ( (> humanscore compscore)
                          (princ "Human has higher score and goes first!")
                          (terpri)
                          (play board '(0 0) '(0 0) '("Human" "White")))

                          ( (< humanscore compscore)
                          (princ "Computer has higher score and goes first!")
                          (terpri)
                          (play board '(0 0) '(0 0) '("Computer" "White")))
                    )
            )
          )
          ;; Game not in an initial state: keep captures from file, set round score to 0
          ( t 
          (play board (list (first human) 0) (list (first comp) 0) nextplayer)) 
  )
)

; ********************************************************************* 
; Function Name: coinToss
; Purpose: simulate coin toss by generating a random state and assigning
;          it heads or tails, compare to user guess
; Parameters: none
; Return Value: list of next player and their stone color
; Algorithm: none
; Assistance Received: none 
; ***********************************************************************
(defun coinToss()
  ;; Simulate coin toss by generating a random state and assigning it heads or tails
  (let ((cointossresult (elt '("Heads" "Tails") (random 2 (make-random-state t)))))

        ;; Get user call
        (format t "Choose heads or tails: ")
        (terpri)
        (let ((user-call (read-line)))
          (cond
            ;; Check validity of user input
            ( (and (not (string-equal (string-upcase user-call) "HEADS")) (not (string-equal (string-upcase user-call) "TAILS")))
              (princ "Invalid input.")
              (terpri)
              (coinToss)
            )
            ( t 
              (terpri)
              (format t "Coin landed on: ~a~%" cointossresult)

                      ;; Compare user call to coin toss result
              ( cond  ( (string-equal (string-upcase user-call) (string-upcase cointossresult))
                        (format t "Human plays first!")
                        (list "Human" "White")
                      )
                      ( t
                        (format t "Computer plays first!")
                        (list "Computer" "White")
                      )
              )
            )
          )
        )
  )
)