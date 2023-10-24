; ********************************************************************* 
; Function Name: tournament
; Purpose: carry out the tournament, which is comprised of rounds; update
;            tournament score at the end of each round
; Parameters: 
;        board - board state passed from file or initialized 
;        human - list of human captures and tournament score 
;        comp - list of computer captures and tournament score 
;        nextplayer - list of next player and their stone color 
;        filename - file to save to if user decides to end the game 
; Return Value: none
; Algorithm:
;        1. Store tournament scores before gameRound changes value to track round score.
;        2. Store current state of round (values returned by round) to quit or continue 
;           the game accordingly.
;        3. If user didn't quit before end of round, round scores displayed. If user quit, save 
;           current state to file.
;        4. Tournament scores displayed and winner announed if user 
;           says no to playing another round. Stats saved to file.
;        5. Tournament called recursively with updated tournament score values while
;           user wants to play another round.
; Assistance Received: none 
; ***********************************************************************
(defun tournament(board human comp nextplayer filename)

  ;; Store tournament scores before round changes them to keep track of round score
  ;; Store game state to pass returned values to steps that follow
  (let* ( (tournamentScoreHuman (second human)) 
          (tournamentScoreComp (second comp))
          (currentstate (gameround board human comp nextplayer))
          (board (first currentstate))
          (human (second currentstate))
          (comp (third currentstate))
          (nextplayer (fourth currentstate))
          (case (fifth currentstate)))

                 ;; Save current state of game to file if user entered return case is "quit"
          (cond ( (string-equal case "quit")
                    (saveToFile board (list (first human) tournamentScoreHuman) (list (first comp) tournamentScoreComp) nextplayer filename)
                    (princ "Game saved to file.")
                    (terpri)
                )
                
                ;; Round ended, round and tournament scores displayed
                ( t
                    (let* ( (tournamentScores (computeScoresAfterRound board nextplayer human comp tournamentScoreHuman tournamentScoreComp))
                            (humanScore (first tournamentScores))
                            (compScore (second tournamentScores)))

                            (princ "Play another round? Enter 'Y' or 'N'")
                            (terpri)

                            (let ((input (anotherroundprompt)))
                              
                                    ;; No more rounds, winner announced
                              (cond  ( (= input 0)
                                        (princ "Tournament scores:")
                                        (terpri)
                                        (printScores humanScore compScore)

                                        (cond ((> humanScore compScore)
                                                (princ "Winner of the tournament: Human player!"))
                                              ((> compScore humanScore)
                                                (princ "Winner of the tournament: Computer player!"))
                                              ( t 
                                              (princ "Winner of the tournament: It's a draw!" ))
                                        )
                                        
                                        (terpri)
                                        (saveToFile (boardInit 19) (list 0 humanScore) (list 0 compScore) nextplayer filename)
                                      )

                                      ;; Another round starts
                                      ( (= input 1)
                                        (tournament (boardInit 19) (list 0 humanScore) (list 0 compScore) '("" "") filename)
                                      )
                              )
                            )
                            (terpri)
                    )
                )
          )
  )
)

; ********************************************************************* 
; Function Name: computeScoresAfterRound
; Purpose: compute round and tournament scores and display them
; Parameters: 
;        board - board after round ends
;        nextplayer - list of next player and their stone color 
;        human - list of human captures and round score 
;        comp - list of computer captures and round score 
;        tournamentScoreHuman - human's tournament score before this round 
;        tournamentScoreComp - computer's tournament score before this round 
; Return Value: updated human and computer tournament scores in a list
; Algorithm:
;        1. Compute round scores by adding captured count, any 5 in a row scores, and counting
;           4 in a rows for each player.
;        2. Compute tournament scores by adding to current tournament scores the round scores.
;        3. Print both, return list of tournament scores to tournament function.
; Assistance Received: none 
; ***********************************************************************
(defun computeScoresAfterRound (board nextplayer human comp tournamentScoreHuman tournamentScoreComp)
  
  ;; Check next player color to add 4 in a row counts to correct players
  (cond ((or (equal nextplayer '("Human" "White")) (equal nextplayer '("Computer" "Black")))
            (let* ((humanRound (+ (first human)(second human)(count4InARow board "W" 0 0)))
                  (compRound (+ (first comp)(second comp)(count4InARow board "B" 0 0)))
                  (humanTournament (+ tournamentScoreHuman humanRound))
                  (compTournament (+ tournamentScoreComp compRound)))

                  (princ "Round scores:")
                  (terpri)
                  (printScores humanRound compRound)
                  (princ "Tournament scores:")
                  (terpri)
                  (printScores humanTournament compTournament)
                  (list humanTournament compTournament)
            )
        )
        (t 
            (let* ((humanRound (+ (first human)(second human)(count4InARow board "B" 0 0)))
                    (compRound (+ (first comp)(second comp)(count4InARow board "W" 0 0)))
                    (humanTournament (+ tournamentScoreHuman humanRound))
                    (compTournament (+ tournamentScoreComp compRound)))

                    (princ "Round scores:")
                    (terpri)
                    (printScores humanRound compRound)
                    (princ "Tournament scores:")
                    (terpri)
                    (printScores humanTournament compTournament)
                    (list humanTournament compTournament)
            )
        )           
  )
)

; ********************************************************************* 
; Function Name: printScores
; Purpose: score printing function
; Parameters: 
;        humanScore - score passed for human 
;        compScore - score passed for computer 
; Return Value: none
; Algorithm: none
; Assistance Received: none 
; ***********************************************************************
(defun printScores (humanScore compScore)
  (princ "Human: ")
  (princ humanScore)
  (terpri)
  (princ "Computer: ")
  (princ compScore)
  (terpri)
)

; ********************************************************************* 
; Function Name: anotherRoundPrompt
; Purpose: get valid input from user 
; Parameters: none
; Return Value: 0 if no more rounds, 1 to play another round 
; Algorithm: called recursively while input is invalid
; Assistance Received: none 
; ***********************************************************************
(defun anotherRoundPrompt()
  (let ((input (read-line)))
    (cond   ((string-equal input "")
             (anotherRoundPrompt))
            ((and (char-equal (char input 0) #\N) (= (length input) 1))
              0)
            ( (and (char-equal (char input 0) #\Y) (= (length input) 1))  
              1 )
            ( t
            (princ "Please enter valid input 'Y' or 'N'.")
            (terpri)
            (anotherRoundPrompt))
    )
  )
)