; ********************************************************************* 
; Function Name: boardInit
; Purpose: initialize board in list of lists form
; Parameters: 
;        count - number of rows (call with 19 for Pente)
; Return Value: initialized board
; Algorithm: cons to list of lists, row after row
; Assistance Received: none 
; ***********************************************************************
(defun boardInit(count)     
    (cond ((= count 0)
                ())
          (   t
            (cons '( O O O O O O O O O O O O O O O O O O O )(boardInit (- count 1))))
    )
)

; ********************************************************************* 
; Function Name: play
; Purpose: carry out the play of a turn
; Parameters: 
;        board - current board state 
;        human - human capture count and round score
;        comp - computer capture count and round score
;        nextplayer - whose turn it is currently and their stone color
; Return Value: list of board, human, comp, nextplayer, and exit case
; Algorithm:       
;       1. Check if board is full. Declare a draw and return list values.
;       2. While game isn't over and human didn't type quit:
;           a. Display board and capture counts.
;           b. Current player identified, appropriate function to make a move called.
;           c. Check if move placement resulted in the game to be over/quit.
;           d. Update board, captures, and score, if applicable, and call play
;              recursively with the next player.
; Assistance Received: none 
; ***********************************************************************
(defun play(board human comp nextplayer)
  ;; Display purposes
  (boardPrint board 19)
  (capturePrint human comp)

        ;; Board full!
  (cond ((= (getMoveCount board 0 0 0) (* 19 19))
          (princ "Game over! Full board. It's a draw!")    
          (terpri)
          (list board human comp nextplayer "over"))
        ;; Game carries on
        (t 
            (let ((player (first nextplayer))
                  (color (first (rest nextplayer))))

              ;; Human method to make a move
              (cond ((string-equal "Human" player)
                      (let* ((move (makeMove board color))
                              (newboard (first move))
                              (scoreinc (second move))
                              (capinc (third move))) 

                                ;; Save and exit game
                          (cond ( (equal move ())
                                  (list board human comp nextplayer "quit")
                                )  
                                ;; Move resulted in capture and/or score increase
                                ( (= (length move) 3)
                                  (let ((capcount (incCaptures human capinc))) 
                                            ;; Check if increase resulted in game being over
                                      (cond ( (or (>= (first capcount) 5)(>= scoreinc 5))
                                            (boardPrint newboard 19)
                                            (capturePrint capcount comp)
                                            (terpri)
                                            (princ "Game over! Human won!")
                                            (terpri)
                                            (list newboard (incScore capcount scoreinc) comp nextplayer "over")
                                            )
                                            ;; Keep playing
                                            ( t 
                                            (play newboard capcount comp (getNextPlayer nextplayer))
                                            )
                                      
                                      )
                                  )
                                )
                                ;; Keep playing
                                ( t
                                  (play move human comp (getNextPlayer nextplayer))
                                )
                          )
                      )
                    
                    )
                    ;; Computer method to make a move
                    ( t 
                      (terpri)
                      (princ "Computer turn") 
                      (terpri)

                      ;; Use strategy for computer move
                      (let ((move (strategy board (string (char color 0)))))
                          (terpri)
                          (princ "The computer placed a ")
                          (princ (string-downcase color))
                          (princ " stone on ")
                          (rowColToMoveName (first move) (second move))
                          (terpri)
                          (let* ((moveplaced (placeMove board (char color 0) move))
                                (newboard (first moveplaced))
                                (scoreinc (second moveplaced))
                                (capinc (third moveplaced)))

                                    ;; Move resulted in capture and/or score increase
                              (cond ( (= (length moveplaced) 3)
                                      (let ((capcount (incCaptures comp capinc))) 
                                                ;; Check if increase resulted in game being over
                                          (cond ( (or (>= (first capcount) 5)(>= scoreinc 5))
                                                (boardPrint newboard 19)
                                                (capturePrint human capcount)
                                                (terpri)
                                                (princ "Game over! Computer won!")
                                                (terpri)
                                                (list newboard human (incScore capcount scoreinc) nextplayer "over")
                                                )
                                                ;; Keep playing
                                                ( t 
                                                (play newboard human capcount (getNextPlayer nextplayer))
                                                )
                                          )
                                      )
                                    )
                                    ;; Keep playing
                                    ( t
                                      (play moveplaced human comp (getNextPlayer nextplayer))
                                    )
                              )
                          )
                      )
                    )
              )
            )
        )
  )
)

; ********************************************************************* 
; Function Name: boardPrint
; Purpose: display board with labels
; Parameters: 
;        board - current board state 
;        num - for recursion (start with 19)
; Return Value: none
; Algorithm: none
; Assistance Received: none 
; ***********************************************************************
(defun boardPrint(board num)
  (cond ( (= num 0)
          (terpri)
          (princ "    A B C D E F G H I J K L M N O P Q R S")
          (terpri))
        ( t
          (cond ((< num 10)
                (terpri)
                (format t "~A  " num))
                (t 
                (print num))
          )
          (princ (first board))
          (boardPrint (rest board)(- num 1)))
  )
)

; ********************************************************************* 
; Function Name: capturePrint
; Purpose: display current capture count for human and computer
; Parameters: 
;        human - human stats 
;        comp - computer states
; Return Value: none
; Algorithm: none
; Assistance Received: none 
; ***********************************************************************
(defun capturePrint(human comp)
  (princ "Human:")
  (terpri)
  (princ "Captures: ")
  (princ (first human))
  (terpri)
  (princ "Computer:")
  (terpri)
  (princ "Captures: ")
  (princ (first comp))
  (terpri)
)

; ********************************************************************* 
; Function Name: getMoveCount
; Purpose: get number of moves that have been placed on the board
; Parameters: 
;        board - current board state 
;        row - row being checked
;        column - column being checked
;        count - move count
; Return Value: move count 
; Algorithm: traverse board -> if space isn't empty, increase count.
; Assistance Received: none 
; ***********************************************************************
(defun getMoveCount(board row column count)
  (cond ( (= row 19) 
          count )
        ( (= column 19)
        (getMoveCount board (+ row 1) 0 count) )
        ( t 
          (cond ((string-not-equal (getElementByPos board (list row column)) "O")
                (getMoveCount board row (+ column 1) (+ count 1)))
                ( t
                (getMoveCount board row (+ column 1) count))
          )
        )
  )
)

; ********************************************************************* 
; Function Name: getElementByPos
; Purpose: get element that is in a board positions ("O", "B", "W")
; Parameters: 
;        board - current board state 
;        position - list of row and column
; Return Value: element in that position
; Algorithm: none
; Assistance Received: none 
; ***********************************************************************
(defun getElementByPos(board position)
  (let* ((row (first position))
        (column (first (rest position))))
        (string (nth column (nth row board)))
  )
)

; ********************************************************************* 
; Function Name: makeMove
; Purpose: prompt player to make a move, check if input returns to be valid,
;          check if board space is empty, place move if so
; Parameters: 
;        board - current board state 
;        color - color of current player
; Return Value: board after move is placed
; Algorithm:       
;       1. Prompt user to make a move.
;       2. Check if quit or help was entered, respond to request accordingly.
;       3. If input returns to be invalid, call makeMove again.
;       4. If it is the first move and entry isn't J10, call makeMove.
;       5. If it is the first player's second turn, check move is at least
;           3 intersections away from center stone.
;       6. If board space is empty, call placeMove to place stone.
;       7. Otherwise, board space taken, call makeMove to try again.
; Assistance Received: none 
; ***********************************************************************
(defun makeMove(board color)
  (terpri)
  (princ "Enter intersection to place ")
  (princ (string-downcase color))
  (princ " stone: ")
  (terpri)

  (let* ((move (read-line)))
          ;; No move placed, return to play function to save game
    (cond ( (string-equal (string-upcase move) "QUIT")
                    ()
          )
          ;; Strategy suggests a move, makeMove called for user to decide
          ( (string-equal (string-upcase move) "HELP")
           (terpri)
           (strategy board (string (char color 0)))
           (makeMove board color)
          )
          ;; Invalid move syntax, moveToRowCol reports error, try again
          ((equal (moveToRowCol move) ())
            (terpri)
            (makeMove board color)
          )
          ;; Restriction on first player's first turn, try again
          ( (and (= (getMoveCount board 0 0 0) 0) (not (string-equal move (string-upcase "J10"))) )
              (princ "First move must be J10.")
              (terpri)
              (makeMove board color)
          )
          ;; Restriction on first player's second turn, try again
          ( (and (= (getMoveCount board 0 0 0) 2) 
             (> (first (moveToRowCol move)) 6)  (< (first (moveToRowCol move)) 12) 
                (> (second (moveToRowCol move)) 6)  (< (second (moveToRowCol move)) 12))
              (princ "On the first player's second turn, move must be at least 3 intersections away from the center stone.")
              (terpri)
              (makeMove board color)
          )
          ;; Move placed
          ((string-equal (getElementByPos board (moveToRowCol move)) "O")
            (placeMove board (char color 0) (moveToRowCol move))
            )
          ;; Move cannot be placed, try again
          ( t
          (princ "Board space taken.")
          (terpri)
          (makeMove board color)
          )
    )
  ) 
)

; ********************************************************************* 
; Function Name: moveToRowCol
; Purpose: checks syntax of passed move, returns row and column if valid
; Parameters: 
;        move - move to be converted to row and column format
; Return Value: row and column if syntax correct, () otherwise
; Algorithm:       
;       1. Check first character to be between A and S.
;       2. Check the second and third values (if any) to be a number 1 - 19.
;       3. If errors encountered, print them to screen and return empty list.
;       4. Otherwise, convert move to row and column and return them in a list.
; Assistance Received: none 
; ***********************************************************************
(defun moveToRowCol(move) 
  (cond ( (or (char-lessp (char move 0) #\A)(char-greaterp (char move 0) #\S))
          (princ "Invalid input. First character must be between A and S")
          ()
        )
        ( (= (length move) 2)
          (cond ( (and (> (- (char-code (char move 1)) 48) 0) (<= (- (char-code (char move 1)) 48) 9))
                  (list  (- 19 (- (char-code (char move 1)) 48)) (- (char-code (char-upcase (char move 0))) 65))
                )
                ( t
                  (princ "Invalid input. Number must be greater than zero.")
                  ()
                )
          )
        )
        ( (= (length move) 3)
          (cond ( (parse-integer (subseq move 1 3) :junk-allowed t) 
                  (cond ( (and (> (parse-integer (subseq move 1 3)) 0) (< (parse-integer (subseq move 1 3)) 20))
                          (list (- 19 (parse-integer (subseq move 1 3))) (- (char-code (char-upcase (char move 0))) 65))
                        )
                        ( t
                          (princ "Invalid input. Number must be between 1 and 19.")
                          ()
                        )
                  ) 
                )
                ( t
                  (princ "Invalid input.")
                    ()
                )
          )
        )
        ( t 
          (princ "Invalid input.")
          ()
        )
  )
)

; ********************************************************************* 
; Function Name: placeMove
; Purpose: place stone, return updated board and any increments to score/captures
; Parameters: 
;        board - board state before move placed
;        color - color being placed
;        position - where it is being placed
; Return Value: board after move is placed, with scores the move resulted in,
;                 if any!
; Algorithm: none
; Assistance Received: none 
; ***********************************************************************
(defun placeMove(board color position)
  (let* ((row (first position))
        (column (first (rest position))))   

        (let* ((newboard (boardAfterMove board row column color))
              (checkcap (checkCapture newboard color row column 0))
              (boardaftercaptures (first checkcap))
              (scorecaptures (second checkcap))
              (score5 (check5InARow newboard row column color)) )

              (cond ((or (> score5 0) (> scorecaptures 0))
                  (list boardaftercaptures score5 scorecaptures))
                  (t
                   newboard
                  )
            )
        )
  )
)

; ********************************************************************* 
; Function Name: boardAfterMove
; Purpose: return updated board after a move is placed
; Parameters: 
;        board - current board state 
;        row - row of move placement
;        column - column of move placement
;        color - stone color being placed
; Return Value: updated board
; Algorithm: none
; Assistance Received: none 
; ***********************************************************************
(defun boardAfterMove(board row column color)
  (cond ((= row 0)
          (cons (rowAfterMove (first board) column color)(nthcdr 1 board))
        )
        (
          (cons (first board)(boardAfterMove (rest board) (- row 1) column color))
        )
  )
)

; ********************************************************************* 
; Function Name: rowAfterMove
; Purpose: return updated row after a move is placed
; Parameters: 
;        row - row of move placement
;        column - column of move placement
;        color - stone color being placed
; Return Value: updated row
; Algorithm: none
; Assistance Received: none 
; ***********************************************************************
(defun rowAfterMove (row column color)
  (cond ((= column 0)
          (cons color (nthcdr 1 row))
        )
        (
          (cons (first row)(rowAfterMove (rest row)(- column 1) color))
        )
  
  )
)

; ********************************************************************* 
; Function Name: checkCapture
; Purpose: check if a move resulted in captures, update board, return score
; Parameters: 
;        board - board state (being updated in function if there is capture)
;        color - color of stone placed
;        row - row of move that is being checked if it resulted in captures
;        column - column of move that is being checked if it resulted in captures
;        score - capture score for this move
; Return Value: board after captured stones are removed, if any, along with score
; Algorithm: 
;         1. Check each direction, while within board bounds, if capture pattern exists.
;         2. If so, remove captured stones, and pass updated board and score back to 
;            function until all directions exhuasted.
;         3. When no more captures are found, return updated board and score.
; Assistance Received: none 
; ***********************************************************************
(defun checkCapture(board color row column score)
  (let ((mycolor (string color))
        (opponentcolor (getOpponentColor color))
        )
              ;; Checking all directions for capture pattern
        (cond ( (and (> row 2) (string-equal (getElementByPos board (list (- row 1) column)) opponentcolor)
                              (string-equal (getElementByPos board (list (- row 2) column)) opponentcolor)
                              (string-equal (getElementByPos board (list (- row 3) column)) mycolor)) 
                    (let* ((removecap1 (boardAfterMove board (- row 1) column "O") ) 
                          (removecap2 (boardAfterMove removecap1 (- row 2) column "O")))
                          (checkCapture removecap2 color row column (+ score 1))
                    )
              )
              ( (and (< row 16) (string-equal (getElementByPos board (list (+ row 1) column)) opponentcolor)
                              (string-equal (getElementByPos board (list (+ row 2) column)) opponentcolor)
                              (string-equal (getElementByPos board (list (+ row 3) column)) mycolor)) 
                    (let* ((removecap1 (boardAfterMove board (+ row 1) column "O") ) 
                          (removecap2 (boardAfterMove removecap1 (+ row 2) column "O")))
                          (checkCapture removecap2 color row column (+ score 1))
                    )
              )
              ( (and (> column 2) (string-equal (getElementByPos board (list row (- column 1))) opponentcolor)
                              (string-equal (getElementByPos board (list row (- column 2))) opponentcolor)
                              (string-equal (getElementByPos board (list row (- column 3))) mycolor)) 
                    (let* ((removecap1 (boardAfterMove board row (- column 1) "O") ) 
                          (removecap2 (boardAfterMove removecap1 row (- column 2) "O")))
                          (checkCapture removecap2 color row column (+ score 1))
                    )
              )
              ( (and (< column 16) (string-equal (getElementByPos board (list row (+ column 1))) opponentcolor)
                              (string-equal (getElementByPos board (list row (+ column 2))) opponentcolor)
                              (string-equal (getElementByPos board (list row (+ column 3))) mycolor)) 
                    (let* ((removecap1 (boardAfterMove board row (+ column 1) "O") ) 
                          (removecap2 (boardAfterMove removecap1 row (+ column 2) "O")))
                          (checkCapture removecap2 color row column (+ score 1))
                    )
              )
              ( (and (< column 16)(< row 16) (string-equal (getElementByPos board (list (+ row 1) (+ column 1))) opponentcolor)
                              (string-equal (getElementByPos board (list (+ row 2) (+ column 2))) opponentcolor)
                              (string-equal (getElementByPos board (list (+ row 3) (+ column 3))) mycolor)) 
                    (let* ((removecap1 (boardAfterMove board (+ row 1) (+ column 1) "O") ) 
                          (removecap2 (boardAfterMove removecap1 (+ row 2) (+ column 2) "O")))
                          (checkCapture removecap2 color row column (+ score 1))
                    )
              )
              ( (and (> column 2)(> row 2) (string-equal (getElementByPos board (list (- row 1) (- column 1))) opponentcolor)
                              (string-equal (getElementByPos board (list (- row 2) (- column 2))) opponentcolor)
                              (string-equal (getElementByPos board (list (- row 3) (- column 3))) mycolor)) 
                    (let* ((removecap1 (boardAfterMove board (- row 1) (- column 1) "O") ) 
                          (removecap2 (boardAfterMove removecap1 (- row 2) (- column 2) "O")))
                          (checkCapture removecap2 color row column (+ score 1))
                    )
              )
              ( (and (> column 2)(< row 16) (string-equal (getElementByPos board (list (+ row 1) (- column 1))) opponentcolor)
                              (string-equal (getElementByPos board (list (+ row 2) (- column 2))) opponentcolor)
                              (string-equal (getElementByPos board (list (+ row 3) (- column 3))) mycolor)) 
                    (let* ((removecap1 (boardAfterMove board (+ row 1) (- column 1) "O") ) 
                          (removecap2 (boardAfterMove removecap1 (+ row 2) (- column 2) "O")))
                          (checkCapture removecap2 color row column (+ score 1))
                    )
              )
              ( (and (< column 16)(> row 2) (string-equal (getElementByPos board (list (- row 1) (+ column 1))) opponentcolor)
                              (string-equal (getElementByPos board (list (- row 2) (+ column 2))) opponentcolor)
                              (string-equal (getElementByPos board (list (- row 3) (+ column 3))) mycolor)) 
                    (let* ((removecap1 (boardAfterMove board (- row 1) (+ column 1) "O") ) 
                          (removecap2 (boardAfterMove removecap1 (- row 2) (+ column 2) "O")))
                          (checkCapture removecap2 color row column (+ score 1))
                    )
              )
              ;; No more captures, return board and score
              ( t 
              (list board score)
              )                             
        )
  )
)

; ********************************************************************* 
; Function Name: getOpponentcolor
; Purpose: get opponent color
; Parameters: 
;        color - current player's color
; Return Value: opponent's color
; Algorithm: none
; Assistance Received: none 
; ***********************************************************************
(defun getOpponentcolor(color)
  (cond ((string-equal (string color) "W")
          "B")
          (t
          "W")
  )
)

; ********************************************************************* 
; Function Name: check5InARow
; Purpose: check if more resulted in 5 in a row, return score
; Parameters: 
;        board - board state 
;        color - color of stone placed
;        row - row of move that is being checked if it resulted in 5 in a row
;        column - column of move that is being checked if it resulted in 5 in a row
; Return Value: score for 5 in a rows
; Algorithm: 
;         1. Call helper5InARow in all directions, check how many consecutive stones there are.
;         2. Add them into score if score >= 5 (checked by getScoreFor5).
; Assistance Received: none 
; ***********************************************************************
(defun check5InARow(board row column color)
  (let* ((score1 (helper5InARow board color row column 1 0 
                        (helper5InARow board color (- row 1) column -1 0 0)) )
         (score2 (helper5InARow board color row column 0 1 
                        (helper5InARow board color row (- column 1) 0 -1 0)) )
         (score3 (helper5InARow board color row column 1 1 
                        (helper5InARow board color (- row 1) (- column 1) -1 -1 0)) )
         (score4 (helper5InARow board color row column 1 -1 
                        (helper5InARow board color (- row 1) (+ column 1) -1 1 0)) )
         (score (+ (+ (+ (getScoreFor5 score1) (getScoreFor5 score2)) (getScoreFor5 score3)) (getScoreFor5 score4)) )
        )

      score
  )
)

; ********************************************************************* 
; Function Name: helper5InARow
; Purpose: count stones recursively in a direction while color matches
; Parameters: 
;        board - board state 
;        color - color of stone placed
;        row - row being checked
;        column - column being check
;        incrow - row increment, direction being checked
;        inccolumn - column increment, direction being checked
; Return Value: count of matching color stones in given direction
; Algorithm: 
;         1. Check if row and column match expected color. If so, increment count,
;            and check next row col positions.
;         2. Otherwise, return count that matched in that direction.
; Assistance Received: none 
; ***********************************************************************
(defun helper5InARow (board color row column incrow inccolumn count)
  (cond ( (and (and (and (>= row 0)(>= column 0)) (and (< row 19)(< column 19)) )
           (string-equal (getElementByPos board (list row column)) (string color)))
                  (helper5InARow board color (+ row incrow) (+ column inccolumn)
                    incrow inccolumn (+ count 1))
        )
        ( t
         count
        )
  )
)

; ********************************************************************* 
; Function Name: getScoreFor5
; Purpose: returns score of 5 if there are 5 consecutive stones, 0 otherwise
; Parameters: 
;        score - number of consecutive stones
; Return Value: score of 0 or 5
; Algorithm: none
; Assistance Received: none 
; ***********************************************************************
(defun getScoreFor5(score)
  (cond ((>= score 5)
                5)
        ( t
          0)
        )
)

; ********************************************************************* 
; Function Name: incCaptures
; Purpose: updates capture count in list for player
; Parameters: 
;        player - list of captures and score
;        score - capture count increase
; Return Value: updated list
; Algorithm: none
; Assistance Received: none 
; ***********************************************************************
(defun incCaptures (player score)
  (list (+ (first player) score) (second player))
)

; ********************************************************************* 
; Function Name: incScore
; Purpose: updates score in list for player
; Parameters: 
;        player - list of captures and score
;        score - score increase increase
; Return Value: updated list
; Algorithm: none
; Assistance Received: none 
; ***********************************************************************
(defun incScore (player score)
  (list (first player) (+ (second player) score))
)

; ********************************************************************* 
; Function Name: getNextPlayer
; Purpose: gets next player and stone color in list form
; Parameters: 
;        currentplayer - list of player and stone color
; Return Value: updated list of next player and their stone color
; Algorithm: none
; Assistance Received: none 
; ***********************************************************************
(defun getNextPlayer(currentplayer)
  (cond ((equal currentplayer '("Human" "White"))
        (list "Computer" "Black"))
        ((equal currentplayer '("Human" "Black"))
        (list "Computer" "White"))
        ((equal currentplayer '("Computer" "White"))
        (list "Human" "Black"))
        ((equal currentplayer '("Computer" "Black"))
        (list "Human" "White"))
  )
)

; ********************************************************************* 
; Function Name: rowColToMoveName
; Purpose: prints move name for row and column 
; Parameters: 
;        row - row being converted
;        col - column being converted
; Return Value: none
; Algorithm: none
; Assistance Received: none 
; ***********************************************************************
(defun rowColToMoveName(row col)
  (princ (code-char (+ col 65)))
  (princ (- 19 row))
)

; ********************************************************************* 
; Function Name: count4InARow
; Purpose: count total number of 4 in a rows for given color/player
; Parameters: 
;        board - board state 
;        color - color being checked
;        scoresum - total number of 4 in a rows
;        loopcount - to check orientations recursively
; Return Value: scoresum
; Algorithm: 
;         1. Call helper4count for each direction, update score as received.
;         2. When board checked completely, return scoresum.
; Assistance Received: none 
; ***********************************************************************
(defun count4InARow(board color scoresum loopcount)
  (cond ((= loopcount 19)
            scoresum)
        ( t
          (let* ((dir1 (helper4Count board color loopcount 0 0 1 0 0))
                  (dir2 (helper4Count board color 0 loopcount 1 0 0 0))
                  (dir4 (helper4Count board color loopcount 18 1 -1 0 0))
                  (dir6 (helper4Count board color loopcount 0 1 1 0 0))
                  (score (+ dir1 dir2 dir4 dir6)))

                    ;; Special cases to not double count central diagonals
                    (cond ((= loopcount 18)
                            (let ((dir5 (helper4Count board color 0 loopcount 1 1 0 0)))
                                  (count4InARow board color (+ scoresum score dir5) (+ loopcount 1))
                            )
                          )
                          ((= loopcount 0)
                            (let ((dir3 (helper4Count board color 0 loopcount 1 -1 0 0)))
                                (count4InARow board color (+ scoresum score dir3) (+ loopcount 1))
                            )
                          )
                          ( t
                            (let ((dir3 (helper4Count board color 0 loopcount 1 -1 0 0))
                                  (dir5 (helper4Count board color 0 loopcount 1 1 0 0)))
                                  (count4InARow board color (+ scoresum score dir3 dir5) (+ loopcount 1))
                            )
                          )
                    )
          )
        )
  )
)

; ********************************************************************* 
; Function Name: helper4Count
; Purpose: count stones recursively in a direction while color matches,
;          if count is 4 when no match is found, increment score
; Parameters: 
;        board - board state 
;        color - color being checked
;        row - row being checked
;        column - column being check
;        incrow - row increment, direction being checked
;        inccolumn - column increment, direction being checked
;        currentcount - number of stones in a row
;        score - number of 4 in a rows in given direction
; Return Value: score
; Algorithm: 
;         1. While within board bounds, check if row and column match color.
;         2. If so, increase count and update row and column by passed increments.
;         3. When match no longer occurs or board bounds exceeded, check if count is 4.
;            If so, increment score.
; Assistance Received: none 
; ***********************************************************************
(defun helper4Count(board color row column incrow inccolumn currentcount score)

          ;; While within board bounds, check if row and column match color
  (cond ( (and (>= row 0) (< row 19) (>= column 0) (< column 19))
          (cond ((string-equal (getElementByPos board (list row column)) color)
                  ;; Match! Increase count
                  (helper4Count board color (+ row incrow) (+ column inccolumn) incrow inccolumn (+ currentcount 1) score)
                )
                ( t 
                        ;; Match no longer occurs, increase score if 4 in a row
                  (cond ( ( and (= currentcount 4))
                          (helper4Count board color (+ row incrow) (+ column inccolumn) incrow inccolumn 0 (+ score 1))
                        )
                        ;; Reset currentcount
                        (t 
                          (helper4Count board color (+ row incrow) (+ column inccolumn) incrow inccolumn 0 score)
                        )
                  )
                )
          )
        )
        ( t
                ;; Board bounds exceeded, increase score if 4 in a row
          (cond ( (= currentcount 4)
                  (+ score 1)
                )
                ( t
                  score
                )
          )
        )
  )
)