; ********************************************************************* 
; Function Name: strategy
; Purpose: suggest a move based on outlined strategy design 
; Parameters: 
;        board - current board state 
;        color - color of player calling the strategy
; Return Value: suggested move for current color
; Algorithm:       
;       1. Check if there is a winning 5 in a row move. Return highest scoring one.
;       2. Check if the oppenent can win with 5 in a row and block this move. 
;       3. Check if the opponent can win in two moves and block one of them.
;       4. Check if a capture can be made.  Return highest scoring one.
;       5. Check if current player can avoid being captured.
;       6. If current player has a chain of 3 stones, add a 4th.
;       7. If current player has a chain of 2 stones, add a 3rd.
;       8. If current player has one stone, add another near it.
;       9. If opponent has one stone, add another near it.
;       10. If it is the first move, suggest only J10.
;       11. Random move (in case of emergency).
; Assistance Received: none 
; ***********************************************************************
(defun strategy(board color)

  ;; Heading to explain the strategy
  (princ "Strategy: ")
  
  (let*  ((opponentColor (getOpponentColor color))
          (maxWinningPos (findWinningMax board color 0 0 ()))
          (winningOpponent (findWinningMax board opponentColor 0 0 ()))
          (winningOpponentInTwo (blockWininTwo board opponentColor))
          (captureMax (maxCapture board color 0 0 ()))
          (avoidCapture (maxCapture board opponentColor 0 0 ()))
          (cluster3 (find3 board color))
          (cluster2 (find2 board color))
          (cluster1 (find1 board color))
          (nearOpponent (find1 board opponentColor))
          (rand (randomMove board color)))
                
                 ;; Executed if there is a winning move
          (cond  ((not (equal maxWinningPos ()))
                  (princ "Found winning move with highest score: ") 
                  (rowColToMoveName (first maxWinningPos)(second maxWinningPos))
                  maxWinningPos)

                  ;; Executed if the opponent has a winning move
                  ((not (equal winningOpponent ()))
                  (princ "Block winning move: ")
                  (rowColToMoveName (first winningOpponent)(second winningOpponent))
                  winningOpponent)

                  ;; Executed if the opponent has a winning move two moves away
                  ((not (equal winningOpponentInTwo ()))
                  (princ "Block winning in 2 moves: ")
                  (rowColToMoveName (first winningOpponentInTwo)(second winningOpponentInTwo))
                  winningOpponentInTwo)

                  ;; Executed if opponent can be captured
                  ((not (equal captureMax ()))
                  (princ "Capture opponent: ")
                  (rowColToMoveName (first captureMax)(second captureMax))
                  captureMax)

                  ;; Executed if current player can avoid being captured
                  ((not (equal avoidCapture ()))
                  (princ "Avoid being captured: ")
                  (rowColToMoveName (first avoidCapture)(second avoidCapture))
                  avoidCapture)

                  ;; Executed if cluster of 3 can be turned to 4
                  ((not (equal cluster3 ()))
                  (princ "Create a row of 4 stones: ")
                  (rowColToMoveName (first cluster3)(second cluster3))
                  cluster3)

                  ;; Executed if cluster of 2 can be turned to 3
                  ((not (equal cluster2 ()))
                  (princ "Create a row of 3 stones: ")
                  (rowColToMoveName (first cluster2)(second cluster2))
                  cluster2)

                  ;; Executed if cluster of 1 can be turned to 2
                  ((not (equal cluster1 ()))
                  (princ "Add stone near existing stone: ")
                  (rowColToMoveName (first cluster1)(second cluster1))
                  cluster1)

                  ;; Executed if player has no stones on board but opponent does
                  ((not (equal nearOpponent ()))
                  (princ "Add stone near opponent's stone: ")
                  (rowColToMoveName (first nearOpponent)(second nearOpponent))
                  nearOpponent)

                  ;; Executed if it is the first move
                  ((= (getMoveCount board 0 0 0) 0)
                  (princ "First move must be J10.")
                  (list 9 9))

                  ;; Executed if something went wrong
                  ((not (equal rand ()))
                  (princ "Random move: ")
                  (rowColToMoveName (first rand)(second rand))
                  rand)
          )
  ) 
)

; ********************************************************************* 
; Function Name: findWinningMax
; Purpose: find winning 5 in a row positions, return the one with maximum 5 in a row score
; Parameters: 
;        board - current board state 
;        color - color being checked
;        row - current row being checked
;        col - current column being checked
;        positions - list of positions storing score, row, and col of winning positions
; Return Value: row and column with highest score if winning position exists, 
;                empty list otherwise
; Algorithm:       
;       1. Traverse the board for empty spaces.
;       2. On each empty space, place a stone, and check if it results in 5 in a row.
;       3. If score is > 0, store the score it yields and the position, and pass to
;          recursive function call until end of board.
;       4. At end of board, returns empty list if there are no positions. Otherwise,
;          call maxPos to return the highest scoring row and column.
; Assistance Received: none 
; ***********************************************************************
(defun findWinningMax (board color row col positions)

          ;; End of board reached, return highest scoring winning position, if any
  (cond ( (= row 19)
          (cond ( (equal positions ())
                  () )
                ( t 
                (maxPos positions '(0 0 0)))
          )
        )
          ;; End of row reached, increase row, reset column
        ( (= col 19)
          (findWinningMax board color (+ row 1) 0 positions)
        )
        ( t       
                ;; Traverse the board for empty spaces
          (cond ( (string-equal (getElementByPos board (list row col)) "O") 

                        ;; Check if placement results in a 5 in a row, store score and position if so
                  (let ((score (check5InARow (boardAfterMove board row col color) row col color)))
                        (cond ( (> score 0)
                                (findWinningMax board color row (+ col 1) (cons (list score row col) positions))
                              )

                              ;; Otherwise, continue board traversal
                              ( t 
                               (findWinningMax board color row (+ col 1) positions)
                              )
                        )
                  )
                )

                ;; Continue board traversal if space taken
                (t 
                   (findWinningMax board color row (+ col 1) positions)
                )
          )
        )
  )
)

; ********************************************************************* 
; Function Name: maxPos
; Purpose: find position in list of positions with highest score
; Parameters: 
;        positions - list of position lists holding score, row, and columm
;        max - position with maximum score value
; Return Value: row and column with highest score
; Algorithm:       
;       1. If positions is empty, it has been traversed. Return max row and col.
;       2. Check if first element in positions has higher score than current max,
;           replace max in recursive function call if so.
;       3. Call maxPos with remaining positions to be checked.
; Assistance Received: none 
; ***********************************************************************
(defun maxPos (positions max)

        ;; No more positions. Return highest scoring row and column
  (cond ((equal positions ())
        (list (second max)(third max)))

        ;; Check if first element in positions has higher score 
        ;; than current max, replace max if so
        ( t
          (cond ( (> (first (first positions)) (first max))
                  (maxPos (rest positions) (first positions))
                ) 
                ( t 
                  (maxPos (rest positions) max)
                )
          )
        )
  )
)

; ********************************************************************* 
; Function Name: blockWininTwo
; Purpose: block a win two moves away
; Parameters: 
;        board - current board state 
;        color - color being checked
; Return Value: row and column of blocking move if any, otherwise empty list
; Algorithm:       
;       1. Check the board for 4 different patterns 2 moves away from winning with 
;          the help of findPattern.
;       2. If at least one pattern is found, the appropriate blocking position is returned.
;       3. If none of the patterns are found, empty list returned.
; Assistance Received: none 
; ***********************************************************************
(defun blockWininTwo(board color)
        ;; Patterns that are 2 moves away from winning if not intervened immediately
  (let* ((pattern1 (findPattern board (concatenate 'string "O" color color color "O" "O") 0))
         (pattern2 (findPattern board (concatenate 'string "O" color color "O" color "O") 0))
         (pattern3 (findPattern board (concatenate 'string "O" color "O" color color "O") 0))
         (pattern4 (findPattern board (concatenate 'string "O" "O" color color color "O") 0)))

          ;; If at least one pattern is found, the appropriate blocking position is returned
          (cond ((not (equal pattern1 () ) )
                    (fifth pattern1)) 
                ((not (equal pattern2 () ) )
                    (fourth pattern2)) 
                ((not (equal pattern3 () ) )
                    (third pattern3)) 
                ((not (equal pattern4 () ) )
                    (second pattern4)) 

                ;; None of the patterns found
                ( t
                () ) 
          )
  )
)

; ********************************************************************* 
; Function Name: findPattern
; Purpose: find passed pattern and return its board positions
; Parameters: 
;        board - current board state 
;        pattern - pattern being searched for
;        loopcount - serves as loop count to iterate through board
; Return Value: positions of the pattern or empty list if no match.
; Algorithm:       
;       1. Call patternHelper for each orientation on the board.
;       2. Store the returns in local direction labels.
;       3. If one of the returns has positions stored, return them.
;       4. Call findPattern with increase loopcount until a set of positions
;          is found or entire board has been traversed.
;       5. No match: return empty list.
; Assistance Received: none 
; ***********************************************************************
(defun findPattern(board pattern loopcount)

        ;; Entire board has been traversed, no match found
  (cond ((= loopcount 19)
            () )

        ;; Keep checking
        (t 
            (let ((dir1 (patternHelper board loopcount 0 0 1 pattern () 0)) 
                  (dir2 (patternHelper board 0 loopcount 1 0 pattern () 0))
                  (dir4 (patternHelper board loopcount 18 1 -1  pattern () 0))
                  (dir6 (patternHelper board loopcount 0 1 1 pattern () 0)))

                  ;; If one of the directions has positions stored, return them
                  (cond ( (not (equal dir1 ()) )
                            dir1)
                        ( (not (equal dir2 ()) )
                          dir2)
                        ( (not (equal dir4 ()) )
                            dir4)
                        ( (not (equal dir6 ()) )
                            dir6 )
                        ( t 
                          
                          ;; Special cases to not double count central diagonals, same logic as above
                          (cond ((= loopcount 18)
                                  (let ((dir5 (patternHelper board 0 loopcount 1 1 pattern () 0)))
                                        (cond ( (not (equal dir5 ()) )
                                                 dir5)
                                              ( t
                                                (findPattern board pattern (+ loopcount 1))
                                              )
                                        )
                                  )
                                )
                                ((= loopcount 0)
                                  (let ((dir3 (patternHelper board 0 loopcount 1 -1 pattern () 0)))
                                    (cond ( (not (equal dir3 ()) )
                                              dir3 )
                                          ( t
                                            (findPattern board pattern (+ loopcount 1))
                                          )
                                    )
                                  )
                                )
                                ( t
                                  (let ((dir3 (patternHelper board 0 loopcount 1 -1 pattern () 0))
                                        (dir5 (patternHelper board 0 loopcount 1 1 pattern () 0)))
                                        (cond ( (not (equal dir3 ()) )
                                                dir3 )
                                              ( (not (equal dir5 ()) )
                                                dir5 )
                                            ( t
                                              (findPattern board pattern (+ loopcount 1))
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
)

; ********************************************************************* 
; Function Name: patternHelper
; Purpose: find passed pattern and return its board positions
; Parameters: 
;        board - current board state 
;        row - row being checked
;        col - column being check
;        incrow - row increment, direction being checked
;        inccol - column increment, direction being checked
;        pattern - pattern being searched for
;        positions - list of positions of the pattern
;        count - keep track of which pattern element is needed next 
;                based on number of positions found already
; Return Value: positions of the pattern or empty list if no match
; Algorithm:       
;        1. While within board bounds, tranverse board by changing row and 
;        col by incrow and inccol.
;        2. At each row col position, check if color corresponds to what 
;        is expected in the pattern.
;        3. If it does, add to positions list and increment count.
;        4. Otherwise, call patternHelper with empty positions list and 0 count,
;        restarting at the first position in positions, if any, incremented by 
;        the increments. This is necessary to not skip over any position that 
;        reset the pattern but are part of the next pattern right away.
;        5. patternHelper called recursively until count is equal to the pattern
;           length or end of row/col direction being traversed.
; Assistance Received: none 
; ***********************************************************************
(defun patternHelper(board row col incrow inccol pattern positions count)

        ;; Pattern found! Return positions in pattern order by reversing list
  (cond ((= count (length pattern))
          (reverselist positions (length positions) ()))
          (t 
            (cond ( (and (>= row 0) (< row 19) (>= col 0) (< col 19))
                          ;; Position matches what is expected in pattern, add to list and increment count
                    (cond ((string-equal (getElementByPos board (list row col)) (char pattern count))
                              (patternHelper board (+ row incrow) (+ col inccol) incrow inccol pattern (cons (list row col) positions) (+ count 1))
                          )
                          ( t       ;; No match, next position
                            (cond (  (equal positions ()) 
                                    (patternHelper board (+ row incrow) (+ col inccol) incrow inccol pattern () 0))
                                  ( t 
                                  ;; Restart at position after the first in the current pattern positions (stored in reverse order)
                                  (patternHelper board (+ (first (first (reverselist positions (length positions) ()))) incrow) (+ (second (first  (reverselist positions (length positions) ()))) inccol) incrow inccol pattern () 0)
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
; Function Name: reverseList
; Purpose: reverse a list
; Parameters: 
;        mylist - list being reversed
;        mylistlength - length
;        reversedlist - list in reverse
; Return Value: reversed list
; Algorithm:       
;        1. While my list length in is not equal to zero, append to reversed list
;           the last element in my list.
;        2. Repeat recursively.
; Assistance Received: none 
; ***********************************************************************
(defun reverseList (mylist mylistlength reversedlist)
  (cond ((= mylistlength 0)
          reversedlist)
        ( t
        (reverseList mylist (- mylistlength 1) (append reversedlist (list (nth (- mylistlength 1) mylist))))
        )
  )
)

; ********************************************************************* 
; Function Name: maxCapture
; Purpose: find position on board that results in the maximum number of captures
; Parameters: 
;        board - current board state 
;        color - color being placed to check
;        row - current row being checked
;        col - current column being checked
;        positions - list of positions storing score, row, and col of capturing positions
; Return Value: row and column with highest score if capturing is possible, 
;                empty list otherwise
; Algorithm:       
;       1. Traverse the board for empty spaces.
;       2. On each empty space, place a stone, and check if it results in a capture.
;       3. If score is > 0, store the score it yields and the position, and pass to
;          recursive function call until end of board.
;       4. At end of board, returns empty list if there are no positions. Otherwise,
;          call maxPos to return the highest scoring row and column.
; Assistance Received: none 
; ***********************************************************************
(defun maxCapture(board color row col positions)
          ;; End of board
    (cond ((= row 19)
                  ;; No captures
            (cond ( (equal positions ())
                    () )
                  ;; Capturing possible, return row col with maximum # of captures
                  ( t 
                  (maxPos positions '(0 0 0)))
            )
          )
          ;; End of row, increment row, reset column
          ((= col 19)
            (maxCapture board color (+ row 1) 0 positions)
          )
          ( t
                  ;; Place stone to check for resulting captures
            (cond ((string-equal (getElementByPos board (list row col)) "O") 
                    (let ((score (second (checkCapture (boardAfterMove board row col color) color row col 0))))
                                  ;; Add capture count and position to positions list
                          (cond  ((> score 0)
                                  (maxCapture board color row (+ col 1) (cons (list score row col) positions))
                                  )
                                  ;; No capture at this position
                                  (t 
                                  (maxCapture board color row (+ col 1) positions)
                                  )
                          )
                          
                    )
                  )
                  ;; Board space not empty, keep traversing
                  (t 
                    (maxCapture board color row (+ col 1) positions)
                  )
            )
          )
  )
)

; ********************************************************************* 
; Function Name: find3
; Purpose: find clusters of 3 stones that can turn to 4, and later to 5
; Parameters: 
;        board - current board state 
;        color - color being checked
; Return Value: row and column of suggested position
; Algorithm:       
;       1. Set up various clusters of 3 patterns.
;       2. Check if they can be found on the board.
;       3. If so, return move that will turn cluster to 4.
;       4. If no patterns are found, return empty list.
; Assistance Received: none 
; ***********************************************************************
(defun find3(board color)
  (let* ((pattern1 (findPattern board (concatenate 'string "O" color color color "O") 0))
         (pattern2 (findPattern board (concatenate 'string "O" "O" color color color) 0))
         (pattern3 (findPattern board (concatenate 'string color color color "O" "O") 0))
         (pattern4 (findPattern board (concatenate 'string "O" color "O" color color) 0))
         (pattern5 (findPattern board (concatenate 'string color "O" color color "O") 0))
         (pattern6 (findPattern board (concatenate 'string color color "O" color "O") 0))
         (pattern7 (findPattern board (concatenate 'string "O" color color "O" color) 0))
         )

          (cond ((not (equal pattern1 () ) )
                    (first pattern1)) 
                ((not (equal pattern2 () ) )
                    (second pattern2)) 
                ((not (equal pattern3 () ) )
                    (fourth pattern3)) 
                ((not (equal pattern4 () ) )
                    (third pattern4)) 
                ((not (equal pattern5 () ) )
                    (second pattern5)) 
                ((not (equal pattern6 () ) )
                    (third pattern6)) 
                ((not (equal pattern7 () ) )
                    (fourth pattern7))     
                ( t
                () ) 
          )
  )
)

; ********************************************************************* 
; Function Name: find2
; Purpose: find clusters of 2 stones that can turn to 3
; Parameters: 
;        board - current board state 
;        color - color being checked 
; Return Value: row and column of suggested position
; Algorithm:       
;       1. Set up various clusters of 2 patterns.
;       2. Check if they can be found on the board.
;       3. If so, return move that will turn cluster to 3.
;       4. If no patterns are found, return empty list.
; Assistance Received: none 
; ***********************************************************************
(defun find2(board color)
  (let* ((pattern1 (findPattern board (concatenate 'string "O" color color "O") 0))
        (pattern2 (findPattern board (concatenate 'string "O" "O" color color) 0))
        (pattern3 (findPattern board (concatenate 'string color color "O" "O") 0))
        (pattern4 (findPattern board (concatenate 'string "O" color "O" color) 0))
        (pattern5 (findPattern board (concatenate 'string color "O" color "O") 0))
        )

        (cond ((not (equal pattern1 () ) )
                  (first pattern1)) 
              ((not (equal pattern2 () ) )
                  (second pattern2)) 
              ((not (equal pattern3 () ) )
                  (third pattern3)) 
              ((not (equal pattern4 () ) )
                  (third pattern4)) 
              ((not (equal pattern5 () ) )
                  (second pattern5))     
              ( t
              () ) 
        )
  )
)


; ********************************************************************* 
; Function Name: find1
; Purpose: find single stones near which another stone can be placed
;            while also avoiding being captured
; Parameters: 
;        board - current board state 
;        color - color being checked
; Return Value: row and column of suggested position
; Algorithm:       
;       1. If move count is 2, must place stone 3 intersections
;        away from center. Two options provided, if one is taken, return other.
;       2. Find patterns and suggest best move while avoiding being captured.
;           (i.e. OC*_ placing C stone at * can result in capture from opponent,
;                 so it is instead placed here OC_*)
; Assistance Received: none 
; ***********************************************************************
(defun find1(board color)
  (let* ((opponentcolor (getOpponentColor color))
        (movecount (getMoveCount board 0 0 0))
        (pattern1 (findPattern board (concatenate 'string opponentcolor color "O" "O") 0))
        (pattern2 (findPattern board (concatenate 'string "O" "O" color "O") 0))
        (pattern3 (findPattern board (concatenate 'string "O" color "O" "O") 0))
        (pattern4 (findPattern board (concatenate 'string "O" "O" color opponentcolor) 0))
        )

        (cond  ((= movecount 2)
                (cond ((string-equal (getElementByPos board '(5 9)) "O")
                        (list 6 9))
                      (t 
                      (list 12 9))
                )
              )
              ((not (equal pattern1 () ) )
                  (fourth pattern1)) 
              ((not (equal pattern2 () ) )
                  (second pattern2)) 
              ((not (equal pattern3 () ) )
                  (third pattern3)) 
              ((not (equal pattern4 () ) )
                  (first pattern4))  
              ( t
              () ) 
        )
  )
)

; ********************************************************************* 
; Function Name: randomMove
; Purpose: generate a random move
; Parameters: 
;        board - current board state 
;        color - color being placed
; Return Value: row and column of random position
; Algorithm:       
;       1. Generates a random row and column, checks if it is empty,
;           returns move if so.
;       2. If position not empty, call randomMove and try again.
; NOTE: This is added in the strategy so the computer can keep playing
;       if something malfunctions. However, there are enough strategy
;       methods that this should never be called. Purely a safety.
; Assistance Received: none 
; ***********************************************************************
(defun randomMove(board color)
  (let ( (row (random 19 (make-random-state t)))
         (column (random 19 (make-random-state t))))

        (cond ((string-equal (getElementByPos board (list row column)) "O")
              (list row column)
              )
              (t 
              (randomMove board color)
              )
        )
  )
)
