;*************************************************************
;* Name:  Mariya Popova                                      *
;* Project:  2 Pente LISP                                    *
;* Class:  CMPS 366-01 Organization of Programming Languages *
;* Date:  October 18, 2023                                   *
;*************************************************************

; ********************************************************************* 
; Function Name: loadFiles
; Purpose: files to be loaded and included
; Parameters: none
; Assistance Received: none 
; ***********************************************************************
(defun loadFiles()
  (load "board.lisp")
  (load "player.lisp")
  (load "round.lisp")
  (load "tournament.lisp")
  (load "file.lisp")
)

; ********************************************************************* 
; Function Name: pente
; Purpose: carry out Pente game!
; Parameters: none
; Assistance Received: none 
; ***********************************************************************
(defun pente()
  (loadFiles)
  (princ "Welcome to Pente")
  (terpri)
  (princ "Load existing game? Enter 'Y' or 'N': ")
  (terpri)
  (start)
)

; ********************************************************************* 
; Function Name: start
; Purpose: prompt user if they want to start or resume a game,
;          get the filename, and start/resume tournament
; Parameters: none
; Assistance Received: none 
; ***********************************************************************
(defun start ()
  (let ((input (read-line)))
            ;; No input
    (cond   ( (string-equal input "")
              (start)
            )
            ;; Resume game
            ( (and (char-equal (char input 0) #\Y) (= (length input) 1))
              (let ((filename (getfilename)))
                (cond ( (probe-file filename)
                        (openFromFile filename)
                      )
                      (t 
                        (format t "File not found. Starting a new game. ~%")
                        (tournament (boardInit 19) '(0 0) '(0 0) '("" "") filename)
                      )
                      
                )            
              )
            )
            ;; New game
            ( (and (char-equal (char input 0) #\N) (= (length input) 1))
              (princ "Starting a new game. Enter filename to save game.")
              (terpri)
              (tournament (boardInit 19) '(0 0) '(0 0) '("" "") (getfilename))
            )
            ;; No input
            ( (char-equal (char input 0) #\Newline)
              (start)                 
            )
            ;; Invalid input
            ( t  
              (princ "Please enter valid input 'Y' or 'N'.")
              (terpri)
              (start)
            )
    )
  )
)

; ********************************************************************* 
; Function Name: getfilename
; Purpose: get a valid filename so program doesn't crash when saving
; Parameters: none
; Return Value: file name
; Algorithm: none
; Assistance Received: none 
; ***********************************************************************
(defun getfilename()
  (princ "Enter filename:")
  (terpri)
  (let ((input (read-line)))
    (cond ((or (string-equal input "")(char-equal (char input 0) #\Newline))
           (getfilename))
          (t
           input )
    )
  )
)

(pente)