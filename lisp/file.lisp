; ********************************************************************* 
; Function Name: openFromFile
; Purpose: read board, score, and next player data from file into local variables
;           and pass them to tournament
; Parameters: 
;        filename - name of file entered by user
; Return Value: none
; Algorithm: follow expected file format 
; Assistance Received: none 
; ***********************************************************************
(defun openFromFile (filename)
  (with-open-file (infile filename) 
      (read-line infile)
      (read-line infile)
      (read-line infile)
      (let ((board (boardFile 19 infile))
            (human (statsFile infile))
            (comp (statsFile infile))
            (nextplayer (statsFile infile))) 
            
            (tournament board human comp nextplayer filename))    
  )
)

; ********************************************************************* 
; Function Name: boardFile
; Purpose: read board from file
; Parameters: 
;        num - number of rows left (start with 19)
;        infile - file to be read from
; Return Value: none
; Algorithm: none
; Assistance Received: none 
; ***********************************************************************
(defun boardFile(num infile)
  (cond ((= num 0)
          (read-line infile)
              ())
        (   t
          (cons (read-from-string (read-line infile)) (boardFile (- num 1) infile)))
  )
)

; ********************************************************************* 
; Function Name: statsFile
; Purpose: read player data from file
; Parameters: 
;        infile - file to be read from
; Return Value: list of data elements, either captures and score (int) or
;               player and stone color (string)
; Algorithm: none
; Assistance Received: none 
; ***********************************************************************
(defun statsFile(infile)        
  (read-line infile)
  (read-line infile)
  (let* ( (values (string-trim '(#\Space #\Tab #\Newline) (read-line infile)))
          (val1 (subseq values 0 (- (findSpace values (length values)) 1) ))
          (val2 (subseq values (findSpace values (length values)) (length values)))
        )
              ;; Captures and score returned as integers
        (cond ((and (parse-integer val1 :junk-allowed t) (parse-integer val2 :junk-allowed t))
                  (list (parse-integer val1) (parse-integer val2)))
              ;; Otherwise, as string
              (  t 
                (list val1 val2))
        )
  )
)

; ********************************************************************* 
; Function Name: findSpace
; Purpose: find the space in a string that is separating two values we need
; Parameters: 
;        str - string
;        length - length of string
; Return Value: location of space in the string
; Algorithm: none
; Assistance Received: none 
; ***********************************************************************
(defun findSpace (str length)
  (cond ((string-equal (subseq str (- length 1) length) " ")
          length )
        ( (= length 1) 
                  )
        ( t 
          (findSpace str (- length 1)))
  )
)

; ********************************************************************* 
; Function Name: boardToFile
; Purpose: save board to file
; Parameters: 
;        board - board at current state of the game
;        filename - file this is being saved to 
;        count - loop counter
; Return Value: none
; Algorithm: follow expected file format
; Assistance Received: none 
; ***********************************************************************
(defun boardToFile(board filename count)
  (with-open-file (stream filename :direction :output  
                                    :if-exists :append :if-does-not-exist :create)
    (cond ( (= count 19)
            (close stream)
          )
          ( t
            (princ "     " stream)
            (princ (nth count board) stream)       
            (terpri stream) 
            (close stream)
            (boardToFile board filename (+ count 1))
          )
    )
  )
)

; ********************************************************************* 
; Function Name: statsToFile
; Purpose: save game stats to file
; Parameters: 
;        stats - either player score and captures or next player
;        filename - file this is being saved to 
; Return Value: none
; Algorithm: follow expected file format
; Assistance Received: none 
; ***********************************************************************
(defun statsToFile(stats filename)
  (with-open-file (stream filename :direction :output  
                                    :if-exists :append :if-does-not-exist :create)
    (terpri stream)
    (princ "    " stream)
    (princ (first stats) stream)
    (princ " " stream)
    (princ (second stats) stream)
    (terpri stream)
    (close stream)
  )
)

; ********************************************************************* 
; Function Name: saveToFile
; Purpose: save current game state to file
; Parameters: 
;        board - board at current state of the game
;        human - captured count and tournament score for human
;        comp - captured count and tournament score for computer
;        nextplayer - player and stone color of next to play
;        filename - file this is being saved to 
; Return Value: none
; Algorithm: follow expected file format
; Assistance Received: none 
; ***********************************************************************
(defun saveToFile(board human comp nextplayer filename)

  ;; Deleting file if it exists so it is not added to (storing more than one board, etc)
  (cond ((probe-file filename)
          (delete-file filename))
  )

  (with-open-file (stream filename :direction :output  
                                   :if-exists :append :if-does-not-exist :create)
    (princ "(" stream)
    (terpri stream)
    (princ "   ; Board:" stream)
    (terpri stream)
    (princ "   (" stream)
    (terpri stream)
    (close stream)
  )

  (boardToFile board filename 0)

  (with-open-file (stream filename :direction :output  
                                  :if-exists :append :if-does-not-exist :create)
    (princ "    )" stream)
    (terpri stream)
    (terpri stream)
    
    (princ "    ; Human" stream)
    (close stream)
  )
  (statsToFile human filename)

  (with-open-file (stream filename :direction :output  
                                  :if-exists :append :if-does-not-exist :create)
    (terpri stream)
    (princ "    ; Computer" stream)
    (close stream)
  )
  (statsToFile comp filename)

  (with-open-file (stream filename :direction :output  
                                  :if-exists :append :if-does-not-exist :create)
    (terpri stream)
    (princ "    ; Next player" stream)
    (close stream)
  )
  (statsToFile nextplayer filename)

  (with-open-file (stream filename :direction :output  
                                  :if-exists :append :if-does-not-exist :create)
    (format stream ")")
  )
)