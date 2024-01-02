;;;;;;;;;;;;;;;;;;;;; COLORS AND PRINTING ;;;;;;;;;;;;;;;;;;;;;

(defun black-on-white ()
    ;(format T "~a[30m~a[107m" (code-char 27) (code-char 27)))
    (format T "~a[96m~a[100m" (code-char 27) (code-char 27)))

(defun white-on-black ()
    ;(format T "~a[40m~a[97m" (code-char 27) (code-char 27)))
    (format T "~a[96m~a[40m" (code-char 27) (code-char 27)))
    
(defun reset-console ()
    (format T "~a[49m" (code-char 27)))
    
(defun print-queen ()                   ; If you have trouble printing the Unicode Queen character..
    ;(format T " Q "))                  ; <--- Uncomment this line
    (format T " ~a " (code-char 9819))) ; <--- and then comment out this line

(defun print-space ()
    (format T "   "))
    
;;;;;;;;;;;;;;;;;;;;; DRAW BOARD ;;;;;;;;;;;;;;;;;;;;;

(defun draw-board (board-size board)
    (loop for y from 0 to (1- board-size) do
        (loop for x from 0 to (1- board-size) do
            (if (equal (mod (+ x y) 2) 0)
                (black-on-white)         
                (white-on-black))
            (if (aref board x y)
                (print-queen)
                (print-space)))
        (reset-console)
        (format T "~%")))

;;;;;;;;;;;;;;;;;;;;; VALID QUEEN ;;;;;;;;;;;;;;;;;;;;;

(defun valid-queen (col row board-size board)
    ; Check the col
    (loop for y from 0 to (1- board-size) do
        (if (aref board col y)
            (return-from valid-queen nil)))
    
    ; Check the row
    (loop for x from 0 to (1- board-size) do
        (if (aref board x row)
            (return-from valid-queen nil)))
    
    ; Check diagonal
    (loop for x from 0 to (1- board-size) do    
        (loop for y from 0 to (1- board-size) do
            (if (or (equal (+ x y) (+ row col)) (equal (- x y) (- col row)))
                (if (aref board x y)
                    (return-from valid-queen nil)))))
    
    (return-from valid-queen T))

;;;;;;;;;;;;;;;;;;;;; FIND QUEENS ;;;;;;;;;;;;;;;;;;;;;

(defun find-queens (col board-size board)
    (if (>= col board-size)
        (progn 
            (draw-board board-size board)
            (reset-console)
            (format T "Press <ENTER> to continue searching... ~%")
            (read-char)            
            (return-from find-queens T)))
    
    (loop for row from 0 to (1- board-size) do
        (if (valid-queen col row board-size board)
            (progn 
                (setf (aref board col row) T)
                (find-queens (1+ col) board-size board)
                (setf (aref board col row) nil))))
    
    (return-from find-queens nil))

;;;;;;;;;;;;;;;;;;;;; MAIN PROGRAM ;;;;;;;;;;;;;;;;;;;;;

(reset-console)
(format T "What size board? ")    
(defvar board-size (read))

(setq board (make-array (list board-size board-size)))
(find-queens 0 board-size board)

(reset-console)