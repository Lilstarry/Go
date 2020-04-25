(setq boardsize 9)
(setq board (make-array '(10 10)
                        :element-type 'character 
                        :initial-element #\- ))

(defun color (move)
  (COND
    ((= (mod move 2) 0) #\B)
    ((= (mod move 2) 1) #\W)))

(defun print_board () 
  (loop for i from 0 to (1- (array-dimension board 0)) do
    (loop for j from 0 to (1- (array-dimension board 1)) do
      (format t "~3:<~A~>~@[~&~]" (aref board i j) (eql (1- (array-dimension board 1)) j)))))

(defun place_stone (stone x y )
  (setf (aref board x y) stone))

(defun main()
  (setq current_move 0)
  (setq current_color #\B)
  (place_stone current_color 3 4)

  (setf current_move (1+ current_move))
  (setf current_color (color current_move))

  (place_stone current_color 4 3)
  (print_board)
)

(main)
