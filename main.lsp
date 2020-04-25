(setq board_size 9)
(setq color_black #\B)
(setq color_white #\W)
(setq point_empty #\-)
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

(defun is_move_ok (x y) 
  (COND
    ((not (equal (aref board x y) point_empty)) nil)
    ((not (equal (aref board x y) point_empty)) nil)
    ((or (> x board_size) (< x 0)) nil)
    ((or (> y board_size) (< y 0)) nil)
    (t t)))

(defun make_move (color)
  (COND
    ((equal color color_white) (format t "White move:~%"))
    ((equal color color_black) (format t "Black move:~%"))
  )

  (loop
    (setq x (read))
    (setq y (read))
    (if (is_move_ok x y) (return (place_stone color x y)))
    (format t "Enter the correct coords, please~%")))

(defun main()
  (format t "Type 0 0 (zero zero) to pass~%")
  (setq current_move 0)
  (setq current_color #\B)
  (setq passes 0)

  (loop
    (COND 
      ((< passes 2) 
       (make_move current_color)
       (setf current_move (1+ current_move))
       (setf current_color (color current_move))
       (print_board))
      ((= passes 2)(return t)))))

(main)
