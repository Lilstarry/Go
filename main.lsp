(setq board_size 9)
(setq color_black #\B)
(setq color_white #\W)
(setq point_empty #\-)
(setq point_marker #\X)
(setq point_kill_marker #\K)

(setq score_b 0.0)
(setq score_w 6.5)
(setq current_move 0)
(setq last_pass_move -2)
(setq game_end nil)
(setq board (make-array '(10 10)
                        :element-type 'character 
                        :initial-element point_empty ))
(setq tmp_board (make-array '(10 10) :element-type 'character))

(defun color (move)
  (COND
    ((= (mod move 2) 0) color_black)
    ((= (mod move 2) 1) color_white)))

(defun inc_prisoners (color)
  (COND
    ((equal color color_black) (setf score_w (1+ score_w)))
    ((equal color color_white) (setf score_b (1+ score_b)))))

(defun print_board (brd) 
  (loop for i from 0 to (1- (array-dimension brd 0)) do
    (loop for j from 0 to (1- (array-dimension brd 1)) do
      (format t "~3:<~A~>~@[~&~]" (aref brd i j) (eql (1- (array-dimension brd 1)) j)))))

(defun place_stone (brd stone x y)
  (setf (aref brd x y) stone))
  
(defun prepare_board ()
  (loop for i from 1 to (1- (array-dimension board 0)) do
    (place_stone board (digit-char i) i 0)
    (place_stone board (digit-char i) 0 i)))

(defun pass () 
  (COND
    ((= last_pass_move (1- current_move)) (setf game_end t))
    (t (setf last_pass_move current_move))))

(defun update_tmp_board () 
  (loop for i from 0 to (1- (array-dimension tmp_board 0)) do
    (loop for j from 0 to (1- (array-dimension tmp_board 1)) do
      (setf (aref tmp_board i j) (aref board i j)))))

(defun is_position_ok (x y)
  (if (or (or (> x board_size) (< x 1))
          (or (> y board_size) (< y 1)))
      nil t))

(defun precheck_liberty (x y color)
  (COND
    ((not (is_position_ok x y)) nil)
    ((equal (aref tmp_board x y) point_empty) t)
    ((equal (aref tmp_board x y) color) t)
    (t nil)))

(defun arr_val_or_nil (arr x y)
  (COND ((is_position_ok x y) (aref arr x y)) )
)

(defun kill (x y color)
  (if (equal (aref tmp_board x y) color)
    (place_stone tmp_board point_kill_marker x y))

  (if (equal (arr_val_or_nil tmp_board x (1- y)) color) 
    (kill x (1- y) color))
  (if (equal (arr_val_or_nil tmp_board x (1+ y)) color)
    (kill x (1+ y) color))
  (if (equal (arr_val_or_nil tmp_board (1- x) y) color)
    (kill (1- x) y color))
  (if (equal (arr_val_or_nil tmp_board (1+ x) y) color)
    (kill (1+ x) y color))

  (when (equal (aref tmp_board x y) point_kill_marker)
    (inc_prisoners color)
    (place_stone tmp_board point_empty x y)
    (place_stone board point_empty x y)))

(defun try_to_kill_by (x y color)
  (defparameter en_col (color (1+ current_move)))
  (defparameter ym (1- y))
  (defparameter yp (1+ y))
  (defparameter xm (1- x))
  (defparameter xp (1+ x))
  (COND
    ((not (is_position_ok x ym)) nil)
    ((not (= (check_liberties en_col x ym nil) 0)) nil)
    (t (update_tmp_board)
      (setf (aref tmp_board x y) color) 
      (when (equal en_col (aref tmp_board x ym))
        (kill x ym en_col)
        (update_tmp_board)
        (setf (aref tmp_board x y) color))))
  (COND
    ((not (is_position_ok x yp)) nil)
    ((not (= (check_liberties en_col x yp nil) 0)) nil)
    (t (update_tmp_board)
      (setf (aref tmp_board x y) color) 
      (when (equal en_col (aref tmp_board x yp))
        (kill x yp en_col)
        (update_tmp_board)
        (setf (aref tmp_board x y) color))))
  (COND
    ((not (is_position_ok xm y)) nil)
    ((not (= (check_liberties en_col xm y nil) 0)) nil)
    (t (update_tmp_board)
      (setf (aref tmp_board x y) color) 
      (when (equal en_col (aref tmp_board xm y))
        (kill xm y en_col)
        (update_tmp_board)
        (setf (aref tmp_board x y) color))))
  (COND
    ((not (is_position_ok xp y)) nil)
    ((not (= (check_liberties en_col xp y nil) 0)) nil)
    (t (update_tmp_board)
      (setf (aref tmp_board x y) color) 
      (when (equal en_col (aref tmp_board xp y))
        (kill xp y en_col)
        (update_tmp_board)
        (setf (aref tmp_board x y) color))))
)

(defun check_liberties (color x y is_move)
  (when is_move
    (setf (aref tmp_board x y) color)
    (try_to_kill_by x y color))

  (if (equal (aref tmp_board x y) point_empty) (return-from check_liberties 1))
  (if (not (equal (aref tmp_board x y) color)) (return-from check_liberties 0))
  (setf (aref tmp_board x y) point_marker)
  (+ (if (precheck_liberty x (1- y) color)
         (check_liberties color x (1- y) nil)
         0)
     (if (precheck_liberty x (1+ y) color)
         (check_liberties color x (1+ y) nil)
         0)
     (if (precheck_liberty (1- x) y color)
         (check_liberties color (1- x) y nil)
         0)
     (if (precheck_liberty (1+ x) y color)
         (check_liberties color (1+ x) y nil)
         0)))

(defun is_move_ok (x y)
  (update_tmp_board)
  (COND
    ((and (= x 0) (= y 0)) t)
    ((not (is_position_ok x y)) nil)
    ((not (equal (aref board x y) point_empty)) nil)
    ((not (equal (aref board x y) point_empty)) nil)
    ((or (> x board_size) (< x 0)) nil)
    ((or (> y board_size) (< y 0)) nil)
    ((= (check_liberties (color current_move) x y t) 0) nil)
    (t t)))

(defun handle_move (color x y)
  (COND
    ((and (= x 0) (= y 0)) (pass))
    (t (place_stone board color x y))))

(defun make_move (color)
  (COND
    ((equal color color_white) (format t "White move:~%"))
    ((equal color color_black) (format t "Black move:~%")))
  (loop
    (setq x (read))
    (setq y (read))
    (if (is_move_ok x y) (return (handle_move color x y)))
    (format t "Enter the correct coords, please~%")))

(defun main()
  (format t "Type 0 0 (zero zero) to pass~%")
  (setq current_color color_black)
  (prepare_board)

  (loop
    (if game_end (return t))
    (make_move current_color)
    (setf current_move (1+ current_move))
    (setf current_color (color current_move))
    (print_board board)
    (format t "Points: W+~a, B+~a~%" score_w score_b)))
  
(main)
