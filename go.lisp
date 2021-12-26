(defparameter *board-size* 9)
(defparameter *color-black* #\B)
(defparameter *color-white* #\W)
(defparameter *point-empty* #\-)
(defparameter *point-marker* #\X)
(defparameter *point-kill-marker* #\K)

(defparameter *score-b* 0.0)
(defparameter *score-w* 6.5)
(defparameter *current-move* 0)
(defparameter *last-pass-move* -2)
(defparameter *game-end* nil)
(defparameter *board* (make-array '(10 10)
                                :element-type 'character 
                                :initial-element *point-empty*))
(defparameter *tmp-board* (make-array '(10 10) :element-type 'character))

(defun color (move)
  (cond
    ((= (mod move 2) 0) *color-black*)
    ((= (mod move 2) 1) *color-white*)))

(defun inc-prisoners (color)
  (cond
    ((equal color *color-black*) (setf *score-w* (1+ *score-w*)))
    ((equal color *color-white*) (setf *score-b* (1+ *score-b*)))))

(defun print-board (board) 
  (loop for i from 0 to (1- (array-dimension board 0)) do
        (loop for j from 0 to (1- (array-dimension board 1)) do
              (format t "~3:<~A~>~@[~&~]" (aref board i j) (eql (1- (array-dimension board 1)) j)))))

(defun place-stone (board stone x y)
  (setf (aref board x y) stone))

(defun prepare-board ()
  (loop for i from 1 to (1- (array-dimension *board* 0)) do
        (place-stone *board* (digit-char i) i 0)
        (place-stone *board* (digit-char i) 0 i)))

(defun pass () 
  (cond
    ((= *last-pass-move* (1- *current-move*)) (setf *game-end* t))
    (t (setf *last-pass-move* *current-move*))))

(defun update-tmp-board () 
  (loop for i from 0 to (1- (array-dimension *tmp-board* 0)) do
        (loop for j from 0 to (1- (array-dimension *tmp-board* 1)) do
              (setf (aref *tmp-board* i j) (aref *board* i j)))))

(defun is-position-ok (x y)
  (if (or (or (> x *board-size*) (< x 1))
          (or (> y *board-size*) (< y 1)))
    nil t))

(defun precheck-liberty (x y color)
  (cond
    ((not (is-position-ok x y)) nil)
    ((equal (aref *tmp-board* x y) *point-empty*) t)
    ((equal (aref *tmp-board* x y) color) t)
    (t nil)))

(defun arr-val-or-nil (arr x y)
  (cond ((is-position-ok x y) (aref arr x y))))

(defun kill (x y color)
  (if (equal (aref *tmp-board* x y) color)
    (place-stone *tmp-board* *point-kill-marker* x y))
  (if (equal (arr-val-or-nil *tmp-board* x (1- y)) color) 
    (kill x (1- y) color))
  (if (equal (arr-val-or-nil *tmp-board* x (1+ y)) color)
    (kill x (1+ y) color))
  (if (equal (arr-val-or-nil *tmp-board* (1- x) y) color)
    (kill (1- x) y color))
  (if (equal (arr-val-or-nil *tmp-board* (1+ x) y) color)
    (kill (1+ x) y color))
  (when (equal (aref *tmp-board* x y) *point-kill-marker*)
    (inc-prisoners color)
    (place-stone *tmp-board* *point-empty* x y)
    (place-stone *board* *point-empty* x y)))

(defun try-to-kill-by (x y color)
  (let ((en-col (color (1+ *current-move*)))
        (ym (1- y))
        (yp (1+ y))
        (xm (1- x))
        (xp (1+ x)))
    (cond
      ((not (is-position-ok x ym)) nil)
      ((not (= (check-liberties en-col x ym nil) 0)) nil)
      (t (update-tmp-board)
         (setf (aref *tmp-board* x y) color) 
         (when (equal en-col (aref *tmp-board* x ym))
           (kill x ym en-col)
           (update-tmp-board)
           (setf (aref *tmp-board* x y) color))))
    (cond
      ((not (is-position-ok x yp)) nil)
      ((not (= (check-liberties en-col x yp nil) 0)) nil)
      (t (update-tmp-board)
         (setf (aref *tmp-board* x y) color) 
         (when (equal en-col (aref *tmp-board* x yp))
           (kill x yp en-col)
           (update-tmp-board)
           (setf (aref *tmp-board* x y) color))))
    (cond
      ((not (is-position-ok xm y)) nil)
      ((not (= (check-liberties en-col xm y nil) 0)) nil)
      (t (update-tmp-board)
         (setf (aref *tmp-board* x y) color) 
         (when (equal en-col (aref *tmp-board* xm y))
           (kill xm y en-col)
           (update-tmp-board)
           (setf (aref *tmp-board* x y) color))))
    (cond
      ((not (is-position-ok xp y)) nil)
      ((not (= (check-liberties en-col xp y nil) 0)) nil)
      (t (update-tmp-board)
         (setf (aref *tmp-board* x y) color) 
         (when (equal en-col (aref *tmp-board* xp y))
           (kill xp y en-col)
           (update-tmp-board)
           (setf (aref *tmp-board* x y) color))))))

(defun check-liberties (color x y is-move)
  (when is-move
    (setf (aref *tmp-board* x y) color)
    (try-to-kill-by x y color))
  (if (equal (aref *tmp-board* x y) *point-empty*) (return-from check-liberties 1))
  (if (not (equal (aref *tmp-board* x y) color)) (return-from check-liberties 0))
  (setf (aref *tmp-board* x y) *point-marker*)
  (+ (if (precheck-liberty x (1- y) color)
       (check-liberties color x (1- y) nil)
       0)
     (if (precheck-liberty x (1+ y) color)
       (check-liberties color x (1+ y) nil)
       0)
     (if (precheck-liberty (1- x) y color)
       (check-liberties color (1- x) y nil)
       0)
     (if (precheck-liberty (1+ x) y color)
       (check-liberties color (1+ x) y nil)
       0)))

(defun is-move-ok (x y)
  (update-tmp-board)
  (cond
    ((and (= x 0) (= y 0)) t)
    ((not (is-position-ok x y)) nil)
    ((not (equal (aref *board* x y) *point-empty*)) nil)
    ((not (equal (aref *board* x y) *point-empty*)) nil)
    ((or (> x *board-size*) (< x 0)) nil)
    ((or (> y *board-size*) (< y 0)) nil)
    ((= (check-liberties (color *current-move*) x y t) 0) nil)
    (t t)))

(defun handle-move (color x y)
  (cond
    ((and (= x 0) (= y 0)) (pass))
    (t (place-stone *board* color x y))))

(defun make-move (color)
  (let (x y)
    (cond
      ((equal color *color-white*) (format t "White move:~%"))
      ((equal color *color-black*) (format t "Black move:~%")))
    (loop
      (setq x (read))
      (setq y (read))
      (if (is-move-ok x y) (return (handle-move color x y)))
      (format t "Enter the correct coords, please~%"))))

(defun main ()
  (format t "Type 0 0 (zero zero) to pass~%")
  (let ((current-color *color-black*))
    (prepare-board)
    (loop
      (if *game-end* (return t))
      (make-move current-color)
      (setf *current-move* (1+ *current-move*))
      (setf current-color (color *current-move*))
      (print-board *board*)
      (format t "Points: W+~a, B+~a~%" *score-w* *score-b*))))
  
(main)
