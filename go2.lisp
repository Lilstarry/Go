;;;; go2.lisp
;;; THE ULTIMATE GO ENGINE
;;; version 2

;; From https://stackoverflow.com/a/26061176
(defun copy-hash-table (hash-table)
  (let ((ht (make-hash-table 
              :test (hash-table-test hash-table)
              :rehash-size (hash-table-rehash-size hash-table)
              :rehash-threshold (hash-table-rehash-threshold hash-table)
              :size (hash-table-size hash-table))))
    (loop for key being each hash-key of hash-table
          using (hash-value value)
          do (setf (gethash key ht) value)
          finally (return ht))))

;; Yeah, try to make 4D go)
;; Hint: probably it can be done by changing `*dimensions*', `neighbour-modifiers' and `print-board' *only*
(defparameter *dimensions* 2)
;(defmacro sides-on-board ()
;  (* *dimensions* 2))

;; right -> top -> left -> bottom
(defconstant +neighbour-modifiers+ (list (lambda (c) (list (1+ (first c)) (second c)))
                                         (lambda (c) (list (first c) (1- (second c))))
                                         (lambda (c) (list (1- (first c)) (second c)))
                                         (lambda (c) (list (first c) (1+ (second c))))))

;; Change if you want negative coordinates or anything else
(defun valid-coordinates? (coordinates &optional board)
  (declare (ignore board))
  (loop for component in coordinates
        always (> component 0)))

(defgeneric add (one another)
  (:documentation "Generic addition of two entities"))

(defgeneric copy (entity)
  (:documentation "Self-evident"))

(defclass stone-group ()
  ((stones :initarg :stones 
           :accessor stones)
   (color :initarg :color
          :accessor color))
  (:documentation "Represents stone group of certain color"))

(defmethod add ((one stone-group) (another stone-group))
  "Concatenate two stone-groups"
  (if (eq (color one) (color another))
    (make-instance 'stone-group
                   :stones (remove-duplicates 
                             (append (stones one)
                                     (stones another))
                             :test #'equal)
                   :color (color one))
    nil))

(defclass board ()
  ((board-size :initarg :board-size
               :reader board-size)
   (field :initform (make-hash-table :test #'equal)
          :accessor field))
  (:documentation "This class represents a board of go game"))

(defmethod copy ((old-board board))
  (let ((new-board (make-instance 'board :board-size (board-size old-board))))
    (setf (field new-board) (copy-hash-table (field old-board)))
    new-board))

(defun liberties (group board)
  (remove-duplicates
    (reduce #'append
            (loop for coordinates in (stones group)
                  collecting (let ((neighbours (remove-if-not #'valid-coordinates?
                                                              (map 'list (lambda (f)
                                                                           (funcall f coordinates))
                                                                   +neighbour-modifiers+)))
                                   (field (field board)))
                               (loop for neighbour in neighbours
                                     if (null (nth-value 1 (gethash neighbour field)))
                                     collect neighbour))))))

(defun join-groups (board &rest groups)
  (and (reduce #'eq (map 'list #'color groups))
       (let ((new-group (reduce #'add groups)))
         (loop for stone in (stones new-group) do
               (setf (gethash stone (field board)) new-group))
         new-group)))

(defun kill-groups (board &rest groups)
  (loop for group in groups do
    (loop for stone in (stones group) do
          (remhash stone (field board))))
  groups)

;; returns operation-status and removed-groups
(defun place-stone (board coordinates stone-color)
  (cond ((null coordinates) (values nil t))
        ((nth-value 1 (gethash coordinates (field board))) (values nil nil))
        (:else (flet ((same-color (another-group) (eq stone-color (color another-group))))
                 (let* ((field (field board))
                        (new-group (make-instance 'stone-group
                                                  :stones (list coordinates)
                                                  :color stone-color))
                        (neighbour-places (remove-if-not #'valid-coordinates?
                                                         (loop for modifier in +neighbour-modifiers+
                                                               collect (funcall modifier coordinates))))
                        (neighbour-groups (remove-if-not #'identity
                                                         (loop for place in neighbour-places
                                                               collect (gethash place field))))
                        (liberties-in-place (- (length neighbour-places) (length neighbour-groups)))
                        (unique-neighbour-groups (remove-duplicates neighbour-groups :test #'equal))
                        (same-color-neighbours (remove-if-not #'same-color unique-neighbour-groups))
                        (opposite-color-neighbours (remove-if #'same-color unique-neighbour-groups)))
                   (cond ((or (/= liberties-in-place 0)
                              ;; If this place have no liberties, then either...
                              ;; One of enemy groups should be eaten
                              (remove-if (lambda (group)
                                           (remove coordinates (liberties group board) :test #'equal))
                                         opposite-color-neighbours)
                              ;; One of friendly groups should have liberties beside this place
                              (remove-if-not (lambda (group)
                                               (remove coordinates (liberties group board) :test #'equal))
                                             same-color-neighbours))
                          (apply #'join-groups (append (list board) same-color-neighbours (list new-group)))
                          (values 
                            (apply #'kill-groups (append (list board)
                                                         (remove-if (lambda (group)
                                                                      (liberties group board))
                                                                    opposite-color-neighbours))) t))
                         (:else (values nil nil))))))))

(defun print-board (board &optional (symbolic-representation (lambda (sym) (cond ((eq sym nil) #\.)
                                                                                 ((eq sym :black) #\B)
                                                                                 ((eq sym :white) #\W)))))
  (flet ((get-color (entity) (and entity (color entity))))
    (let ((field (field board)))
      (format t "~{~{~3:<~A~>~}~&~}"
              (append (list (append (list #\Space) (loop for i from 1 to (board-size board)
                                                     collect i)))
                      (loop for y from 1 to (board-size board)
                            collect (append (list y)
                                            (map 'list symbolic-representation
                                                 (loop for x from 1 to (board-size board)
                                                       collect (get-color (gethash (list y x) field)))))))))))

(defclass move ()
  ((board :initarg :board
          :reader board)
   (color :initarg :color
          :reader color)
   (stone :initarg :stone
          :reader stone)
   (removed-groups :initarg :removed-groups
                   :reader removed-groups))
  (:default-initargs :removed-groups nil))

(defclass game ()
  ((board :initarg :board
          :reader board)
   (moves :initform (list)
          :reader moves)))

;; And multiple colored go is a go too
;; But will require more changes
(defconstant +player-colors+ (list :black :white))
(defun next-player (game)
  (or (and (= (mod (length (moves game)) 2) 0)
           :black)
      :white))

(defconstant +initial-game-score+ (list :black 0.0 :white 6.5))

(defun calculate-score (game)
  (reduce (lambda (x y)
            (reduce #'append
                    (loop for color in +player-colors+
                          collect (list color (+ (or (getf x color) 0)
                                                 (or (getf y color) 0))))))
          (append (list +initial-game-score+)
                  (loop for move in (moves game)
                        collect (list (color move) (loop for group in (removed-groups move)
                                                         summing (length (stones group))))))))

;; returns board from last move if succeed
;; nil if failed
(defun undo-move (game)
  (with-slots (board moves) game
    (and moves
         (let ((last-move (pop moves)))
           (setf board (board last-move)))
         t)))

;; Reinvented wheel. Use cl-utilities instead.
;; https://www.cliki.net/cl-utilities
;; This code was stolen from https://stackoverflow.com/a/15394085
(defun split (string &key (delimiter? (lambda (c) (char= c #\Space))))
  (loop :for beg = (position-if-not delimiter? string)
        :then (position-if-not delimiter? string :start (1+ end))
        :for end = (and beg (position-if delimiter? string :start beg))
        :when beg :collect (subseq string beg end)
        :while end))

(defun input-command ()
  (let ((line (string-downcase (string-trim " \t" (read-line)))))
    (cond ((string= line "/undo")
           :undo)
          ((string= line "/skip")
           :skip)
          (:else (let ((terms (map 'list
                                   (lambda (term)
                                     (multiple-value-bind (num used-chars) (parse-integer term :junk-allowed t)
                                       (and (= used-chars (length term))
                                            num)))
                                   (split line))))
                   (and (= (length terms) 2)
                        (integerp (first terms))
                        (integerp (second terms))
                        terms))))))

(defun make-move (game color coordinates)
  (with-slots (moves board) game
    (let ((copied-board (copy board)))
      (multiple-value-bind (removed-groups move-status) (place-stone copied-board coordinates color)
        (when move-status
          (setf board copied-board)
          (push (make-instance 'move :board board :color color :stone coordinates :removed-groups removed-groups) moves))))))

(defun who-skipped (game)
  (loop for move in (moves game)
        while (eq (stone move) :skip)
        collect (color move)))

(defun skip-move (game)
  (with-slots (board moves) game
    (push (make-instance 'move :board board :color (next-player game) :stone :skip) moves)))

(defun print-help ()
  (format t "Enter \"<row> <column>\" to move~&      \"/skip\" to skip a move~&      \"/undo\" to undo move~&"))

(defun print-prompt (player-color)
  (format t "~A move: " (string-capitalize player-color))
  (finish-output))

(defun print-error ()
  (format t "This is not a valid move. Try again.~&"))

(defun print-score (score)
  (format t "Points: ~{~A+~A~#[~:;, ~]~}~&" score))

(defun print-separator ()
  (format t "~%"))

(defun play-match (board-size)
  (print-help)
  (let ((game (make-instance 'game :board (make-instance 'board :board-size board-size))))
    (handler-case
      (loop named mainloop do
            (let* ((player-color (next-player game))
                   (move ((lambda ()
                            (print-prompt player-color)
                            (input-command))))
                   (move-result (cond ((null move) nil)
                                      ((eq move :undo) (undo-move game))
                                      ((eq move :skip) (skip-move game))
                                      (:else (make-move game player-color move)))))
              (cond ((null move-result) (print-error))
                    (:else (print-board (board game))
                           (print-score (calculate-score game))
                           (print-separator)))
              (when (null (set-difference +player-colors+ (who-skipped game)))
                (return-from mainloop (values (board game) (calculate-score game))))))
      (end-of-file () (values (board game) (calculate-score game))))))
