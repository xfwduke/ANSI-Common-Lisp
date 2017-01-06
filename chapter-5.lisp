;;; 1)
(defun foo-a (y)
  (let ((x (car y)))
    (cons x x)))

(defun foo-a-alter (y)
  ((lambda (x)
     (cons x x))
   (car y)))

(defun foo-b (x z)
  (let* ((w (car x))
	 (y (+ w z)))
    (cons w y)))

(defun foo-b-alter (p1 z)
  ((lambda (w)
     ((lambda (y)
	(cons w y))
      (+ w z)))
   (car p1)))
  
;;; 2)
(defun mystery (x y)
  (if (null y)
      nil
      (if (eql (car y) x)
	  0
	  (let ((z (mystery x (cdr y))))
	    (and z (+ z 1))))))

(defun mystery-alter (x y)
  (cond
    ((null y) nil)
    ((eql (car y) x) 0)
    (t (let ((z (mystery-alter x (cdr y))))
	 (and z (+ z 1))))))

;;; 3)
(defun square>5 (x)
  (if (> x 5)
      (* x x)
      x))

;;; 5)
(defun precedes-iter (x v)
  (let ((res nil))
    (dotimes (idx (length v) (remove-duplicates res))
      (when (and
	     (> idx 0)
	     (eql x (aref v idx))
	     (not (eql x (aref v (- idx 1)))))
	(push (aref v (- idx 1)) res)))))

(defun precedes-recu (x v)
  (cond
    ((< (length v) 2) nil)
    ((and (eql x (aref v 1))
	  (not (eql x (aref v 0))))
     (remove-duplicates
      (cons (aref v 0) (precedes-resc x (subseq v 2)))))
    (t (precedes-resc x (subseq v 1)))))

;;; 6)
(defun intersperse-iter (dot lst)
  (do*
   ((x lst (cdr x))
    (res (list (car x)) (nconc res (list dot (car x)))))
   ((null (cdr x)) res)))

(defun intersperse-recu (dot lst)
  (cond
    ((null (cdr lst)) lst)
    (t (nconc (list (car lst) dot) (intersperse-resc dot (cdr lst))))))

;;; 7)
(defun arithmetic-sequnce-recu-p (lst)
  (cond
    ((< (length lst) 2) nil)
    ((eql (length lst) 2) (eql (- (second lst) (first lst)) 1))
    (t (and (eql (- (second lst) (first lst)) 1)
	    (arithmetic-sequnce-recu-p (cdr lst))))))

(defun arithmetic-sequnce-do-p (lst)
  (unless (< (length lst) 2)
    (do ((x lst (cdr x)))
	((= (length x) 2) (eql (- (second x) (first x)) 1))
      (unless (eql (- (second lst) (first lst)) 1) (return nil)))))

(defun arithmetic-sequnce-mapc-p (lst)
  (let ((pre nil))
    (mapc #'(lambda (x)
	      (unless (or (null pre)
			  (eql (- x pre) 1))
		(return-from arithmetic-sequnce-mapc-p nil))
	      (setf pre x))
	  lst)
    t))

;;; 8)
(defun find-max-min (v)
  (cond
    ((eql (length v) 2)
     (if (> (svref v 0) (svref v 1))
	 (values (svref v 0) (svref v 1))
	 (values (svref v 1) (svref v 0))))
    (t
     (multiple-value-bind (_max _min) (find-max-min (subseq v 1))
       (values (max (svref v 0) _max) (min (svref v 0) _min))))))
