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
