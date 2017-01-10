(defun philosoph (thing &optional (property 'funny))
  (list thing 'is property))

(defun primo (lst)
  (car lst))

(defun (setf primo) (val lst)
  (setf (car lst) val))

;;; 3)
(defun count-args (&rest args)
  (length args))

;;; 6)
(defun make-history-bigest ()
  (let ((b nil))
    #'(lambda (x)
	(if (or (null b)
	     (> x b))
	    (setf b x))
	b)))
