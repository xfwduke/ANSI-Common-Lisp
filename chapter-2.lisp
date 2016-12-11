;; 7)
(defun my-nest-listp (x)
  (if (null x)
      nil
      (or
       (listp (car x))
       (my-nest-listp (cdr x)))))

;; 8-a)
(defun print-dot (n)
  (do
   ((i 1 (+ i 1)))
   ((> i n) (format t "~%"))
    (format t ".")))

(defun print-dot-r (n)
  (if (eql n 0)
      (format t "~%")
      (progn
	(format t ".")
	(print-dot-r (- n 1)))))

;; 8-b)
(defun count-element (v lst)
  (let ((c 0))
    (dolist (i lst)
      (if (eql i v)
	  (setf c (+ c 1))))
    c))

(defun count-element-r (v lst)
  (if (null lst)
      0
      (if (not (eql v (car lst)))
	  (count-element-r v (cdr lst))
	  (+ 1 (count-element-r v (cdr lst))))))


;; 9-a)
(defun sum-ignore-nil-use-remove (lst)
  (apply #'+ (remove nil lst))
  )

;; 9-b)
(defun sum-ignore-nil (lst)
  (if (null lst)
      0
      (let ((x (car lst)))
	(if (null x)
	    (sum-ignore-nil (cdr lst))
	    (+ x (sum-ignore-nil (cdr lst)))))))
