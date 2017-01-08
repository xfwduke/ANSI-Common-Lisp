(a b (c d) e)
(defun too (x)
  )
;; compress
(defun compress (lst)
  (if (null lst)
      nil
      (do-compress (car lst) 1 (cdr lst))))

(defun do-compress (head n left)
  (if (null left)
      (list head n)
      (if (eql head (car left))
  	  (do-compress head (+ n 1) (cdr left))
	  (cons (list head n)
		(do-compress (car left) 1 (cdr left))))))

;; uncompress
(defun uncompress-iter (x)
  (if (null x)
      nil
      (let ((res nil))
	(dolist (obj x)
	  (setf res
		(append
		 res
		 (make-list (car (cdr obj)) :initial-element (car obj)))))
	res)))

;; recursion
;; 本来想写个尾递归, 最终发现好像不是-_-
(defun uncompress-r (x)
  (if (null x)
      nil
      (do-uncompress-r nil x)))

(defun do-uncompress-r (res x)
  (if (null x)
      res
      (let ((head (car x)))
	(do-uncompress-r
	    (append res (make-list (car (cdr head)) :initial-element (car head)))
	  (cdr x)))))

(defun short-list (lists)
  (let ((r (first lists)))
    (dolist (obj (rest lists))
      (when (> (length r) (length obj))
	(setf r obj)))
    r))

(defun find-min (begin end v)
  (cond
    ((member end (assoc begin v)) (list begin end))
    ((null (assoc begin v)) nil)
    (t (let ((sp nil))
	 (setf sp
	       (short-list
		(remove nil
			(mapcar #'(lambda (nei)
				    (find-min nei end v))
				(rest (assoc begin v))))))
	 (if sp (cons begin sp))))))

;; 2)
(defun new-union (list1 list2)
  (dolist (obj list2 list1)
    (unless (member obj list1)
      (setf list1 (append list1 (list obj))))))

;; 3)
(defun occurrences-it (lst)
  (let ((res nil))
    (dolist (obj lst res)
      (if (assoc obj res)
	  (setf (cdr (assoc obj res))
		(1+ (cdr (assoc obj res))))
	  (setf res
		(cons (cons obj 1) res))))
    res))

(defun occurrences-helper (lst res)
  (cond
    ((null lst) res)
    (t
     (let ((obj (car lst)))
       (if (assoc obj res)
	   (setf
	    (cdr (assoc obj res))
	    (1+ (cdr (assoc obj res))))
	   (push (cons obj 1) res))
       (occurrences-helper (rest lst) res)))))

(defun occurrences-r (lst)
  (let ((res '((a . 0) (c . 0) (g . 0) (t . 0))))
    (occurrences-helper lst res)))

;; 5)
(defun pos+ (lst)
  (do ((idx 0 (incf idx)))
      ((= idx (length lst)) lst)
    (setf (nth idx lst) (+ idx (nth idx lst)))))

;; 8)
(defun showdots (lst)
  (showdots-helper lst (length lst)))

(defun showdots-helper (lst n)
  (cond
    ((null lst) (format t "~S" lst)
     (dotimes (i n)
       (format t ")")))
    ((atom lst) (format t "(~S" lst))
    (t
     (showdots-helper (car lst) n)
     (format t " . ")
     (showdots-helper (cdr lst) n))))

;; 答案的实现, 漂亮很多
(defun showdots-1 (lst)
  (format t "~A" (showdots-helper-1 lst)))

(defun showdots-helper-1 (lst)
  (if (atom lst)
      lst
      (format nil "(~A . ~A)"
	      (showdots-helper-1 (car lst))
	      (showdots-helper-1 (cdr lst)))))

