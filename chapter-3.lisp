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
      (do-uncompress-r nil x)
      )
  )

(defun do-uncompress-r (res x)
  (if (null x)
      res
      (let ((head (car x)))
	(do-uncompress-r
	    (append res (make-list (car (cdr head)) :initial-element (car head)))
	  (cdr x))
	)
      )
  )

(defun build-neighbors-table (v)
  (let ((tbl nil) (scanned nil))
    ;; (dolist (cycle v)
    ;;   (dolist (node cycle)
    ;; 	(if (not (member node scanned))
    ;; 	    (progn
    ;; 	      (push node scanned)
    ;; 	      (setf (assoc node tbl) (cons (assoc node )))
    ;; 	      )
    ;; 	    )
    ;; 	)
    ;;   )
    )
  )

(defun build-neighbors-table-for-cycle (c)
  (let ((tbl nil))
  (mapcar
   #'(lambda (n)
       (cons n (push  (remove n c) (cdr (assoc n tbl) )))
       )
   c
   )
  ))
