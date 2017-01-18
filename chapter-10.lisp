(defmacro while (test &rest body)
  `(do ()
      ((not ,test))
     ,@body))

(defun foo ()
  (let ((x 0))
    (while (< x 10)
      (princ x)
      (incf x))))

(defmacro ntimes-wrong (n &rest body)
  `(dotimes (i ,n)
    ,@body
    ))

(defmacro ntimes (n &rest body)
  (let ((i (gensym))
	(nn (gensym)))
    `(let ((,nn ,n))
      (dotimes (,i ,nn)
	,@body))))

;;; 2)
(defmacro our-if (test true-exp &optional false-exp)
  `(cond
    (,test ,true-exp)
    (t ,false-exp)))

;;; 3)
(defmacro nth-expr (n &rest exprs)
  "这个实现所有exprs都会被eval"
  (let ((gn (gensym)))
    `(let ((,gn ,n))
       (nth ,gn (list ,@exprs)))))


(defmacro nth-expr-a (n &body body)
  "这个是答案，这样的写法无关的expr不会执行"
  (if (integerp n)
      (nth n body)
    `(case ,n
       ,@(let ((i -1))
           (mapcar #'(lambda(x) `(,(incf i) ,x)) body)))))

(defun foo-ex ()
  "test function"
  (nth-expr-a (+ 1 1)
	    (progn
	      (format t "in 1:")
	      (+ 1 1))
	    (progn
	      (format t "in 2:")
	      (+ 1 2))
	    (progn
	      (format t "in 3:")
	      (+ 1 3))
	    ))

;;; 4)
(defmacro ntimes-re (n &body body)
  (let ((ng (gensym))
	(rec-fn (gensym)))
    `(let ((,ng ,n))
       (labels ((,rec-fn (tn)
		  (when (> tn 0)
		    ,@body
		    (,rec-fn (decf tn)))))
	(,rec-fn ,ng)))))

;;; 5)
(defmacro n-of (n expr)
  (let ((gn (gensym))
	(gi (gensym))
	(gr (gensym)))
    `(let ((,gn ,n)
	  (,gr nil))
       (dotimes (,gi ,gn)
	 (setf ,gr (append ,gr (list ,expr))))
       ,gr)))

;;; 8)
(defmacro my-double (x)
  (let ((nx (gensym)))
    `(let ((,nx ,x))
      (setf ,nx (* ,nx 2))
      )
    )
  )


