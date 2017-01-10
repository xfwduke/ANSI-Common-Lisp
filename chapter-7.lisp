(defun seek-token (file-path token alter-token)
  (let ((buffer (make-string (length token))) (token-idx 0))
    (with-open-file (f file-path :direction :input)
      (do ((c (read-char f nil 'eof) (read-char f nil 'eof)))
	  ((eql c 'eof))
	;; 每从文件读入一个字符, 就和token中token-idx指向的字符对比
	(if (char= c (char token token-idx))
	    (progn
	      ;; 相等则存入buffer
	      (setf (char buffer token-idx) c)
	      ;; 并让token-idx指向token的下一个字符
	      (incf token-idx)
	      ;; 确实找到了一个, 做替换输出
	      (when (= (length token) token-idx)
		(format t "~A" alter-token)
		;; 清空buffer
		(setf buffer (make-string (length token)))
		(setf token-idx 0)))
	    (progn
	      (if (> token-idx 0)
		  (progn
		    (unread-char c f)
		    (format t "~A" (char buffer 0)) ;buffer的第一个字符直接输出
		    ;; buffer中剩下的字符从尾到头送回stream
		    (do ((buffer-idx (- token-idx 1) (- buffer-idx 1)))
			((<= buffer-idx 0))
		      (unread-char (char buffer buffer-idx) f))
		    ;; 清空buffer
		    (setf buffer (make-string (length token)))
		    (setf token-idx 0))	
		  (format t "~A" c))))))))	;如果不相等, 而且buffer中没东西, 直接输出

;;; 1)
(defun file-line-to-list (file)
  (with-open-file (f file :direction :input)
    (do* ((line (read-line f nil 'eof) (read-line f nil 'eof))
	  (res (append nil (list line)) (append res (list line))))
	 ((eql line 'eof) res))))

;;; 2)
(defun file-exp-to-list (file)
  (with-open-file (f file :direction :input)
    (do* ((exp (read f nil 'eof) (read f nil 'eof))
	  (res (append nil (list exp)) (append res (list exp))))
	 ((eql exp 'eof) (remove 'eof res)))))

;;; 3)
(defun delete-comment (file-in file-out)
  (with-open-file (f-in file-in :direction :input)
    (with-open-file (f-out file-out :direction :output :if-exists :overwrite)
      (do ((line (read-line f-in nil 'eof) (read-line f-in nil 'eof)))
	  ((eql line 'eof))
	(format f-out "~A~%" (subseq line 0 (position #\% line)))))))

;;; 4)
(defun print-matrix (matrix)
  (let* ((dim (array-dimensions matrix))
  	 (m (first dim))
  	 (n (second dim)))
    (dotimes (mi m)
      (dotimes (ni n)
	(format t "~10,2F" (aref matrix mi ni)))
      (format t "~%"))))

;;; 5)
(defun seek-token-wild-+ (file-path token alter-token)
  (let ((buffer (make-string (length token))) (token-idx 0))
    (with-open-file (f file-path :direction :input)
      (do ((c (read-char f nil 'eof) (read-char f nil 'eof)))
	  ((eql c 'eof))
	;; 每从文件读入一个字符, 就和token中token-idx指向的字符对比
	(if (or (char= c (char token token-idx))
		(char= #\+ (char token token-idx))
		)
	    (progn
	      ;; 相等则存入buffer
	      (setf (char buffer token-idx) c)
	      ;; 并让token-idx指向token的下一个字符
	      (incf token-idx)
	      ;; 确实找到了一个, 做替换输出
	      (when (= (length token) token-idx)
		(format t "~A" alter-token)
		;; 清空buffer
		(setf buffer (make-string (length token)))
		(setf token-idx 0)))
	    (progn
	      (if (> token-idx 0)
		  (progn
		    (unread-char c f)
		    (format t "~A" (char buffer 0)) ;buffer的第一个字符直接输出
		    ;; buffer中剩下的字符从尾到头送回stream
		    (do ((buffer-idx (- token-idx 1) (- buffer-idx 1)))
			((<= buffer-idx 0))
		      (unread-char (char buffer buffer-idx) f))
		    ;; 清空buffer
		    (setf buffer (make-string (length token)))
		    (setf token-idx 0))	
		  (format t "~A" c))))))))	;如果不相等, 而且buffer中没东西, 直接输出
