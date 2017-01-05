(defun mirror? (seq)
  (let ((half-len 0))
    (if (oddp (length seq))
	(setq half-len (/ (- (length seq) 1) 2))
	(setq half-len (/ (length seq) 2)))
    (dotimes (idx half-len t)
      (when (not (equal (elt seq idx)
			(elt seq (- (- (length seq) 1) idx))))
	(return-from mirror? nil)))))


(defun mirror?-book (seq)
  (let ((len (length seq)))
    (and (evenp len)
	 (do
	  ((forward 0 (+ forward 1))
	   (backward (- len 1) (- backward 1)))
	  ((or (> forward backward)
	       (not (equal (elt seq forward) (elt seq backward))))
	   (> forward backward))))))

(defun generate-seq (n)
  (let ((i 0))
    (mapcar #'(lambda (x)
		(setf x (incf i)))
	    (make-array  n))))

(defstruct (node (:print-object
		  (lambda (n s)
		    (format s "#<elt:~A>" (node-elt n)))))
  elt
  (left nil)
  (right nil))

;;; 这个二叉树生成方式好有意思
;;; 如果 obj < elt
;;;     则新建一个 node.elt == elt 的节点
;;;     把 obj insert 到新节点的 left
;;;     新节点的 right 保持原样
;;; 如果 obj > elt, 过程和上面类似
;;; 这是一个递归过程, 当前的节点总是被新建
;;; 而其他版本的二叉树生成算法是真的遍历已有的树
;;; 直到找到一个 left/right == nil 的合适节点时
;;; 用 obj 新建一个节点, 再添加上去

(defun bst-insert (obj bst)
  (if (null bst)
      (make-node :elt obj)
      (let ((elt (node-elt bst)))
	(if (eql obj elt)
	    bst
	    (if (< obj elt)
		(make-node
		 :elt elt
		 :left (bst-insert obj (node-left bst))
		 :right (node-right bst))
		(make-node
		 :elt elt
		 :left (node-left bst)
		 :right (bst-insert obj (node-right bst))))))))


(defun bst-insert-general (obj bst)
  (if (null bst)
      (make-node :elt obj)
      (let ((elt (node-elt bst)))
	(if (eql obj elt)
	    bst
	    (if (< obj elt)
		(if (null (node-left bst))
		    (setf (node-left bst) (make-node :elt obj))
		    (bst-insert-general obj (node-left bst)))
		(if (null (node-right bst))
		    (setf (node-right bst) (make-node :elt obj))
		    (bst-insert-general obj (node-right bst))))))))

(defun bst-find (obj bst)
  (cond
    ((null bst) nil)
    ((eql obj (node-elt bst)) bst)
    ((< obj (node-elt bst)) (bst-find obj (node-left bst)))
    (t (bst-find obj (node-right bst)))))

(defun bst-min (bst)
  (and bst
       (or (bst-min (node-left bst)) bst)))

(defun bst-min (bst)
  (and bst
       (or (bst-min (node-right bst)) bst)))

(defun bst-tranverse (fn bst)
  (cond
    ((null bst) nil)
    (t
     (bst-tranverse fn (node-left bst))
     (funcall fn bst)
     (bst-tranverse fn (node-right bst)))))

;;; 1)
(defun quarter-turn (mt)
  (let* ((dim (array-dimension mt 0))
	 (res (make-array (list dim dim))))
    (dotimes (m dim)
      (dotimes (n dim)
	(setf (aref res m n) (aref mt (- dim (+ n 1)) m))))
   res))

;;; 2)
(defun my-copy-list (lst)
  (reduce #'(lambda (a b)
	      (append a (list b)))
	  lst
	  :initial-value nil))

(defun my-reserve (lst)
  (reduce #'(lambda (a b)
	      (push b a))
	  lst
	  :initial-value nil))

;;; 3)
(defstruct (tri-node (:print-object
		      (lambda (o s)
			(format s "#<~A>" (tri-node-elt o)))))
  elt
  (left nil)
  (middle nil)
  (right nil))
