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
		  (lambda (n s) (format s "#<~A>" (node-elt n)))))
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

