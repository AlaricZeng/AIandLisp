(defun subtree_leq (item lst)
	(cond ((null lst) ())
		  ((and (null (cadr lst)) (null (caddr lst)) (> item (car lst))) (list (car lst))) 
		  (t (cond ((>= item (car lst)) (append (append (subtree_leq item (cadr lst)) (list (car lst))) (subtree_leq item (caddr lst))))
		  		   (t (subtree_leq item (cadr lst)))))))
		  