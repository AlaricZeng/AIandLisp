(defun finditem(item lst)
	(cond ((null lst) 0)
		  (t (cond ((= item (car lst)) (+ 1 (finditem item (cdr lst))))
				   (t (finditem item (cdr lst)))))))

(defun most_once(lst1 lst2)
	(cond ((null lst1) 0)
		  (t (cond ((> 2 (finditem (car lst1) lst2)) (+ 1 (most_once (cdr lst1) lst2)))
		  	       (t (most_once (cdr lst1) lst2))))))
