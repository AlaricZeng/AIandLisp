(defun Twoitems_list(item lst)
	(cond ((null lst) ())
		  (t (cons (list item (car lst)) (Twoitems_list item (cdr lst))))))


(defun Cartesian_prod(lst1 lst2)
	(cond ((null lst1) ())
		  (t (append (Twoitems_list (car lst1) lst2) (Cartesian_prod (cdr lst1) lst2)))))