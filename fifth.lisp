(defun inside_m(lst)
	(cond ((null (caddr lst)) ())
		  (t (cond ((listp (cadr lst)) (append (inside_item (cadr lst)) (inside_m (cdr lst))))))))

(defun inside_item(lst)
	(cond ((null (caddr lst)) ())
		  (t (cons (cadr lst) (inside_item (cdr lst))))))