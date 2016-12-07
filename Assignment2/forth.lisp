(defun infix(lst)
	(cond ((listp (car lst))  (list (append (append (infix (cdar lst)) (list (caar lst))) (infix (cddar lst)))))
		  ((null (cddr lst)) (list (car lst)))
		  (t (append (append (infix (cdr lst)) (list (car lst))) (infix (cddr lst))))))