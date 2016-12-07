(defun full-inherit-get(symbol property)
	(cond ((equal nil (get symbol property)) (cond ((equal nil (get symbol 'isa)) ())
												   (t (full-inherit-get-help (full-inherit-get symbol 'isa) property))))
		  (t (get symbol property))))

(defun full-inherit-get-help (lst property)
	(cond ((null lst) ())
		  (t (cond ((equal nil (get (car lst) property)) (or (full-inherit-get-help (cdr lst) property) (full-inherit-get-help (full-inherit-get (car lst) 'isa) property)))
				   (t (get (car lst) property))))))

