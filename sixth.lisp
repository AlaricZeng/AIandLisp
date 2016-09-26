(defun meld_in (item lst)
	(cond ((null lst) ())
	 	  ((atom (car lst)) (cons (car lst) (meld_in item (cdr lst))))
	 	  (t (cons (append item (meld_in item (car lst))) (meld_in item (cdr lst))))))