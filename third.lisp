(defun rep(n k)
	(cond ((= 0 n) ())
		  (t (cons k (rep (- n 1) k)))))

(defun duplicate_atoms(lst)
		  (duplicate_atoms_help lst 1))

(defun duplicate_atoms_help(lst num)
	(cond ((null lst) ())
		  (t (cons (rep (+ num (car lst)) (car lst)) (duplicate_atoms_help (cdr lst) (+ num 1))))))