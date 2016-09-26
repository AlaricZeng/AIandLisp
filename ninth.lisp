(defun inherit-get (symbol property)
	(cond ((equal nil (get symbol property)) (cond ((equal nil (get symbol 'isa)) nil) 
												   (t (inherit-get (inherit-get symbol 'isa) property))))
		  (t (get symbol property))))