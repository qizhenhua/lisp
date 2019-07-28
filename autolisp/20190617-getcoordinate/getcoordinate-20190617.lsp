(defun C:getcircle()
 (setq zerop (getpoint "First, you need to pickup a Zero Point:"))
 (setq result (list))
 (print)
 (princ "Now, Pickup Circle:")

 (while (setq mysel (entsel))
  (setq myent (car mysel))
  (setq p0 (cdr (assoc 10 (entget myent))))
  (setq p0 (list (- (nth 0 p0) (car zerop)) (- (nth 1 p0) (cadr zerop)) ))
  (setq result (append result (list p0)))
 )

 (print)
 (princ "The coordinate of circles:")
 (print)

 (setq n 0)
 (while (<= n (length result))
  (print (nth n result))
  (print)
  (setq n (+ 1 n))
 )

 (print)
)


