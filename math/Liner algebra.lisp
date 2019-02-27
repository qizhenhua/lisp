;;This lisps for QiZhenhua to study Liner algebra
;;Project start from 2018/8/1, just for fun
;;----------Part I: input system--------
;;INPUT-LIST combine the previous 2 functions, if n<=0, it will quit until get nil
(defun input-list (n)
  (format t "input: ")
  (let (( a (read)))
    (if (= (- n 1) 0)
	(cons a nil)
	(progn 
	  (if (and (<= n 0) (equal a 'nil))
	      nil
	      (cons a (input-list (- n 1))))))))

;;----------Part II: Algebra basic operation----
;;LIST-ELEMENT-EXCHANGE is used to change 2 lines position
(defun list-element-exchange (lst n1 n2)
  ;;no check
  (let ((a (nth n1 lst)) (b (nth n2 lst)) (c (copy-list lst)))
    (setf (nth n1 c) b)
    (setf (nth n2 c) a)
    c))
    
;;MATRIX-ROW-MULTIPLY is used to make the Nth row multiply a number x
(defun matrix-row-multiply (matrix n x)
  ;;should check n or x is legal
  (let ((a (nth n matrix)) (c (copy-list matrix)))
    (setf (nth n c) (mapcar #'(lambda (x1) (* x x1)) a))
    c))

;;MATRIX-ROW-ADD is used to add n1 row to n2 row in list
(defun matrix-row-add (matrix n1 n2)
  ;; as above
  (let ((a (nth n2 matrix)) (b (nth n1 matrix)) (c (copy-list matrix)))
    (setf (nth n2 c) (mapcar #'+ a b))
    c ))

;;MATRIX-ROW-MULTIPLY-ADD combine upon operation
(defun matrix-row-multiply-add (matrix n1 x n2)
  (let ((a (nth n1 matrix)) (c (copy-list matrix)))
    (setf a (mapcar #'(lambda (x1) (* x x1)) a))
    (setf (nth n2 c) (mapcar #'+ a (nth n2 c)))
    c))

;;----------Part III: matrix operation forward phase (of row reduction)---
;;WHERE-0 is used to detect where are 0 in a list
(defun where-0 (lst)
  (let ((output nil))
    (do ((i 0 (+ 1 i))) ((> i (- (length lst) 1)) output)
      (if (equal 0 (nth i lst))
	  (setf output (cons i output)))
      )))

;;PERMUTATION is used to find all permutation of a list elements, if set start zero
(defun permutation (lst start) ;start must small than lenth of lst
  (let ( (M (copy-list lst)) (N (1- (length lst))) (output nil) (another nil))
    (do ((i start (1+ i))) ((> i N) output)
      (do ((j (1+ i) (1+ j))) ((> j  N) output)
	(setf another (list-element-exchange M i j))
	(setf output (cons another output))
	;(print output)
	(setf output (append (permutation another (1+ i)) output))
	))))

;;SET-POSITION-UN0 set the indicate position to un0
(defun set-position-un0 (matrix row col)
  (let ( (M (copy-list matrix)))
    (setf all-situation (cons M (permutation M row)))
    (do ((i 0 (1+ i))) ( (= i (length all-situation)) M)
      (setf M (nth i all-situation))
      (if (= 0 (nth col (nth row M)))
	  (setf M nil) (return-from set-position-un0 M)))))

;;MATRIX-ELIMINATE used to make to eliminate a column
;;Ncol indicate whice col, general from the first column (the left column 0)
(defun matrix-eliminate (matrix Ncol)
  (let ( (M (copy-list matrix)))
    (setf M (set-position-un0 M Ncol Ncol))
    (if M M (return-from matrix-eliminate nil));if nil, Ncol should -1
    ;;Set the first line main eliment to 1
    (do ((Nrow Ncol (1+ Nrow))) ((=  Nrow (length M)) M)
      (if (= Nrow Ncol) ;diagonal element
	  (setf M (Matrix-row-multiply M Nrow
				       (/ 1 (nth Ncol (nth Nrow M))))))
       ;prevent divide by zero
      (if (and (/= 0 (nth Ncol (nth Nrow M))) (> Nrow Ncol))
	  (setf M (matrix-row-multiply-add M Ncol (- 0 (nth Ncol (nth Nrow M))) Nrow)))
      )))

;;MATRIX-ECHELON be used to make echelon of a matrix, it will be used to previous ELIMILATE	
(defun matrix-echelon (matrix)
  (let ( (M (copy-list matrix))); (matrix-diagonal matrix)))
    (if M
	(Do ((Ncol 0 (+ Ncol 1))) ((> Ncol (- (length M) 1)) M)
	  (print M)
	  (setf M (matrix-eliminate M Ncol))
	  (if M M (print "The matrix cann't go on forward phase row of reduction! "))
	  ))))
;---------------Part IV: backward phase-------------

;;POSITION-MAIN-ELIMENT be used to find the main element position
(defun position-main-element (lst)
  (do ((i 0 (+ i 1))) ((= i (length lst)))
    (if (/= 0 (nth i lst))
	(return i))))

;;MATRIX-SIMPLIFY be used to make a matrix to a simplified form
(defun matrix-simplify (matrix Nrow)
  (if (= Nrow 0) (return-from matrix-simplify matrix))
  (let ((M (copy-list matrix)) (factor nil)
	(Ncol (position-main-element (nth Nrow matrix))))
    (Do ((i Nrow (- i 1))) ((= i 0) M)
      (setf factor (- 0 (nth Ncol (nth (1- i) M))))
      (setf M (matrix-row-multiply-add M Nrow factor (- i 1))))
    (print M)
    (matrix-simplify M (1- Nrow))))

;;--------Main function-----
;;Main function, solve linear equation system
(defun solutionof (matrix)
    (let ((M (matrix-echelon matrix)))
      (if M M (return-from solutionof (print "The linear system have no the only solution!")))
      (matrix-simplify M (1- (length M)))))
