;You can modify the below line to fit your bubble block
(setq mybubble "hh") ;<---modify here, replace "hh" with your block
;hh is foxconn's bubble
;--------------------------
(setq bubblemsg "
Author: QiZhenhua, 2018/07/21
Right belong: QiZhenhua, forever
This lisp use to add/insert/modify bubbles for drawing.
If you drawing have another bubble block name with a attribute,
you can modify the second line of bubble.lsp
----------useage---------
createbubble: add a bubble in drawing, the counter will +1 auto.
rotatebubble: rotate a bubble angle 90, one by one manual.
insertbubble: insert a bubble, the counter will +1 after this bubble.
modifybubble: bubbles after selected bubble will +/- a number
helpofbubble: show this message
--------------------------
")
;-------------------------

(defun C:helpofbubble()
 (alert bubblemsg)
)
;--------------------------
(defun C:createbubble()
 (setq n (getint "Please input start number: "))
 (if(= n nil) (setq n 1))
 (setq x (getreal "Please input scale: "))
 (if(= x nil) (setq n 1))
 (while T
  (command "-insert" mybubble PAUSE x x 0 n "")
  (setq n (1+ n))
 )
)
;--------------------------
(defun C:rotatebubble()
 (while T
 (setq myblock (entsel "please select HH bubble block: "))
 (setq mylist (entget (car myblock)))
 (if (= mybubble (cdr (assoc 2 mylist)))   ;get and compare block name
     (progn
      (setq p0 (cdr (assoc 10 mylist))) ;get rotate center
      (setq p0 (list (nth 0 p0) (nth 1 p0)))  ;remove z axis
      (command "rotate" myblock "" p0 90)
     )
 )
 )
)
;--------------------------
(defun C:insertbubble()
 (alert "Input a number, app will insert a block from the number.")
 (setq insnum (getint "Please input a number that you want to insert: "))
 (command "-insert" mybubble PAUSE 1 1 0 insnum "")
 (setq myblock (entlast))
 (adjustBubble myblock 1 T)
)
;--------------------------
(defun C:modifybubble()
 (Alert "Select a HH block and input a number,\nthe app will add the number from the block.")
 (setq myblock (car(entsel)))
 (setq insnum (getint "Please input a number that you want to increase: "))
 (if(= insnum nil) (setq insnum 0))
 (if (= mybubble (cdr (assoc 2 (entget myblock))))
   (adjustBubble myblock insnum nil)
   (alert "Not a HH block, do nothing! ")
 )
)
;--------------------------
(defun adjustbubble(/myblock insnum myflag) ;myflag decide remove myblock from list
 (setq mytext (entnext myblock))  ;my block's text, it is a sting
 (setq mynumber (cdr (assoc 1 (entget mytext)))) ;get the text as sting
 (setq listb (ssget "X" (list (cons 2 mybubble)))) ; select all bubbles
 ;(setq a (ssget "C" (getpoint) (getpoint))) ;more function will make things complex
 (if myflag (ssdel myblock listb)) ;according flag remove myblock
 (setq n (sslength listb))
 (setq i 0)
 (while (< i n)
  (setq cblock (ssname listb i)) ;current block
  (setq ctext (entnext cblock))  ;current block's text, this is a entity
  (setq cnumber (cdr (assoc 1 (entget ctext)))) ;get the text, it is a sting
  (if (>= (atoi cnumber) (atoi mynumber))
   (progn
     (setq oldr (assoc 1 (entget ctext)))
     (setq newr (cons 1 (itoa (+ insnum (atoi cnumber)))))
     (entmod (subst newr oldr (entget ctext)))
    )
   )
  (setq i (1+ i))
 )
 (command "regen") ;otherwise the drawing will not change invision
)
;--------------------------
(defun c:test() ;for test
 (setq a (car (entsel "select a block")))
 (setq b (entnext a))
 (setq c (assoc 1 (entget b)))
)