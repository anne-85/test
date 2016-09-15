;; morse code grouped by ten  
;; 0-9, A-J, K-T, U-Z
;; encoding is : 
;; left three bits for char size
;; five right bits : 0 => short, 1 => long, non used bits left at 0
;; ----------------------------------------------------------------
(defvar mrs '((0 . 191) (1 . 175) (2 . 167) (3 . 163) (4 . 161) (5 . 160) (6 . 176) (7 . 184) (8 . 188) (9 . 190)
	(a . 65) (b . 136) (c . 138) (d . 100) (e . 32) (f . 130) (g . 102) (h . 128) (i . 64) (j . 135)
	(k . 101) (l . 132) (m . 67) (n . 66) (o . 103) (p . 134) (q . 141) (r . 98) (s . 96) (t . 33)
	(u . 97) (v . 129) (w . 99) (x . 137) (y . 139) (z . 140)))

(defvar spd 200)

;; blink
;; given a pos in the morse code list, make the led blink the morse digit/letter
;; -----------------------------------------------------------------------------
(defun bli(ch)
	(let* ((bin (cdr (assoc ch mrs)))
		   (i (1- (ash bin -5))))
		(loop
			(analogwrite 10 255)
			(if (logbitp i bin)
		   		(delay (* spd 3))
		   		(delay spd))   
			(analogwrite 10 0)
			(delay spd)
			(decf i)
			(when (< i 0) (return)))))

;; emit
;; given a list of morse code positions, successively emit the code
;; a negative value indicates a word separation
;; ----------------------------------------------------------------
(defun emt(lst)
	(dolist (elm lst)
		(setq spd (+ 100 ( / (analogread 8) 3)))
		(if (and (numberp elm) (< elm 0))
			(delay (* spd 7))
			(progn
				(bli elm)
				(delay (* spd 3))))))

;; usage example
;; emit the digits then the full alphabet
;; --------------------------------------
(defun sos()
	(loop (emt '(s o s -1))))

(emt '(0 1 2 3 4 5 6 7 8 9))
(emt '(a b c d e f g h i j k l m n o p q r s t u v w x y z))

(emt '(a n n e))
(emt '(s e b a s t i e n -1 e s t -1 l e -1 p l u s -1 b e a u))
