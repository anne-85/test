;; morse code grouped by ten  
;; 0-9, A-J, K-T, U-Z
;; encoding is : 
;; left three bits for char size
;; five right bits : 0 => short, 1 => long, non used bits left at 0
;; ----------------------------------------------------------------
(defvar mrs '(191 175 167 163 161 160 176 184 188 190
	65 136 138 100 32 130 102 128 64 135
	101 132 67 66 103 134 141 98 96 33
	97 129 99 137 139 140))

;; blink
;; given a pos in the morse code list, make the led blink the morse digit/letter
;; -----------------------------------------------------------------------------
(defun bli(ch)
	(let* ((bin (nth ch mrs))
		   (i (1- (ash bin -5))))
		(loop
		   (analogwrite 10 255)
		   (if (logbitp i bin)
		   		(delay 600)
		   		(delay 200))   
		   (analogwrite 10 0)
		   (delay 200)
		   (decf i)
		   (when (< i 0) (return)))))

;; emit
;; given a list of morse code positions, successively emit the code
;; a negative value indicates a word separation
;; ----------------------------------------------------------------
(defun emt(lst)
	(dolist (elm lst)
		(if (< elm 0)
			(delay 1400)
			(progn
				(bli elm)
				(delay 600)))))

;; usage example
;; emit the digits then the full alphabet
;; --------------------------------------
(emt '(0 1 2 3 4 5 6 7 8 9))
(emt '(10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31 32 33 34 35))

(defun sos()
	(loop (emt '(28 24 28 -1))))


(emt '(10 23 23 14))  ;;anne
(emt '(28 14 11 10 28 29 18 14 23 -1 14 28 29 -1 21 14 -1 25 21 30 28 -1 11 14 10 30)) ;;sebastien est le plus beau
