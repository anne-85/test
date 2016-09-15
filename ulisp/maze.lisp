; Route finder

(defvar md nil)

(defun adr (frm to tim)
  (push (list frm to tim) md)
  (push (list to frm tim) md))

; Add item to correct position in queue

(defun adi (i q)
  (if (null q) 
      (cons i q)
    (if (< (first i) (first (first q)))
        (cons i q)
      (cons (first q) (adi i (rest q))))))

; Add to queue

(defun adq (tim loc via pq)
  (setq pq (adi (list tim loc via) pq))
  pq)

;
; For all the roads from a place, add to queue
;

(defun ars (loc go pq)
  (dolist (i md pq)
    (let* ((frm (first i))
           (to (second i))
           (tim (third i)))
      (when (eq frm loc)
          (setq pq (adq (+ go tim) to loc pq))))))

; Grow search

(defun gro (frm to)
  (let* ((vis (list (cons frm nil)))
         (pq (ars frm 0 nil))
         w)
    (loop
     (when (eq frm to) (return (reverse vis)))
     (unless pq (return))
     (setq w (first pq))
     (setq frm (second w))
     (setq pq (cdr pq))
     (unless (assoc frm vis)
       (setq vis (cons (cons frm (third w)) vis))
       (setq pq (ars frm (car w) pq))))))

; List the route

(defun lis (frm to)
  (let* ((vis (gro frm to))
        rte)
    (when vis
      (loop
        (push to rte)
        (when (eq frm to) (return rte))
        (setq to (cdr (assoc to vis)))))))

; Define the map

(defun mm ()
  (adr 'a 'b 2)
  (adr 'b 'c 3)
  (adr 'a 'd 9)
  (adr 'b 'e 3)
  (adr 'c 'f 7)
  (adr 'd 'e 3)
  (adr 'e 'f 6)
  (adr 'd 'g 2)
  (adr 'e 'h 8)
  (adr 'f 'z 6)
  (adr 'g 'h 2)
  (adr 'h 'z 4))