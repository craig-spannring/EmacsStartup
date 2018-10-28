
(put 'unary+ 'prec 9)
(put 'unary- 'prec 9)
(put '*      'prec 6)
(put '/      'prec 6)
(put '%      'prec 6)
(put '+      'prec 4)
(put '-      'prec 4)


(defun infix (prec expr)
  "Convert a subset of lisp arithmetic expressions to infix notation"
  (cond 
   ((atom  expr) (format " %s " expr))
   ((and (listp expr) (member (first expr) '(+ - * / mod)))
    ; See if it's unary
    (if (and (member (first expr) '(+ -)) (= (length expr) 2))
        (format " %s%s " (first expr) (second expr))
      (let ((op (if (equal 'mod (first expr)) '% (first expr)))
            result)
        (setq result (infix (get op 'prec) (second expr)))
        (setq expr (cddr expr))
        (while expr
          (setq result (format "%s %s %s" result op 
                               (infix (get op 'prec) (first expr))))
          (setq expr (rest expr)))
        (if (>= prec (get op 'prec)) 
            (concat " ( " result " ) ")
          result))))
   ((and (listp expr) (member (first expr) '(setq)) (= (length expr) 3))
    (format "%s = %s" (second expr) (infix 0 (third expr))))
   ((and (listp expr) 
         (member (first expr) '(sin cos tan sqrt))
         (= (length expr) 2))
    (format " %s(%s) " (first expr) (infix 0 (second expr))))
   (t (format "NotHandled"))))

  


(defun print-cvs-tin (foo)
  "Debug utility."
  (let ((cookie (tin-cookie cvs-cookie-handle foo))
	(stream (get-buffer-create "debug")))
    (princ "==============\n" stream)
    (princ (cvs-fileinfo->file-name cookie) stream)
    (princ "\n" stream)
    (princ (cvs-fileinfo->dir cookie) stream)
    (princ "\n" stream)
    (princ (cvs-fileinfo->full-log cookie) stream)
    (princ "\n" stream)
    (princ (cvs-fileinfo->marked cookie) stream)
    (princ "\n" stream)))

(defun parse-cvs-bug-mail ()
  "Try the current parser on a cvs bug message."
  (interactive)
  (goto-char (point-min))
  (re-search-forward
   "--- Contents of stdout buffer (\\([0-9]+\\) chars) ---\n")
  (let ((size (string-to-int (buffer-substring (match-beginning 1)
					       (match-end 1))))
	(beg (point))
	(stdout-buf (get-buffer-create "stdout")))
    (save-window-excursion (set-buffer stdout-buf) (erase-buffer))
    (goto-char (+ (point) size))
    (if (looking-at "\n")
	(forward-char))
    (if (not (looking-at "--- End of stdout buffer ---"))
	(error "Expected \"--- End of stdout buffer ---\" at point"))
    (princ (buffer-substring beg (point)) stdout-buf)
    (re-search-forward 
     "--- Contents of stderr buffer (\\([0-9]+\\) chars) ---\n")
    (let ((size (string-to-int (buffer-substring (match-beginning 1)
						 (match-end 1))))
	  (beg (point))
	  (stderr-buf (get-buffer-create "stderr")))
      (save-window-excursion (set-buffer stderr-buf) (erase-buffer))
      (goto-char (+ (point) size))
      (if (looking-at "\n")
	  (forward-char))
      (if (not (looking-at "--- End of stderr buffer ---"))
	  (error "Expected \"--- End of stderr buffer ---\" at point"))
      (princ (buffer-substring beg (point)) stderr-buf)
      (cvs-parse-update stdout-buf stderr-buf)))
  (switch-to-buffer-other-window "*cvs*"))


(defun distance-between-points (p1 p2)
  "Return the distance between two points.  The points are represented as lists with the form '(x y)
"
  (let ((x1 (+ 0.0 (first   p1)))
        (y1  (+ 0.0 (second p1)))
        (x2  (+ 0.0 (first  p2)))
        (y2  (+ 0.0 (second p2))))
    (let ((dx (- x1 x2))
          (dy (- y1 y2)))

      (sqrt (+ (* dx dx) (* dy dy))))))



(defun distance-line-to-point (l p)
  "Return the distance from the given point p to the line l

p is a list with two elements.  The first element is the X coordinate,
the second is the Y coordinate.

l is a list of two lists.  Each list contains a point on the line.
The format of the sublists is the same as the format of p.

Note-  doesn't handle vertical lines yet
"

  (let 
      ((px1 (+ 0.0 (first p)))
       (py1 (+ 0.0 (second p)))
       (x1 (+ 0.0 (first (first l))))
       (y1  (+ 0.0 (second (first l))))
       (x2  (+ 0.0 (first (second l))))
       (y2  (+ 0.0 (second (second l)))))

    (let ((dx  (- x2 x1))
          (dy  (- y2 y1)))
      (let ((m   (/ dy dx))) ; slope of the line
        (let ((A   m)
              (B   -1)
              (C   (+ (* (- m) x1) y1)))
          (/ (abs (+ (* A px1) (* B py1) C)) (sqrt (+ (* A A) (* B B)))))))))

