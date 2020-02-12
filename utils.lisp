#|
 |  Lege
 |  Copyright (C) 2008-2010 Matteo Grella
 |
 |  This program is free software: you can redistribute it and/or modify
 |  it under the terms of the GNU General Public License as published by
 |  the Free Software Foundation, either version 3 of the License, or
 |  (at your option) any later version.
 |
 |  This program is distributed in the hope that it will be useful,
 |  but WITHOUT ANY WARRANTY; without even the implied warranty of
 |  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 |  GNU General Public License for more details.
 |
 |  You should have received a copy of the GNU General Public License
 |  along with this program. If not, see <http://www.gnu.org/licenses/>.
|#

;
; Funzioni di elaborazione stringhe
;

(defmacro is-unix ()
	'(find :UNIX *features*))

(defun str-repeat (str n)
	(let ((ret ""))
		(dotimes (i n ret)
			(setf ret (concatenate 'string ret str)))))
			
(defun right-str (string len)
	(when (and (>= (length string) len) (> len 0))
		  (subseq string (- (length string) len))))

(defun left-str (string len)
	(when (and (>= (length string) len) (> len 0))
		  (subseq string 0 len)))

; expand strings, replacing old string with new
(defun replace-str (string old new)
    (let* ((from 0) (opos 0)
           (lnew (length new))
           (lold (length old)))
        (loop
            (setq opos (search old string :start2 from :test #'string-equal))
            (unless opos (return string))
            (setf string (concatenate 'string (subseq string 0 opos) new
                                              (subseq string (+ opos lold))))
            (setq from (+ opos lnew)))))

; expand strings, replacing old char with new
(defun replace-chr (string oldchr newchr)
    (substitute (char newchr 0) (char oldchr 0) string))

(defun count-char (str c)
	(let ((cnt 0))		 
		 (progn	(dotimes (i (length str)) (when (equal (char str i) c) (setq cnt (1+ cnt)))) cnt)))
		 
;
; Funzioni per le liste
;

(defun append1 (lis el) 
"; aggiunge un elemento a una lista, come append ma fa da solo (list el)"
	(append lis (list el)))


(defun append2 (lis el)
"come append1, ma se l'elemento e' nil non viene aggiunto"
	(print el)
   (cond ((or (null el) (null lis)) (append lis el))
	 (t (append lis (list el)))))


(defun a-in-b (a b)
"Restituisce t se tutti gli elementi di a sono presenti in b, altrimenti nil"
	(if (set-difference a b :test #'equal) nil t))
	
(defun a-eq-b (a b)
"Restituisce t se tutti gli elementi di a sono presenti in b e viceversa"
	(if (and (equal (length a) (length b)) (equal (set-difference a b :test #'equal) nil)) t nil))

; STRING-LIST creates a list of words by a string of chars.
; string   -> name of the string to transform in list.
; sepchar  -> the separation character.
; flag     -> T= transforms multiple separators into one single, NIL= no.
; ritorna  -> the list of words that they compose the string.

(defun skip-char (string pos sepchar max)
    (loop
        (if (>= (setq pos (1+ pos)) max)
            (return (1- pos))
            (if (char/= (char string pos) sepchar)
                (return (1- pos))))))


(defun string-list (string sepchar &optional (flag t))
    (let* ((listw (list ""))
           (lens 0) (oldp -1) (i 0))
        (when flag
            (setf string (string-trim (string sepchar) string)))
        (if (eq (setf lens (length string)) 0)
            listw
            (progn
              (setf string
                    (concatenate 'string string (string sepchar)))
              (loop
                (when (char= (char string i) sepchar)
                    (nconc listw (list (subseq string (1+ oldp) i)))
                    (when flag (setq i (skip-char string i sepchar lens)))
                    (setq oldp i))
                (if (> (setq i (1+ i)) lens)
                    (return (rest listw))))))))

(defun string-to-list-2 (string)
  "Returns a list of the data items represented in the given list."
  (let ((the-list nil) ;; we'll build the list of data items here
        (end-marker (gensym))) ;; a unique value to designate "done"
    (loop (multiple-value-bind (returned-value end-position)
                               (read-from-string string nil end-marker)
            (when (eq returned-value end-marker)
              (return the-list))
            ;; if not done, add the read thing to the list
            (setq the-list 
                  (append the-list (list returned-value)))
            ;; and chop the read characters off of the string
            (setq string (subseq string end-position))))))
            
(defun str-by-strlist (strlist sep)
"Da una lista contenente stringhe, restituisce un'unica stringa con separatore specificato."
	(let ((str "") (first t))
		(dolist (x strlist str)
			(if first
				(setf first nil)
				(setf str (concatenate 'string str sep)))
			(setf str (concatenate 'string str x)))))
			
			
	
; rimuove i nil da una lista
(defun remove-nil (l)
  (cond ((null l) nil)
        ((null (car l)) (remove-nil (cdr l)))
        (t (cons (car l) (remove-nil (cdr l))))))

;
; Input con prompt
;
(defun prompt-read (prompt)
	(format *query-io* "~a" prompt)
	(force-output *query-io*)
	(read-line *query-io*))

;
; Trasforma una stringa in numero, se non e' un numero valido restituisce 0
;
(defun str-to-int (str)
	(or (parse-integer str :junk-allowed t) 0))
	
	
(defun force-garbage (times)
    (dotimes (i times)
        (gc)) nil)  ;force the garbage collection
		

(defun princc (&rest args)
    (dolist (sym args)                                             
       (if (eq sym t) (terpri) (princ sym)))) ;(print sym)


;---------------
; PROGRESS BAR 
;---------------

(defun tpbar-init (vmax &optional (vmin 0.0))
"Inizializza una nuova progress bar"
    (let ((ret (list :min-val vmin :max-val vmax :cur-val vmin :perc 0.0 :old-perc -1 :screen-len 50 :time-start (get-universal-time))))
	ret))
	
(defun tpbar-inc (pbar &optional (val 1))
"Incrementa il valore"
    (setf (getf pbar :cur-val) (+ (getf pbar :cur-val) val))
    (setf (getf pbar :perc) (* (/ (- (getf pbar :cur-val) (getf pbar :min-val)) (- (getf pbar :max-val) (getf pbar :min-val))) 100.0)))

(defun tpbar-start (pbar)
"Inizializza il time-start"
    (setf (getf pbar :time-start) (get-universal-time)))

(defun h-i-s (s)
"Dal numero di secondi passato in argomento, restituisce una stringa formato 'H:i:s'"
    (let ((h 0) (i 0))
	(loop while (>= s 3600) do (incf h) (setf s (- s 3600)))
	(loop while (>= s 60) do (incf i) (setf s (- s 60)))
	(when (> h 99) (setf h 99))
	(format nil "~2,'0D:~2,'0D:~2,'0D" h i s)))

(defun tpbar-print (pbar &optional (dest t))
"Mostra la barra (print 'economico')"
    (unless (equal (round (getf pbar :perc)) (round (getf pbar :old-perc)))
	(let (iTo time-elap time-rema)
	    ; time-elap : (cur-min) = time-rema : (max-cur)
	    ; time-rema = time-elap * (max-cur) / (cur-min)
	    (setf time-elap (- (get-universal-time) (getf pbar :time-start)))
	    (setf time-rema (round (/ (* time-elap (- (getf pbar :max-val) (getf pbar :cur-val))) (- (getf pbar :cur-val) (getf pbar :min-val)))))
	    
		(format dest "~a[" #\Return)
	    
	    (setf iTo (round (* (/ (getf pbar :perc) 100.0) (getf pbar :screen-len))))
	    
	    (when (> iTo 1) (loop for i from 2 to iTo do (format dest "=")))
	    (when (> iTo 0) (format dest ">"))
	    
	    (setf iTo (- (getf pbar :screen-len) iTo))
	    (loop for i from 1 to iTo do (format dest "."))
		
	    (format dest "] ~3D% (~A) (~A)" (round (getf pbar :perc)) (h-i-s time-elap) (h-i-s time-rema))
	    (setf (getf pbar :old-perc) (getf pbar :perc)))
		(force-output) ; per sbcl!
		))

#|(defun main()

    (let (pb)
	
	(setf pb (tpbar-init 2000 1000))
	
	(loop for i from 1000 to 2000 do
	
	    (loop for k from 0 to 9999 do t)
	
	    (tpbar-inc pb)
	    (tpbar-print pb))
	
	nil
	
	))
|#

(defun colorize-str (str &optional &key (fg 'WHITE) (bg 'BLACK) (no-bold))

	(setf fg (case fg
			 	(BLACK 30)
				(RED 31)
				(GREEN 32)
			    (YELLOW 33)
				(BLUE 34)
				(MAGENTA 35)
				(CYAN 36)
				(WHITE 37)
				(t 0)))
						
	(setf bg (case bg
			 	(BLACK 40)
				(RED 41)
				(GREEN 42)
			    (YELLOW 43)
				(BLUE 44)
				(MAGENTA 45)
				(CYAN 46)
				(WHITE 47)
				(t 0)))
	(if no-bold
		(format nil "~C[~A;~Am~A~C[0m" #\Esc fg bg str #\Esc)
		(format nil "~C[~A;~A;1m~A~C[0m" #\Esc fg bg str #\Esc)))

(defun recursive-lex-color (lst)

	(let (prev-el)
	
		(loop for i from 0 to (1- (length lst)) do
		
			(let ((el (nth i lst)))
		
				(if (listp el)
					;; el Ã¨ una lista => ricorsione
					(setf (nth i lst) (recursive-lex-color el))
					
					;; el non Ã¨ una lista
					(when (and (equal prev-el :LEX) (stringp el))
						(case (getf lst :CAT)
							((100 120 130)		(setf (nth i lst) (colorize-str el :fg 'WHITE :bg 'GREEN)))
							((20 21 200 201 101 202 203)	(setf (nth i lst) (colorize-str el :fg 'WHITE :bg 'RED)))
							((450 451)	(setf (nth i lst) (colorize-str el :fg 'WHITE :bg 'MAGENTA)))
							((800) (setf (nth i lst) (colorize-str el :fg 'RED :bg 'BLACK :no-bold nil)))
							(t
								(cond
									((and (>= (getf lst :CAT) 700)) (setf (nth i lst) (colorize-str el :fg 'BLACK :bg 'WHITE)))
									((and (>= (getf lst :CAT) 500)	(< (getf lst :CAT) 600)) (setf (nth i lst) (colorize-str el :fg 'WHITE :bg 'RED)))
									((> (getf lst :CAT) 490)	(setf (nth i lst) (colorize-str el :fg 'WHITE :bg 'BLUE)))
									(t							(setf (nth i lst) (colorize-str el :fg 'BLACK :bg 'YELLOW)))
									)								
								))))
						
				(setf prev-el el))))
	lst)



(defmacro clear() (dotimes (i 255) (format t "~%")))

(defun repid-html-entities-encode (obj)
	
	(setf obj (format nil "~a" obj)) ; in qualunque caso riconduco ad una stringa
	
	(setf obj (replace-str obj "&" "&amp;")) ; <- deve rimanere la prima operazione!
	
	(setf obj (replace-str obj "\"" "&quot;"))
	(setf obj (replace-str obj "<" "&lt;"))
	(setf obj (replace-str obj ">" "&gt;"))
	
	(setf obj (replace-str obj "à" "&agrave;"))
	(setf obj (replace-str obj "è" "&egrave;"))
	(setf obj (replace-str obj "é" "&eacute;"))
	(setf obj (replace-str obj "ì" "&igrave;"))
	(setf obj (replace-str obj "ò" "&ograve;"))
	(setf obj (replace-str obj "ù" "&ugrave;"))
	
	obj)
