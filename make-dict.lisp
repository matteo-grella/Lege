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

(defvar dat-verbi-disc-complete)
(load "dat-verbi-disc-complete")

(defun are-comparable (a b)
	(and (equal (getf a :LEX) (getf b :LEX))
	     (equal (getf a :CAT) (getf b :CAT))
	     (equal (getf a :MDV) (getf b :MDV))
	     (equal (getf a :TMP) (getf b :TMP))
	     (equal (getf a :PRS) (getf b :PRS))
	     (equal (getf a :DGR) (getf b :DGR))))

;; cross check
(defun num-gen (a b num1 gen1 num2 gen2)
	(or
		(and (equal (getf a :NUM) num1)
		     (equal (getf a :GEN) gen1)
		     (equal (getf b :NUM) num2)
		     (equal (getf b :GEN) gen2))
		     
		(and (equal (getf a :NUM) num2)
		     (equal (getf a :GEN) gen2)
		     (equal (getf b :NUM) num1)
		     (equal (getf b :GEN) gen1))))

(defun consolidate-morphology (ambiguous)
	
	(if (= (length ambiguous) 1)
	
		;; len == 1: return itself
		ambiguous
		
		;; len > 1
		(let (ret elem1 elem2 changes)
			(setf ambiguous (copy-tree ambiguous)) ;; safe copy
			
			;(format t "---------------------~%")
			;(format t "~a~%" ambiguous)
			;(format t "---------------------~%")
			
			;; Firstly keep multiple elems as-it-is
			(setf ambiguous (remove-if #'(lambda (elem) (progn
					(if (= (length elem) 1)
						nil ; do not remove single element
						(progn (push elem ret) t)) ; move out
				)) ambiguous))
			
			;; Remove a level for remaining single-level
			(setf ambiguous (mapcar #'(lambda (elem) (car elem)) ambiguous))
			
			;; Ok...
			(setf changes t)
			(loop while changes do
			
				(setf changes nil)
				
				(block outer-loop (loop for i from 0 to (1- (length ambiguous)) do
					(setf elem1 (nth i ambiguous))
					
					(loop for j from (1+ i) to (1- (length ambiguous)) do
						(setf elem2 (nth j ambiguous))
						
						(when (are-comparable elem1 elem2)
							
							(cond
								((num-gen elem1 elem2    1 1    1 1)
									(setf changes t)) ;; DUPLICATE
							
								((num-gen elem1 elem2    1 1    1 2)
									(setf (getf elem1 :NUM) 1)
									(setf (getf elem1 :GEN) 3)
									(setf changes t))
								
								((num-gen elem1 elem2    1 1    1 3)
									(setf (getf elem1 :NUM) 1)
									(setf (getf elem1 :GEN) 3)
									(setf changes t))
								
								((num-gen elem1 elem2    1 1    2 1)
									(setf (getf elem1 :NUM) 3)
									(setf (getf elem1 :GEN) 1)
									(setf changes t))
									
								((num-gen elem1 elem2    1 1    2 2)
									nil) ; directly incompatible
									
								((num-gen elem1 elem2    1 1    2 3)
									(setf (getf elem1 :NUM) 3)
									(setf (getf elem1 :GEN) 3)
									(setf changes t))
								
								((num-gen elem1 elem2    1 1    3 1)
									(setf (getf elem1 :NUM) 3)
									(setf (getf elem1 :GEN) 1)
									(setf changes t))
									
								((num-gen elem1 elem2    1 1    3 2)
									(setf (getf elem1 :NUM) 3)
									(setf (getf elem1 :GEN) 3)
									(setf changes t))
									
								((num-gen elem1 elem2    1 1    3 3)
									(setf (getf elem1 :NUM) 3)
									(setf (getf elem1 :GEN) 3)
									(setf changes t))
									
								((num-gen elem1 elem2    1 2    1 2)
									(setf changes t)) ;; DUPLICATE
									
								((num-gen elem1 elem2    1 2    1 3)
									(setf (getf elem1 :NUM) 1)
									(setf (getf elem1 :GEN) 3)
									(setf changes t))
								
								((num-gen elem1 elem2    1 2    2 1)
									nil) ; directly incompatible
								
								((num-gen elem1 elem2    1 2    2 2)
									(setf (getf elem1 :NUM) 3)
									(setf (getf elem1 :GEN) 2)
									(setf changes t))
								
								((num-gen elem1 elem2    1 2    2 3)
									(setf (getf elem1 :NUM) 3)
									(setf (getf elem1 :GEN) 3)
									(setf changes t))
								
								((num-gen elem1 elem2    1 2    3 1)
									(setf (getf elem1 :NUM) 3)
									(setf (getf elem1 :GEN) 3)
									(setf changes t))
								
								((num-gen elem1 elem2    1 2    3 2)
									(setf (getf elem1 :NUM) 3)
									(setf (getf elem1 :GEN) 2)
									(setf changes t))
								
								((num-gen elem1 elem2    1 2    3 3)
									(setf (getf elem1 :NUM) 3)
									(setf (getf elem1 :GEN) 3)
									(setf changes t))
								
								((num-gen elem1 elem2    1 3    1 3)
									(setf changes t)) ;; DUPLICATE
									
								((num-gen elem1 elem2    1 3    2 1)
									(setf (getf elem1 :NUM) 3)
									(setf (getf elem1 :GEN) 3)
									(setf changes t))
								
								((num-gen elem1 elem2    1 3    2 2)
									(setf (getf elem1 :NUM) 3)
									(setf (getf elem1 :GEN) 3)
									(setf changes t))
								
								((num-gen elem1 elem2    1 3    2 3)
									(setf (getf elem1 :NUM) 3)
									(setf (getf elem1 :GEN) 3)
									(setf changes t))
								
								((num-gen elem1 elem2    1 3    3 1)
									(setf (getf elem1 :NUM) 3)
									(setf (getf elem1 :GEN) 3)
									(setf changes t))
								
								((num-gen elem1 elem2    1 3    3 2)
									(setf (getf elem1 :NUM) 3)
									(setf (getf elem1 :GEN) 3)
									(setf changes t))
								
								((num-gen elem1 elem2    1 3    3 3)
									(setf (getf elem1 :NUM) 3)
									(setf (getf elem1 :GEN) 3)
									(setf changes t))
								
								((num-gen elem1 elem2    2 1    2 1)
									(setf changes t)) ;; DUPLICATE
								
								((num-gen elem1 elem2    2 1    2 2)
									(setf (getf elem1 :NUM) 2)
									(setf (getf elem1 :GEN) 3)
									(setf changes t))
								
								((num-gen elem1 elem2    2 1    2 3)
									(setf (getf elem1 :NUM) 2)
									(setf (getf elem1 :GEN) 3)
									(setf changes t))
								
								((num-gen elem1 elem2    2 1    3 1)
									(setf (getf elem1 :NUM) 3)
									(setf (getf elem1 :GEN) 1)
									(setf changes t))
								
								((num-gen elem1 elem2    2 1    3 2)
									(setf (getf elem1 :NUM) 3)
									(setf (getf elem1 :GEN) 3)
									(setf changes t))
								
								((num-gen elem1 elem2    2 1    3 3)
									(setf (getf elem1 :NUM) 3)
									(setf (getf elem1 :GEN) 3)
									(setf changes t))
								
								((num-gen elem1 elem2    2 2    2 2)
									(setf changes t)) ;; DUPLICATE
								
								((num-gen elem1 elem2    2 2    2 3)
									(setf (getf elem1 :NUM) 2)
									(setf (getf elem1 :GEN) 3)
									(setf changes t))
								
								((num-gen elem1 elem2    2 2    3 1)
									(setf (getf elem1 :NUM) 3)
									(setf (getf elem1 :GEN) 3)
									(setf changes t))
								
								((num-gen elem1 elem2    2 2    3 2)
									(setf (getf elem1 :NUM) 3)
									(setf (getf elem1 :GEN) 2)
									(setf changes t))
								
								((num-gen elem1 elem2    2 2    3 3)
									(setf (getf elem1 :NUM) 3)
									(setf (getf elem1 :GEN) 3)
									(setf changes t))
								
								((num-gen elem1 elem2    2 3    2 3)
									(setf changes t)) ;; DUPLICATE
								
								((num-gen elem1 elem2    2 3    3 1)
									(setf (getf elem1 :NUM) 3)
									(setf (getf elem1 :GEN) 3)
									(setf changes t))
								
								((num-gen elem1 elem2    2 3    3 2)
									(setf (getf elem1 :NUM) 3)
									(setf (getf elem1 :GEN) 3)
									(setf changes t))
								
								((num-gen elem1 elem2    2 3    3 3)
									(setf (getf elem1 :NUM) 3)
									(setf (getf elem1 :GEN) 3)
									(setf changes t))
								
								((num-gen elem1 elem2    3 1    3 1)
									(setf changes t)) ;; DUPLICATE
									
								((num-gen elem1 elem2    3 1    3 2)
									(setf (getf elem1 :NUM) 3)
									(setf (getf elem1 :GEN) 3)
									(setf changes t))
								
								((num-gen elem1 elem2    3 1    3 3)
									(setf (getf elem1 :NUM) 3)
									(setf (getf elem1 :GEN) 3)
									(setf changes t))
								
								((num-gen elem1 elem2    3 2    3 2)
									(setf changes t)) ;; DUPLICATE
								
								((num-gen elem1 elem2    3 2    3 3)
									(setf (getf elem1 :NUM) 3)
									(setf (getf elem1 :GEN) 3)
									(setf changes t))
								
								((num-gen elem1 elem2    3 3    3 3)
									(setf changes t)) ;; DUPLICATE
								)
						
							(when changes
								(setf (nth i ambiguous) elem1)
								(setf (nth j ambiguous) nil)
								
								;; Remove the NIL elements
								(setf ambiguous (remove-if-not #'(lambda (elem) (progn
										elem
									)) ambiguous))
								
								(return-from outer-loop)))))))
				
			;(print ambiguous)
			
			(dolist (elem ambiguous)
				(push (list elem) ret))
			
			;(format t "~a~%" ret)
			
			ret)))
			
(defun rewrite-morphology (forms cat)

	(let ((ht (make-hash-table :test #'equal)) ret)
	
		;; Create hash-table
		(dolist (form forms)
			(push (second form) (gethash (first form) ht)))
		
		;; Hash-table loop
		(maphash #'(lambda (h-key h-val) (progn
			
			(setf h-val (consolidate-morphology h-val))
        
			(dolist (buf h-val)
				(setf buf (car buf))
				
                (push 
					(format nil "(\"~a\" ((:lex \"~a\" :cat ~a :mdv ~a :tmp ~a :prs ~a :num ~a :gen ~a :gcase ~a :dgr ~a)))"
						h-key (getf buf :LEX) (rewrite-cat (getf buf :CAT)) (rewrite-mdv (getf buf :MDV)) (rewrite-tmp (getf buf :TMP)) (rewrite-prs (getf buf :PRS) cat) (rewrite-num (getf buf :NUM)) (rewrite-gen (getf buf :GEN)) (rewrite-case (getf buf :GCASE)) (rewrite-dgr (getf buf :DGR)))
						
					ret))
				
			; (format t "---------------------~%")
			;(format nil "~a => ~a~%" h-key h-val)
			
			)) ht) ret))
	

(defun skip-verb (y)
	(or (find 'NO (getf y :MUSO))
	    (and (not (find 'DISC (getf y :FONTE))) (null (setq in-disc-compl (is-common-verb (getf y :LEX)))))
		(and (not (find 'DISC (getf y :FONTE)))
			 (or (equal (getf y :MUSO) '(BU LE))
			     (equal (getf y :MUSO) '(LE BU))
				 (equal (getf y :MUSO) '(BU OB))
				 (equal (getf y :MUSO) '(OB BU))
				 (equal (getf y :MUSO) '(OB LE))
				 (equal (getf y :MUSO) '(LE OB))
				 (equal (getf y :MUSO) '(LE RE))
				 (equal (getf y :MUSO) '(RE LE))
				 ;(equal (getf y :MUSO) '(BU))
				 
				 (and (second in-disc-compl) (equal (second in-disc-compl) "#")
					  
                      (or (null (getf y :MUSO))
					      (equal (getf y :MUSO) '(TS))	
					      (equal (getf y :MUSO) '(RE))
						  (equal (getf y :MUSO) '(BU))
						  (equal (getf y :MUSO) '(RE OB))
						  (equal (getf y :MUSO) '(OB RE))
						  (equal (getf y :MUSO) '(RE TS))
						  (equal (getf y :MUSO) '(TS RE))
						  (equal (getf y :MUSO) '(OB TS))
						  (equal (getf y :MUSO) '(TS OB))
						  (equal (getf y :MUSO) '(OB LE TS))))))))
		
(defun is-common-verb (curv)
    (and curv
	(or (find (list curv "-H") dat-verbi-disc-complete :test 'equal)
	    (find (list curv "+H") dat-verbi-disc-complete :test 'equal)
		(find (list curv "-L") dat-verbi-disc-complete :test 'equal)
		(find (list curv "+L") dat-verbi-disc-complete :test 'equal)
		(find (list curv "N") dat-verbi-disc-complete :test 'equal)
		(find (list curv "#") dat-verbi-disc-complete :test 'equal))))
	

(defun verbo-scartato (analv)
;;TODO: keep PARTICIPLE-PRESENT?

	(let* ((primainterp (caadr (car analv)))
		   (recordv (gethash (getf primainterp :ID) (eval (cat-to-diz (getf primainterp :CAT))))))

		(skip-verb recordv)))
	
	

(defun scarta (y)
	(or (find 'NO (getf y :MUSO))
		(and (not (find 'DISC (getf y :FONTE)))
			 (or (equal (getf y :MUSO) '(BU LE))
			     (equal (getf y :MUSO) '(LE BU))
				 (equal (getf y :MUSO) '(BU OB))
				 (equal (getf y :MUSO) '(OB BU))
				 (equal (getf y :MUSO) '(OB LE))
				 (equal (getf y :MUSO) '(LE OB))
				 (equal (getf y :MUSO) '(LE RE))
				 ;(equal (getf y :MUSO) '(BU))
				 (equal (getf y :MUSO) '(RE LE))))
				 
		;(and (equal (getf y :FONTE) '(DEVOLI))
		(and (not (find 'DISC (getf y :FONTE)))
			 (not (find 'FO (getf y :MUSO)))
			 (not (find 'CO (getf y :MUSO)))
		     (an-verbo (getf y :LEX))
			 (not (verbo-scartato (an-verbo (getf y :LEX))))
			 (print (getf y :LEX)))))


(defun make-dict ()
	
	(with-open-file (fout "dictionary_of_forms.it" :direction :output :if-exists :overwrite :if-does-not-exist :create)
			
		; NOUN / ADJ / ADV
		
		(when t
			(format t "~%;; Create forms for NOUN, ADJ, ADV...~%")
			(let ((pb (tpbar-init (hash-table-count |LEMMI|))))		
				(maphash #'(lambda(x y) (progn
					(tpbar-inc pb)
					(tpbar-print pb)
				
					(if (scarta y) nil
					
						(progn (let (superlativo super)
						
							;---------------
							;; NOUN, ADJ
							
                            (dolist (cat '(200 290 400 490)) (let ((blocca-participio nil))
							;(dolist (cat '(200 290 400)) (let ((blocca-participio nil))
							
								;; TODO 4maggio2010 se categoria e' SOLO 400 e non ha fonte DISC ed e' anche un verbio al participio passato 
								(when (and (not (find 'DISC (getf y :FONTE))) (equal cat 400))
									(let ((res-an-verbo (an-verbo (getf y :LEX))))
										(when res-an-verbo
											(dolist (cur res-an-verbo)
												(dolist (sub-cur (getf cur :V))
												
													(when (and (equal (getf sub-cur :MDV) 4) (equal (getf sub-cur :TMP) 3))
												
														(when (not (skip-verb y)) ; aprile 2012
															(setf blocca-participio t))
														
														))))))
									
									
							
								(let ((tmpbuffer nil))
							
									(dolist (gen-num '((1 1) (1 2) (2 1) (2 2)))
									
										;---------------------------------------------
										; se aggettivo provare a generare superlativo
										(when (and (equal gen-num '(1 2)) (equal cat 400)) ; maschile plurale + ssimo/a/i/e
											
											(dolist (gen-num-super '((1 1) (1 2) (2 1) (2 2)))
												(cond 
													((equal gen-num-super '(1 1)) (setf super "ssimo"))
													((equal gen-num-super '(1 2)) (setf super "ssimi"))
													((equal gen-num-super '(2 1)) (setf super "ssima"))
													((equal gen-num-super '(2 2)) (setf super "ssime")))
										
												(dolist (flessione (gen-lemma x cat (nth 0 gen-num) (nth 1 gen-num)))
													(when (and (getf flessione :FORMA) (not (search " " (getf flessione :FORMA))) (not (search "-" (getf flessione :FORMA))))
														(if (equal (right-str (getf flessione :FORMA) 1) "e")
															(setf superlativo (format nil "~ai~a" (subseq (getf flessione :FORMA) 0 (1- (length (getf flessione :FORMA)))) super))
															(setf superlativo (format nil "~a~a" (getf flessione :FORMA) super)))
														
														(if (get-lemma superlativo)
															(progn
																;(format fout "(\"~a\" ((:lex \"~a\" :cat ~a :mdv ~a :tmp ~a :prs ~a :num ~a :gen ~a dgr ~a)))~%" 
																
																(setf tmpbuffer (append tmpbuffer
																	(string-to-list-2
																		(format nil "(\"~a\" ((:lex \"~a\" :cat ~a :mdv ~a :tmp ~a :prs ~a :num ~a :gen ~a :gcase ~a dgr ~a)))"  ; ~%
																			(replace-str superlativo " " "_") (replace-str (getf y :lex) " " "_") 400 0 0 3 (nth 1 gen-num-super) (nth 0 gen-num-super) 0 5))
																	
																	))
																	
																; colgo occasione per generare avverbio di grado superlativo assoluto
																; tramite l'aggettivo s.ass. al femminile singolare + mente
																(when (equal gen-num-super '(2 1))
																
																	
																	(let ((avverbio (first (last (remove-duplicates (gen-lemma x 300) :test 'equal)))))
																		
																		(when (and (getf avverbio :FORMA) (not (search " " (getf avverbio :FORMA))) (not (search "-" (getf avverbio :FORMA))))										
																	
																			;(format fout "(\"~amente\" ((:lex \"~a\" :cat ~a :mdv ~a :tmp ~a :prs ~a :num ~a :gen ~a :dgr ~a)))~%" 
																			(setf tmpbuffer (append tmpbuffer
																				(string-to-list-2
																					(format nil "(\"~amente\" ((:lex \"~a\" :cat ~a :mdv ~a :tmp ~a :prs ~a :num ~a :gen ~a :gcase ~a :dgr ~a)))" ; ~%
																						(replace-str superlativo " " "_") (replace-str (getf avverbio :FORMA) " " "_") 300 0 0 0 0 0 0 5))
																					
																				))
																				
																				)
																
																		)))
																		
															nil ;(format t "~a | ~a -> NO~%" (getf flessione :FORMA) superlativo)
															)
													))
											))
										
										(when (not blocca-participio)
											(dolist (flessione (gen-lemma x cat (nth 0 gen-num) (nth 1 gen-num)))
												(when (getf flessione :FORMA)
													;(setf cur-ret (format fout "(\"~a\" ((:lex \"~a\" :cat ~a :mdv ~a :tmp ~a :prs ~a :num ~a :gen ~a :dgr ~a)))~%" 
												
													(setf tmpbuffer (append tmpbuffer 
														(string-to-list-2
															(setf cur-ret (format nil "(\"~a\" ((:lex \"~a\" :cat ~a :mdv ~a :tmp ~a :prs ~a :num ~a :gen ~a :gcase ~a :dgr ~a)))"  ;~%
																(replace-str (getf flessione :FORMA) " " "_") (replace-str (getf y :lex) " " "_") cat 0 0 3 (getf flessione :num) (getf flessione :gen) 0 (if (getf flessione :SEM) (getf flessione :SEM) 0) )))
													
														)))))
													
													
													)
												
												
									;########################################################################
									;####### PER OGNI CATEGORIA RAGGRUPPO GENERI E NUMERI E SALVO SU FILE
									;########################################################################
									
									(when tmpbuffer

										(setf tmpbuffer (rewrite-morphology tmpbuffer cat))
									
										; print to file
										(dolist (cur tmpbuffer)
											(format fout "~a~%" cur))))))
						
							;---------------
							; AVVERBI
							(dolist (flessione (remove-duplicates (append (gen-lemma x 390) (gen-lemma x 300) (gen-lemma x 300 nil nil t)) :test 'equal))
								(when (and (getf flessione :FORMA) (not (search " " (getf flessione :FORMA))) (not (search "-" (getf flessione :FORMA))))
									;(format fout "~a|~a|~a|~a|~a|~a|~a|~a~%" 
									(format fout "(\"~a\" ((:lex \"~a\" :cat ~a :mdv ~a :tmp ~a :prs ~a :num ~a :gen ~a :gcase ~a :dgr ~a)))~%" 
										(replace-str (getf flessione :FORMA) " " "_") (replace-str (getf flessione :FORMA) " " "_") (rewrite-cat 300) "_" "_" "_" "_" "_" "_" "_")))
												
							
							))))) |LEMMI|)))
					
		
		(when t ; !!!!!
			(format t "~%;; Creo forme verbi irregolari...~%")
			(let ((pb (tpbar-init (hash-table-count |VERBI-IRR|))))
				(tpbar-inc pb)
				(tpbar-print pb)
					
				; Espansione verbi irregolari
				(maphash #'(lambda(x y) (progn
					(if (skip-verb y) nil
					
						(progn
							(list-verb-paradigm (append y (list :ID x)) nil fout t) ;; ultima t per il rewrite
							(list-verb-paradigm (append y (list :ID x)) t fout t))) ;; ultima t per il rewrite ; con clitici
				
					)) |VERBI-IRR|)))
				
				
		; TODO: generare superlativi assoluti di participio passato
		(when t ;!!!!
			(format t "~%;; Creo forme verbi regolari...~%")
			(let ((pb (tpbar-init (hash-table-count |VERBI|))))
			
				; Espansione verbi regolari
				(maphash #'(lambda(x y) (progn
				
					(tpbar-inc pb)
					(tpbar-print pb)
					
					(if (skip-verb y) nil
						(progn
							(list-verb-paradigm (append y (list :ID x)) nil fout t)
							(list-verb-paradigm (append y (list :ID x)) t fout t))) ; con clitici
					
					)) |VERBI|)))


				))
			

(defun error-not-normal-alfa (i-str)
	(let ((str (string-downcase (format nil "~a" i-str))) (newstr "") c)
		(loop for i from 0 to (1- (length str)) do
			(setf c (subseq str i (1+ i)))
			(unless (search c ".&abcdefghijklmnopqrstuvwxyz_-1234567890'")
				(break)
				
				))
		newstr))
		
        
(defun make-glue-dict-clclass-number ()
	(loop for i from 2 to 10000 do
		(format t "~a|~a|670|0|0|0|2|0~%" (getf (gen-clclass 670 i) :LEX) i)))


(defun split-dict ()	
	(setf nda (make-hash-table :test #'equal))
	(setf nda-lemmi (make-hash-table :test #'equal))
	(setf nda-attr (make-hash-table :test #'equal :size 200))
	
    (format t "Process functional words~%")
    
    (with-open-file (stream "functional_words.it")
		(process #'process-dict-line stream))

    (format t "Process punctuation~%")
    
    (with-open-file (stream "punctuations.it")
		(process #'process-dict-line stream))

    (format t "Process dictionary~%")
    
	(with-open-file (stream "dictionary_of_forms.it")
		(process #'process-dict-line stream))
	
	(with-open-file (fout "nda.lemma" :direction :output :if-exists :overwrite :if-does-not-exist :create)
		(maphash #'(lambda(x y)
			(format fout "~a~%" x)
			;(print (list y x) fout)
			) nda-lemmi))
			
	(with-open-file (fout "nda.attributes" :direction :output :if-exists :overwrite :if-does-not-exist :create)
		(maphash #'(lambda(x y)
			(let ((listina (read-from-string x)))
			
			;(format fout "~a~a" (subseq (format nil "~a" (nth 1 listina)) 0 1) #\Tab) ; POS principale
			
			;(if (> (length (format nil "~a" (nth 1 listina))) 1)
			;	(format fout "~a~a" (subseq (format nil "~a" (nth 1 listina)) 0 2) #\Tab) ; POS un po' più dettagliata
			;	(format fout "~a~a" (subseq (format nil "~a" (nth 1 listina)) 0 1) #\Tab) ; POS principale
			;	)
			
			(if (search "-" (format nil "~a" (nth 1 listina)) :test #'equal)
				(format fout "~a~a" (subseq (format nil "~a" (nth 1 listina)) 0 (search  "-" (format nil "~a" (nth 1 listina)) :test #'equal)) #\Tab)
				(format fout "~a~a" (format nil "~a" (nth 1 listina)) #\Tab)) ; POS principale
			
			(loop for i from 1 to (length listina) by 2 do
				(format fout "~a~a" (nth i listina) (if (< (1+ i) (length listina)) #\Tab ""))
					
				))
			(format fout "~%")
			;(print (list y x) fout)
			
			
			) nda-attr))
	
	;; alla fine di tutto azzero forme uguali a chiave e inserisco una chiave
	(with-open-file (fout "nda.dictionary" :direction :output :if-exists :overwrite :if-does-not-exist :create)
		(maphash #'(lambda(x y)
			
			(format fout "~a" x)
			
			(dolist (interp y)
					
				(format fout "~a~a~a" #\Tab (car interp) #\Tab)
					
				(dolist (anal (list (cdr interp)))
					
					(let ((primo t))
						(dolist (buf anal)
							
							(unless primo
								(format fout " | "))

							(let ((spazio ""))
								(dolist (buf2 buf)
								
									(format fout "~a~a ~a" spazio (first buf2) (second buf2))
									(setf spazio " ")))
							
							(setf primo nil)))))
				
				(format fout "~%")) nda)))
	
	
(setf nda nil) ; New Diz Ada
(setf nda-lemmi nil)
(setf nda-attr nil)
	
(defun process-dict-line (line)
	;(format t "~A~%" line)

	(let* ((record (car (string-to-list-2 (format nil "~a" line))))
		   (forma-orig (first record))
		   (forma-deacc (deacc forma-orig)))
		
		;(error-not-normal-alfa forma-deacc) ;; evito sorprese su accenti...
		;(print (list forma-deacc forma-orig))
		
		;; compongo dati
		(let (compongo)
			(dolist (dati (second record))
				(let (id-lex id-attr)
					
					; ottengo id univoco per (getf dati :LEX)
					(unless (setf id-lex (gethash (getf dati :LEX) nda-lemmi))
						(setf id-lex (setf (gethash (getf dati :LEX) nda-lemmi) (hash-table-count nda-lemmi))))
					
					; ottengo id univoco per (cddr dati)
					(unless (setf id-attr (gethash (format nil "~a" (cddr dati)) nda-attr))
						(setf id-attr (setf (gethash (format nil "~a" (cddr dati)) nda-attr) (hash-table-count nda-attr))))
						
					(push (list id-lex id-attr) compongo)))

			(if (assoc (intern forma-orig) (gethash forma-deacc nda))
				(progn
					(setf (cdr (assoc (intern forma-orig) (gethash forma-deacc nda)))
						(remove-duplicates (append (cdr (assoc (intern forma-orig) (gethash forma-deacc nda))) (list (reverse compongo))) :test 'equal)))
					
				(push (cons (intern forma-orig) (list (reverse compongo))) (gethash forma-deacc nda))))))
	

(defun process-dict-line-to-json (line fout)
	
    (setf wout fout)

	(let* ((record (car (string-to-list-2 (format nil "~a" line)))))
        (progn
           ; (format t "~A~%" line)
           
            (format wout "{")
            (format wout "\"form\": \"~a\"" (car record))
            (format wout ", ")
            (format wout "\"morpho\": ")
            (format wout "[")
            
            (setf m 0)
            
            (dolist (morpho (second record))
                (setf m (+ m 1))
                
                (format wout "{")
                (format wout "\"lemma\": \"~a\"" (getf morpho :LEX))
                (format wout ", \"pos\": \"~a\"" (getf morpho :CAT))
                
                (if (equal (getf morpho :MDV) '_)
                    (format wout ", \"mood\": null")
                    (format wout ", \"mood\": \"~a\"" (getf morpho :MDV)))
                
                (if (equal (getf morpho :TMP) '_)
                    (format wout ", \"tense\": null")
                    (format wout ", \"tense\": \"~a\"" (getf morpho :TMP)))
                    
                (if (equal (getf morpho :PRS) '_)
                    (format wout ", \"person\": null")
                    (format wout ", \"person\": \"~a\"" (getf morpho :PRS)))
                
                (if (equal (getf morpho :NUM) '_)
                    (format wout ", \"number\": null")
                    (format wout ", \"number\": \"~a\"" (getf morpho :NUM)))
                
                (if (equal (getf morpho :GEN) '_)
                    (format wout ", \"gender\": null")
                    (format wout ", \"gender\": \"~a\"" (getf morpho :GEN)))
                
                (if (equal (getf morpho :gcase) '_)
                    (format wout ", \"case\": null")
                    (format wout ", \"case\": \"~a\"" (getf morpho :gcase)))
                
                (if (equal (getf morpho :dgr) '_)
                    (format wout ", \"degree\": null")
                    (format wout ", \"degree\": \"~a\"" (getf morpho :dgr)))
                    
                (format wout "}")    
                	
                (unless (= (length (second record)) m)
                    (format wout ", "))
                    
                )
                
            (format wout "]")
            (format wout "}")
            (format wout "~%")
            
            ;(print record)
            
            )))
    
    
    
    
(defun dict-to-json ()

    (with-open-file (fout "dict-it.json" :direction :output :if-exists :overwrite :if-does-not-exist :create)
		    
        (format t "Process functional words~%")
        
        (with-open-file (stream "functional_words.it")
            (process-out #'process-dict-line-to-json stream fout))

        (format t "Process punctuation~%")
        
        (with-open-file (stream "punctuations.it")
            (process-out #'process-dict-line-to-json stream fout))

        (format t "Process dictionary~%")
        
        (with-open-file (stream "dictionary_of_forms.it")
        	(process-out #'process-dict-line-to-json stream fout))
            
            ))
        
;;; Process stream, calling per-line-fn on each line of the stream
(defun process (per-line-fn stream)
	(let ((line (read-line stream nil)))
		(if (not (null line))
			(progn (funcall per-line-fn line)
			(process per-line-fn stream)))))

(defun process-out (per-line-fn stream wout)
	(let ((line (read-line stream nil)))
		(if (not (null line))
			(progn (funcall per-line-fn line wout)
			(process-out per-line-fn stream wout)))))


;;; to generate the dictionary, call the functions in sequence

(defmacro aa () '(load "make-dict"))
(defmacro bb () '(make-dict))
(defmacro cc () '(split-dict))
(defmacro dd () '(dict-to-json))
