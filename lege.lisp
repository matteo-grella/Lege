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

(load "utils") ; funzioni di utility
(load "ddi")   ; modulo di accesso diretto al dizionario
(load "make-dict")

(defun lege-image()
	(sb-ext:save-lisp-and-die "lege-img.mem" :save-runtime-options t :purify t))

(defun lege-compile(&optional (function-name 'function-server-start))
    ;(ext:saveinitmem "lege" :init-function function-name :executable t :quiet t)
	(sb-ext:save-lisp-and-die "lege" :save-runtime-options t :purify t :executable t :toplevel function-name))
	
;*******************************************************
;*** Inizializzazione del sistema e salvataggio immagine
(defun start ()

    (setf sb-impl::*default-external-format* :UTF-8)

	(unless (probe-file "lege-img.mem")
		;*** Creazione e salvataggio dati
		(when (y-or-n-p "Primo avvio di lege. Confermi avvio delle procedure di caricamento dati e creazione immagine?")
			(time (load-ddi))
			(lege-view-memory)
			(lege-image)
			(quit))))


(defun lege-view-memory ()
	(format t "Numero totale lemmi: ~a ~%"
		(+ (hash-table-count |LEMMI|) (hash-table-count |VERBI|) (hash-table-count |VERBI-IRR|) (hash-table-count |CLCLASS|)))
	(format t "- Verbi: ~a ~%"
		(+ (hash-table-count |VERBI|) (hash-table-count |VERBI-IRR|)))
	(format t "- Classi chiuse: ~a ~%"
		(hash-table-count |CLCLASS|))
    nil)

;****************************************
;; demo usage of useful top functions
(defun demo ()
    ;; main morphological analyzer
    (analyze "Lei suona il piano e lui la tromba")
    ;; convert string in number
    (print (strnum "quarantadue"))
    ;; convert number in string
    (print (strnum 42))
    ;; analyze verbs
    (print (get-verbo "programmo"))
    ;; analyze compound form verb+clt
    (print (an-verbo "chiamala"))
    ;; analyze nouns/adjs/advs
    (print (get-lemma "infermiere"))
    ;; coniugate a known lemma (e.g. 29930 = infermiere)
    (print (gen-lemma 29930 200 1 2))
    ;; analyze functional words
    (print (an-clclass "della"))
    ;; create all conjugated variants of a particular verb (if verb doesn't exist it ask some information to user)
    (mconiuga "amare"))

;****************************************
;*** Modalita' console
(defun lege-console()
	;(setf *print-case* ':downcase) ; imposta l'output della console in minuscolo
  	(loop (let ((in (prompt-read "> ")))
		(if (> (length in) 0)
            (progn
                (princc t (make-string 60 :initial-element #\-) t)
                (analyze in)            
                (princc t (make-string 60 :initial-element #\-) t t))
			(return-from lege-console)))))



;****************************************
(defun pre-format (testo)
	(setq testo (concatenate 'string " " testo))
	;------------------- Analisi definizioni
	(setq testo (replace-str testo " qlcu." " qualcuno"))
	(setq testo (replace-str testo " qlco." " qualcosa"))
	(setq testo (replace-str testo " sim." " simili"))
	(setq testo (replace-str testo " sing." " singolare"))
	(setq testo (replace-str testo " spec." " specialmente"))
	(setq testo (replace-str testo " inf." " infinito"))
	(setq testo (replace-str testo " loc. avv." " locuzione avverbiale"))
	(setq testo (replace-str testo " loc." " locuzione"))
	(setq testo (replace-str testo " avv." " avverbio"))
	;(setq testo (replace-str testo " ecc." " e altri"))
	(setq testo (string-trim " " testo ))
	
	testo)

(defun make-wlist (testo)

	;;
	;; Creazione della wlist
	;;
	(let ((SEPARATORI_FRASE	"!?.;:") ; La funzione gestisce i segni di interpunzione intesi come segni di fine frase
		  (CONGIUNZIONI 	",")
		  (PUNTEGGIATURA 	"\"()[]{}<> ")
		  (SIMBOLI 			"£$%&/\\=+*#@°-_'") ; EUR
		  (ALFABETO 		"abcdefghijklmnopqrstuvwxyz")
		  (ALFABETO_ACCENTI	"àáèéìíòóùú")
		  (NUMERI 			"0123456789")
		  ;-----------------------------------------
		  (wlist nil) ; lista di ritorno, formato: (("a" "b" "c" ".") ("x" "y" "z" "!"))
		  (frase nil) ; una singola frase, da inserire in wlist, formato: ("a" "b" "c" ".")
		  (parola "") ; una singola parola (stringa), da inserire in frase
		  cur		  ; carattere corrente
		  succ		  ; carattere successivo
		  unisci)	  ; flag di utilità
				
		(setq testo (replace-str testo (string (code-char 9)) " "))
		(setq testo (replace-str testo (string (code-char 10)) " "))
		(setq testo (replace-str testo (string (code-char 13)) " "))
		
		(loop while (search "  " testo :test #'string-equal) do
			(setq testo (replace-str testo "  " " "))) ; riporto un solo spazio tra una parola ed un'altra

		(setq testo (string-trim " " testo ))
		
		(if (search (right-str testo 1) SEPARATORI_FRASE :test #'string-equal)
			(setq testo (concatenate 'string testo " "))
			(setq testo (concatenate 'string testo ". ")))
		
		(dotimes (i (length testo))
			
			(setq cur (string-downcase (subseq testo i (1+ i))))
			(setq succ (when (< i (1- (length testo))) (string-downcase (subseq testo (+ i 1) (+ i 2)))))

			(cond
				
				((search cur (concatenate 'string ALFABETO ALFABETO_ACCENTI NUMERI SIMBOLI) :test #'string-equal)
					(setq parola (concatenate 'string parola cur)) ; continuo a comporre la parola, carattere per carattere
				
					(when (and (equal cur "'") (search succ (concatenate 'string ALFABETO ALFABETO_ACCENTI) :test #'string-equal))
						(setf frase (append frase (list parola)))
						(setq parola "")))
						
				((search cur (concatenate 'string PUNTEGGIATURA CONGIUNZIONI) :test #'string-equal)
					
					(unless (equal parola "")
						(setf frase (append frase (list parola)))
						(setq parola ""))
						
					(unless (equal cur " ")
						(setf frase (append frase (list cur)))))
					
				((search cur (concatenate 'string SEPARATORI_FRASE) :test #'string-equal)
					
					; Decido se unire alla parola o meno in base al primo carattere che trovo che sia diverso da SEPARATORI_FRASE+CONGIUNZIONI
					(setq unisci nil)
					
					(block ricerca-prossimo-separatore
						(loop for j from (1+ i) to (1- (length testo)) do
							(unless (search (subseq testo j (1+ j)) (concatenate 'string SEPARATORI_FRASE) :test #'string-equal)
								(unless (equal (subseq testo j (1+ j)) " ") (setq unisci t))
								(return-from ricerca-prossimo-separatore))))
				
					(if unisci
						(setq parola (concatenate 'string parola cur))
						(progn
							(unless (equal parola "")
								(setf frase (append frase (list parola)))
								(setq parola ""))
								
							(unless (equal cur " ")
								(setf frase (append frase (list cur)))
								(setf wlist (append wlist (list frase)))
								(setf frase nil)))))
					
				(t ; Carattere non riconosciuto
					(setq parola (concatenate 'string parola cur)))))
						
			wlist))


;**********************************************************************************+
;*** La pila restituita da make-pila è il risultato dell'analisi morfologica
;    della wlist. 
;
;*** Ogni elemento ha un next-index nel formato (pcnt cnt)
;
(defun make-pila (wlist)

	;*** Creazione della pila
	(let ((pila nil)
		  (pila-prop nil)
		  (fast-ddi (make-hash-table :test #'equal))
		  ; ---
		  cnt-next
		  level-1
		  level-pointer
		  levels-other
		  max-w2
		  num-new-level
		  str-loc
		  te
		  wrd)
		  
		;*** Scorro le frasi dalla wlist
		(dolist (prop wlist)
			(setf pila-prop nil)
			
			;; Raccolgo modalita' enunciativa della frase
			;; La forma linguistica di un enuciato spesso non consente di distinguere i due tipi di funzione illocutiva 
			;; (interrogazione o comando), ed è quindi una limitazione arbitraria che qui si porrà al sistema,
			;; quella di considerare ogni enunciato di tipo interrogativo come una richiesta di informazione. 
			;; (es. Vuoi chiudere la porta? sai che ore sono?)
			(setq te  (cond ((equal (nth (1- (length prop)) prop) "?") 2) ; interrogativa
							((equal (nth (1- (length prop)) prop) "!") 7) ; imperativa (corrisponde a mdv imperativo dei verbi)
							(t #|. ;|# 1)))							  	  ; assertiva
			
			; Hash-table che userò per memorizzare le posizioni delle locuzioni
			(let ((|memo-locuzioni| (make-hash-table :test #'equal)))
			
				
				;*** Scorro le parole della frase (w indicatore di wrd)
				(loop for wcnt from 0 to (- (length prop) 2) do
					(setq wrd (nth wcnt prop)) ; parola

					;-----------------------------------------------------
					(let (cv cn cc cost-vnc)
					;-----------------------------------------------------
				
						;*** Analisi morfologica delle parole (wrd)
						(if (setf cost-vnc (gethash wrd fast-ddi))
							(progn ; separo i costituenti in cv cn cc
								(setf cv (getf cost-vnc :cv))  ; analisi verbi
								(setf cn (getf cost-vnc :cn))  ; analisi nomi aggettivi e avverbi
								(setf cc (getf cost-vnc :cc))) ; analisi classi chiuse
								
							(progn ; ricavo i costituenti dalle analisi in ddi e aggiungo analisi in fast-ddi
								(setf cv (if (or (> (length wrd) 1) 
												 (equal wrd "e'")
												 (equal wrd "è")
												 (equal wrd "é"))
											  (an-verbo wrd) ; analisi verbi
											  nil))    
									
								(setf cn (if (> (length wrd) 1) (get-lemma wrd) nil))   ; analisi nomi aggettivi e avverbi (solo se (length wrd) > 1, quindi potrei eliminare quelli inferiori da dat-lemmi)
								(setf cc (an-clclass wrd))  ; analisi classi chiuse
								(setf (gethash wrd fast-ddi) (list :cv cv :cn cn :cc cc))))
									
						;*** Se non ho riconosciuto la categoria morfologica, ipotizzo che sia un nome comune o proprio
						(when (and (not cv) (not cn) (not cc))
							(setf cn (list (list :lex wrd :cat 203 :gen 3 :num 3 :forma wrd))))
						
						;*** Algoritmo per inserimento dei costituenti nella pila
						(if (equal wcnt (- (length prop) 2)) ; se e' l'ultima parola della frase
						
							(setq cnt-next nil) ; non punto ad alcuna parola successiva
						
							(progn ; diversamente calcolo quanti elementi nuovi dovrò inserire per poi calcolare il prossimo elemento a cui puntare
								(setq num-new-level 1) ; Di default ho un solo livello (riempito poi adeguatamente nel caso in cui nessuna analisi abbia dato esito positivo)
								
								;; Conteggio Costituenti nominali (cn)
								;; Solo primo livello (nessun calcolo particolare)
								;;
								;; Conteggio Costituenti verbali (cv)
								;; i :V sono sul primo livello; i :P su vari livelli successivi (attenzione ai pronomi da sdoppiare)s
								;;
								;;-----------------------------------------------------------
								(dolist (cur-cv cv) ; scorro la lista dei costituenti verbali
								;;-----------------------------------------------------------
								
									(when (getf cur-cv :p) ; ho almeno un pronome clitico
										(if (equal (getf (nth 0 (getf cur-cv :p)) :cat) 516)
											(setq num-new-level (+ num-new-level 2)) ; pronomi clitici doppi (es. "-sene" "-tele")
											(setq num-new-level (+ num-new-level 1))))) ; pronomi clitici normali, su un solo livello

								;***Conteggio Costituenti Classi Chiuse (cc)
								;   su tanti nuovi livelli quanti sono i pronomi da sdoppiare (se no primo livello)
								;;-----------------------------------------------------------------
								(dolist (cur-cc cc) ; scorro la lista dei costituenti classi chiuse
								;;-----------------------------------------------------------------
									
									(when (find (getf cur-cc :cat) '(516 810))
										(setq num-new-level (+ num-new-level 1))))
								
								(setq cnt-next (+ (length pila-prop) num-new-level))))
			
						(setf level-pointer (length pila-prop)) ;; Posizione corrente nella pila-prop (verrà man mano incrementata)

						;*** Aggiornamento locuzioni
						(dolist (update-loc (gethash wcnt |memo-locuzioni|))
							(setf (getf (nth (getf update-loc :sub-cnt) (nth (getf update-loc :cnt) pila-prop)) :cnt-next) (length pila-prop))
							(setf (getf (nth (getf update-loc :sub-cnt) (nth (getf update-loc :cnt) pila-prop)) :next-index) (length pila-prop)))
						
						;*** Inserimento degli elementi nella pila-prop
						(setf level-1 nil)
						(setf levels-other nil)
						
						;*** Inserimento Costituenti nominali (cn)
						;;-----------------------------------------------------------
						(dolist (cur-cn cn) ; scorro la lista dei costituenti nominali
						;;-----------------------------------------------------------
						
                            ; inserisco tutto al primo livello
							(setf level-1 (append level-1 (list 
								(append (list :wrd wcnt
											  :cnt (length pila-prop) 
											  :cnt-next cnt-next
											  :next-index cnt-next
											  :fon (fon (getf cur-cn :lex))) cur-cn)))))
												
						;*** Inserimento Costituenti verbali (cv)
						;;-----------------------------------------------------------
						(dolist (cur-cv cv) ; scorro la lista dei costituenti verbali
						;;-----------------------------------------------------------

							(setf (getf cur-cv :v) (mapcar #'(lambda (v) ; ciclo i verbi contenuti nel cv+pron
								
								; valenza
								(setf (getf v :VAL) 
									(remove-duplicates (getf (gethash (getf v :id) (if (equal (getf v :cat) 100) |VERBI| |VERBI-IRR|)) :VALENZA) :test 'equal))
								
								; integro nel costituente verbale la componente pragmatica della modalità di enunciazione
								(setf (getf v :te) te)
								
								v) (getf cur-cv :v)))
							
							;*** in presenza di pronomi
							(if (getf cur-cv :p) 
								
								;*** creo un nuovo livello per i pronomi clitici, se presenti (due livelli nel caso di pronomi doppi)
								(if (equal (getf (nth 0 (getf cur-cv :p)) :cat) 516)
									(progn ;*** pronomi clitici doppi
										(setq level-pointer (+ level-pointer 1))
										
										;*** verbo
										(setf level-1 (append level-1 (mapcar #'(lambda (x) 
												(append (list :wrd wcnt 
															  :cnt (length pila-prop) 
															  :cnt-next level-pointer
															  :next-index level-pointer
															  :fon (fon (getf x :lex))) x)) (getf cur-cv :v))))
																									
										(setq level-pointer (+ level-pointer 1))
										(let ((gclcl1 (gen-clclass (getf (nth 0 (getf cur-cv :p)) :cat1) (getf (nth 0 (getf cur-cv :p)) :id1)))
											  (gclcl2 (gen-clclass (getf (nth 0 (getf cur-cv :p)) :cat2) (getf (nth 0 (getf cur-cv :p)) :id2))))
													
												;*** primo elemento
												(setf levels-other (append levels-other (list (list 
													(append (list :wrd wcnt
																  :cnt (1- level-pointer) 
																  :cnt-next level-pointer
																  :next-index level-pointer
																  :fon (fon (getf gclcl1 :lex)) 
																  :clit t) gclcl1)))))

												;*** secondo elemento
												(setf levels-other (append levels-other (list (list 
													(append (list :wrd wcnt
																  :cnt level-pointer 
																  :cnt-next cnt-next
																  :next-index cnt-next
																  :fon (fon (getf gclcl2 :lex)) 
																  :clit t) gclcl2)))))))
									
									(progn ;** pronomi clitici singoli
										
										(setq level-pointer (+ level-pointer 1))
										
										(setf level-1 (append level-1 (mapcar #'(lambda (x) 
											(append (list :wrd wcnt
														  :cnt (length pila-prop) 
														  :cnt-next level-pointer
														  :next-index level-pointer
														  :fon (fon (getf x :lex)) 
														  :clit t) x)) (getf cur-cv :v))))
																									
										(setf levels-other (append levels-other (list (mapcar #'(lambda (x) 
											(append (list :wrd wcnt 
														  :cnt level-pointer 
														  :cnt-next cnt-next
														  :next-index cnt-next
														  :fon (fon (getf x :lex)) 
														  :clit t) x)) (getf cur-cv :p)))))))

								
								;*** verbi senza pronomi clitici
								(setf level-1 (append level-1 (mapcar #'(lambda (x) 
									(append (list :wrd wcnt 
												  :cnt (length pila-prop) 
												  :cnt-next cnt-next
												  :next-index cnt-next
												  :fon (fon (getf x :lex))) x)) (getf cur-cv :v))))))
						
						
						;*** Inserimento Costituenti Classi Chiuse (cc)
						;;-----------------------------------------------------------
						(dolist (cur-cc cc) ; scorro la lista dei costituenti classi chiuse
						;;-----------------------------------------------------------
						
							(if (find (getf cur-cc :cat) '(516 810))
								(progn ; inserimento classe chiusa doppia
									(setq level-pointer (+ level-pointer 1))
									(let ((gclcl1 (gen-clclass (getf cur-cc :cat1) (getf cur-cc :id1)))
										  (gclcl2 (gen-clclass (getf cur-cc :cat2) (getf cur-cc :id2))))
											
											(setf level-1 (append level-1 (list 
												(append (list 
															  :no1wrd t ; 4marzo10, significa che non è una sola parola ma in realtà due
															  :wrd wcnt 
															  :cnt (length pila-prop) 
															  :cnt-next level-pointer
															  :next-index level-pointer
															  :fon (fon (getf gclcl1 :lex))) gclcl1))))
															  
											(setf levels-other (append levels-other (list (list 
												(append (list 
															  :no1wrd t ; 4marzo10, significa che non è una sola parola ma in realtà due
															  :wrd wcnt 
															  :cnt level-pointer 
															  :cnt-next cnt-next
															  :next-index cnt-next
															  :fon (fon (getf gclcl2 :lex))) gclcl2)))))))
															  
								(progn ; inserimento classe chiusa singola
									(setf level-1 (append level-1 (list 
										(append (list :wrd wcnt 
													  :cnt (length pila-prop) 
													  :cnt-next cnt-next
													  :next-index cnt-next
													  :fon (fon (getf cur-cc :lex))) cur-cc)))))))
						
						
						;*** Ricerca e memorizzazione di locuzioni Costituenti Nominali e locuzioni Costituenti Classi Chiuse
						(setq max-w2 ; indice dell'ultima parola da includere nella locuzione
							(if (> wcnt (- (- (length prop) 1) *max-len-loc*))
								(- (length prop) 2)
								(- (+ wcnt *max-len-loc*) 1)))
						
						(when (> max-w2 wcnt) ; se ho più di una sola parola
							(loop for w2cnt from (1+ wcnt) to max-w2 do ; scorro la wlist da (1+ wcnt) a max-w2
								
								(setq str-loc "") ; creo la stringa-locuzione accodando le parole della wlist da wcnt a w2cnt
								(loop for i from wcnt to w2cnt do
									(setq str-loc (concatenate 'string str-loc (nth i prop) (when (< i w2cnt) " "))))
								
								;; Lemmi
								(when (<= (- w2cnt wcnt -1) *max-lemmi-loc*) ; la lunghezza della locuzione rientra tra quelle possibili per i lemmi
									(dolist (loc-cn (get-lemma-loc str-loc))  ; analisi locuzione costituente nominale
										;(format t "~a ~a~%" (1+ w2cnt) (list :pos1 (length pila-prop) :pos2 (length level-1)))
										
										(setf (gethash (1+ w2cnt) |memo-locuzioni|)
											(if (gethash (1+ w2cnt) |memo-locuzioni|)
												(append (gethash (1+ w2cnt) |memo-locuzioni|) (list (list :cnt (length pila-prop) :sub-cnt (length level-1))))
												(list (list :cnt (length pila-prop) :sub-cnt (length level-1)))))
										
										(setf level-1 (append level-1 (list 
											(append (list :wrd wcnt 
														  :cnt (length pila-prop) 
														  :cnt-next nil
														  :next-index nil
														  :fon (fon (getf loc-cn :lex))) loc-cn))))))
								
								;; Classi chiuse
								(when (and cc (<= (- w2cnt wcnt -1) *max-clclass-loc*)) ; la locuzione deve iniziare con una classe chiusa (ovvero cc non nil) la lunghezza della locuzione rientra tra quelle possibili per le classi chiuse
									(dolist (loc-cc (get-clclass str-loc))  ; analisi locuzione costituente classi chiuse
										
										;(format t "~a ~a~%" (1+ w2cnt) (list :pos1 (length pila-prop) :pos2 (length level-1)))
										
										(setf (gethash (1+ w2cnt) |memo-locuzioni|)
											(if (gethash (1+ w2cnt) |memo-locuzioni|)
												(append (gethash (1+ w2cnt) |memo-locuzioni|) (list (list :cnt (length pila-prop) :sub-cnt (length level-1))))
												(list (list :cnt (length pila-prop) :sub-cnt (length level-1)))))
										
										(setf level-1 (append level-1 (list 
											(append (list :wrd wcnt 
														  :cnt (length pila-prop) 
														  :cnt-next nil
														  :next-index nil
														  :fon (fon (getf loc-cc :lex))) loc-cc))))
										
										; alla chiave (1+ w2cnt) memorizzo (:info xyzkmn)
										; in level-1 memorizzo ciscun loc-cc con ciascuno aggiunti: (:wrd wcnt :cnt (length pila-prop) :cnt-next -1)
										; attenzione che se una locuzione va fino in fondo alla frase, (1+ max-w2) non verrà mai raggiunto. Occorre
										; controllare quando sono all'ultima parola, di fare il get anche del successivo e far puntare i cnt-next a nil
										)))) ;; // fine inserimento locuzioni
						
						(when level-1 (setf pila-prop (append pila-prop (list level-1))))
						(when levels-other (setf pila-prop (append pila-prop levels-other))))))

		(let (pp-num)
			(dolist (pp pila-prop)
				(let ((i 0))
					(setf pp (mapcar #'(lambda (_) (progn (setf (getf _ :SUB-CNT) i) (incf i) _)) pp))
					(setf pp-num (append pp-num (list pp)))))
			(setf pila-prop pp-num))
		
		(setf pila (append pila (list pila-prop)))) pila)) ; ritorno la pila
	


	
;; approssima la categoria sintattica in italiano dal numero della classe
(defun num-to-cat-approx (cat deprel)
	
	(cond	
		((search deprel "DEN QNT QNTA PSS DET") ; la categoria coincide con la relazione di dipendenza
			deprel)
		
		((find cat '(200 201 203 290 101))
			"NOUN")
			
		((find cat '(400 401 490))
			"ADJ")
			
		((find cat '(300 301 310 390))
			"ADV")
		
		((find cat '(100 110 120 130))
			"VERB")
			
		((find cat '(800 810))
			deprel)
		
		((and (>= cat 500) (< cat 600))
			"PROUN")
		
		((equal cat 710)
			"VIRG")
			
		((find cat '(711 712 713 714 715))
			"CONJ")))
			
		;((and (>= cat 715) (<= 752))
		;	"CONJ_C")
        
;************************************************************
;*** Modulo di lettura automatica
(defun analyze (testo)
	(let* ((testo1 (pre-format testo))		   ; preformattazione del testo (es. estensione delle sigle)
		   (wlist  (make-wlist testo1)) 	   ; creazione wlist
		   (pila   (make-pila wlist)))         ; creazione pila

        ;*** numero parole nella wlist
        ;(let ((n-word 0)) (dolist (x wlist) (incf n-word (1- (length x)))) (print (list 'n-word n-word)))  

        ;*** print pila
        ;(print pila)
        
        (format t "p,wrd,parola~%")
        (setf cnt-p 0)
        (dolist (w wlist)
                
            (setf cnt-wrd 0)
                (dolist (w2 w)
                    (format t "~a,~a,~a~%" cnt-p cnt-wrd w2)
                    (incf cnt-wrd))
                    
                (incf cnt-p))
                
        (format t "~%")
        (format t "p-cnt,wrd-cnt,te,cnt,cnt-next,cat,id,mdv,tmp,prs,num,gen,aux,tran,sem,lex,forma~%")
				
        (setf cnt-p 0)
        (dolist (p pila)
            (incf cnt-p)
            (dolist (p2 p)
                (dolist (p3 p2)
                    (let ((printstr (replace-str ; sostituisco NIL con -1
                        (format nil "~a,~a,~a,~a,~a,~a,~a,~a,~a,~a,~a,~a,~a,~a,~a,~a,~a~%" 
                            cnt-p (getf p3 :wrd) (getf p3 :te) (getf p3 :cnt) (getf p3 :cnt-next) 
                            (getf p3 :cat) (getf p3 :id) 
                            (getf p3 :mdv) (getf p3 :tmp) (getf p3 :prs) (getf p3 :num) (getf p3 :gen) 
                            (getf p3 :aux) (getf p3 :tran) (getf p3 :sem) (getf p3 :lex) (getf p3 :forma))
                            ",NIL" ",-1")))
                    (format t "~a" printstr)))))
                                
		nil))
        
