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
 
; DDI (Direct-Dictionary-Interface)

;; Legenda categorie (no classi-chiuse)
;;   200 : nome (comune)
;;   201 : nome (proprio)
;;   290 : nome critico
;;   300 : avverbio
;;   390 : avverbio critico
;;   400 : aggettivo
;;   490 : aggettivo critico
;;   100, 120, 130 : verbi


;  (setf i 0) (dolist (v *dat-verbi*) (incf i) (setf i (+ i 55)) (list-verb-paradigm v))

; (list-verb-paradigm (car (get-verbo "verificare" )) 1)
 
 
;*** Definizione dizionari
(defvar |VERBI-IRR| nil) ; verbi irregolari
(defvar |VERBI| nil)     ; verbi regolari
(defvar |LEMMI| nil)     ; lemmi (sostantivi aggettivi avverbi)
(defvar |CLCLASS| nil)   ; classi chiuse

;*** Definizione strutture per puntamento rapido ai dizionari
(defvar |VERBI-DES|       nil) ; key= des     value= (nth x *desinenze*)
(defvar |VERBI-RADICI| 	  nil) ; key= radice  value= (nth x *verbi*)
(defvar |VERBI-IRR-FORME| nil) ; key= forma   value= (nth-verbo nth-forma) in |VERBI-IRR|)
(defvar |LEMMI-RADICI| 	  nil) ; key= radice  value= (gethash x |LEMMI|)
(defvar |LEMMI-LOC| 	  nil) ; key= locuz   value= (gethash x |LEMMI|)
(defvar |CLCLASS-LEX| 	  nil) ; key= lex     value= (nth x *clclass*)
(defvar |CLCLASS-CAT| 	  nil) ; key= cat     value= ((nth x *clclass*) (nth x *clclass*) (nth x *clclass*) ...)

;*** Definizione variabili per gestione delle locuzioni
(defvar *max-lemmi-loc* 0)
(defvar *max-clclass-loc* 0)
(defvar *max-len-loc* 0)

;*******************************************************************************
;*** Restituisce il dizionario associato alla categoria specificata.
;    Se feval t restituisce direttamente il dizionario, diversamente il simbolo
(defun cat-to-diz (cat &optional (feval nil))
	(case cat 
		(100 ;*** verbi regolari e semi-regolari
			(if feval |VERBI| '|VERBI|))
		((120 130) ;*** verbi irregolari (o da controllare)
			(if feval |VERBI-IRR| '|VERBI-IRR|))
		((200 201 290 300 390 400 490) ;*** lemmi
			(if feval |LEMMI| '|LEMMI|))
		(t ;*** classi chiuse
			(if feval |CLCLASS| '|CLCLASS|))))
		
;****************************************************************
;*** Restituisce la lista delle classi contenute in un dizionario
(defun diz-to-cat (diz)
	(case diz 
		(|VERBI| '(100)) ; verbi regolari e semi-regolari
		(|VERBI-IRR| '(120 130)) ; verbi irregolari (o da controllare)
		(|LEMMI| '(200 201 290 300 390 400 490)) ; lemmi
		(|CLCLASS| (remove-duplicates (mapcar #'(lambda(x) (getf x :cat))  *clclass*) :test #'equalp)))) ; classi chiuse

;****************************************************
;*** elimina il separatore dalle sillabe, formando così un lemma con accenti corretti
(defun desill (sill)
	(replace-str sill "|" ""))

;****************************************************
;*** Verifica se l'argomento passato è una vocale (anche accentata)
(defun vocale(c)
	(if (find c '(#\a "a" #\à "à" #\á "á"
				  #\e "e" #\è "è" #\é "é"
				  #\i "i" #\ì "ì" #\í "í"
				  #\o "o" #\ò "ò" #\ó "ó"
				  #\u "u" #\ù "ù" #\ú "ú") :test #'equal) t nil))

;**********************************************************
;*** Verifica se l'argomento passato è una vocale accentata
(defun voc-acc(c)
	(if (find c '(#\à "à" #\á "á"
				  #\è "è" #\é "é"
				  #\ì "ì" #\í "í"
				  #\ò "ò" #\ó "ó"
				  #\ù "ù" #\ú "ú") :test #'equal) t nil))


;**************************************************
;*** Verifica se ci sono due consonanti consecutive

(defun 2-consonanti-uguali (str)
	 (and (> (length str) 1)
		  (not (vocale (left-str str 1)))
		  (equal (left-str str 1) (subseq str 1 2))))

(defun 2-consonanti (str) ; risegato il 24feb10
	 (and (> (length str) 1)
		  (not (vocale (left-str str 1)))
		  (not (vocale (subseq str 1 2)))))

;*****************************************************************
;*** Verifica se la stringa presenta almeno un carattere accentato
(defun has-acc (str)
	(or (search "à" str :test #'string-equal) (search "á" str :test #'string-equal)
		(search "è" str :test #'string-equal) (search "é" str :test #'string-equal)
		(search "ì" str :test #'string-equal) (search "í" str :test #'string-equal)
		(search "ò" str :test #'string-equal) (search "ó" str :test #'string-equal)
		(search "ù" str :test #'string-equal) (search "ú" str :test #'string-equal)))

;***********************************************************************************************
;*** Rimuove da una parola le vocali accentate, rimpiazzandole con le corrispettive vocali atone 
;*** Vengono eliminati gli apostrofi
(defun deacc (str)
	(setq str (replace-str str "à" "a")) (setq str (replace-str str "á" "a"))
	(setq str (replace-str str "è" "e")) (setq str (replace-str str "é" "e"))
	(setq str (replace-str str "ì" "i")) (setq str (replace-str str "í" "i"))
	(setq str (replace-str str "ò" "o")) (setq str (replace-str str "ó" "o"))
	(setq str (replace-str str "ù" "u")) (setq str (replace-str str "ú" "u"))
	
	(setq str (replace-str str "ç" "c")) ; inserito 12 gennaio 2010
	(setq str (replace-str str "ï" "i")) ; inserito 12 gennaio 2010
	(setq str (replace-str str "î" "i")) ; inserito 12 gennaio 2010
	(setq str (replace-str str "û" "u")) ; inserito 12 gennaio 2010
	(setq str (replace-str str "ñ" "n")) ; inserito 12 gennaio 2010
	(setq str (replace-str str "" "z"))
	(setq str (replace-str str "â" "a"))
	(setq str (replace-str str "ê" "e"))
	(setq str (replace-str str "ô" "o"))
	(setq str (replace-str str "ä" "o"))
	(setq str (replace-str str "ü" "u"))
	(setq str (replace-str str "ä" "a"))
	(setq str (replace-str str "ö" "o"))
	(setq str (replace-str str "ã" "a"))
	(setq str (replace-str str "å" "a"))

	
	(setq str (replace-str str "'" "")))

;**************************************************************************************
;*** Semplifica la parola sostituendo tutte le vocali con accento acuto con altrettante 
;    vocali con accento grave
;*** L'apostrofo dopo una vocale rende tale vocale accentata (sempre accento grave)
(defun semplacc (str)
	(setq str (replace-str str "á" "à")) (setq str (replace-str str "a'" "à"))
	(setq str (replace-str str "é" "è")) (setq str (replace-str str "e'" "è"))
	(setq str (replace-str str "í" "ì")) (setq str (replace-str str "i'" "ì"))
	(setq str (replace-str str "ó" "ò")) (setq str (replace-str str "o'" "ò"))
	(setq str (replace-str str "ú" "ù")) (setq str (replace-str str "u'" "ù")))

;********************************************************************
;*** Sostituisce una vocale non accentata con una con l'accento grave
(defun accento-grave(s)
	(cond
		((equal s "a") "à")
		((equal s "e") "è")
		((equal s "i") "ì")
		((equal s "o") "ò")
		((equal s "u") "ù")))

;**************************************************************************************
;*** A seconda di dove cade l'accento nella sillabazione, classifica il tipo di parola:
;    TRONCA, PIANA, SDRUCCIOLA, BISDRUCCIOLA
(defun cadacc (sill)
	(let (cnt-sill pos-acc)		
		(when (> (length sill) 0)
			(setf cnt-sill 0)
			(setf pos-acc -1)

			(dotimes (i (length sill))
				(cond
					; conto il numero totale di sillabe
					((equal (char sill i) #\|)
					 (setf cnt-sill (1+ cnt-sill)))
					 
					; memorizzo la posizione della sillaba che contiene l'accento
					((find (char sill i) '(#\à #\á #\è #\é #\ì #\í #\ò #\ó #\ù #\ú) :test #'equal)
					 (setf pos-acc cnt-sill))))
			
			(cond ; determino dove cade l'accento
				((equal pos-acc -1) nil)
				((equal pos-acc cnt-sill) 'TRONCA)
				((equal pos-acc (- cnt-sill 1)) 'PIANA)
				((equal pos-acc (- cnt-sill 2)) 'SDRUCCIOLA)
				((equal pos-acc (- cnt-sill 3)) 'BISDRUCCIOLA)))))

;*************************************************************************
;*** Accordo Fonetico. L'articolo, il dimostrativo e altri elementi 
;    del determinante possono assumere forme diverse a seconda dell'iniziale
;    della parola successiva.
;*** Il loro campo FSC deve quindi trovare corrispondenze con il risultato di questa funzione fon()
;    per reputare valida un assemblaggio
;*** La situazione è in realtà resa ancora più complessa dalla diffusione dell'uso di una e la senza elisione
;    anche davanti a vocale (cfr. LEPSCHY, 1993, p.95). Si potrebbe render conto di questo fenomeno
;    assegnando a questi due articoli FON=0, il che sarebbe compatibile con il carattere non normativo della
;    nostra grammatica.
(defun fon (lex)
	(let* ((l1 (left-str lex 1))
		   (l2 (left-str lex 2))
		   (c2 (right-str l2 1))) 
		  
		(cond ((or (equal l1 "z")  (equal l1 "x")  (equal l1 "j") 	; z-  x-  j-
				   (equal l2 "gn") (equal l2 "ps") (equal l2 "pn") 	; gn- ps- pn-
				   (and (equal l1 "s") (not (vocale c2)))			; s- impura (s- + consonante)
				   (and (equal l1 "i") (vocale c2))) 3)				; i- + vocale
		
			  ((not (vocale l1)) 1) ; qualsiasi consonante non inclusa nei casi precedenti
		
			  (t 2)))) ; qualsiasi vocale (esclusa i- + vocale)

;***********************************************************************************************
;*** Elimina da risultati tutte le occorrenze che non concordano in base ad apostrofi ed accenti
;*** Nota del 10.09.09. Da verificare se non è necessaria una differenziazione dei criteri di concordanza
(defun remove-by-acc (str risultati)

	(let ((sempl-str (semplacc str)))
	
		(remove-if-not #'(lambda (cur)
			;(setf sempl-cur (semplacc (getf cur :forma)))
		
			(case (getf cur :cat)
				(100 ; verbi regolari e semi-regolari
					(equal sempl-str (semplacc (getf cur :forma)))) ; eguaglianza esatta (semplificata)
				((120 130) ; verbi irregolari (o da controllare)
					(equal sempl-str (semplacc (getf cur :forma)))) ; eguaglianza esatta (semplificata)
				((200 201 290 300 390 400 490) ; lemmi
					(equal sempl-str (semplacc (getf cur :forma)))) ; eguaglianza esatta (semplificata)
				(t ; classi chiuse
					(equal sempl-str (semplacc (getf cur :lex)))))) ; eguaglianza esatta (semplificata)
			
			risultati)))

#|
Nota. se un lemma non posso generarlo al plurale dalle procedure
e nella sua descrizione ha solo il singolare, allora genero come
se fosse inv anche il plurale?

Non si trovano:
(:LEX "indarno" :SILL "in|dàr|no" :CAT "490") 
(:LEX "cellai" :SILL "cel|là|rio" :CAT "490") 
|#

;****************************************************************************************
;*** Crea un'unica radice univoca per un nome o un aggettivo (nil in caso di fallimento)
;*** Nel caso di numero 3, ovvero invariabile, viene restituito nil per indicare che non
;    esiste una radice ma e' da considerarsi il lemma stesso
(defun crea-radice (lex num)
	(let (; parte sinistra di str 
		  (l1 (left-str lex (- (length lex) 1)))
		  (l2 (left-str lex (- (length lex) 2)))
		  ;
		  ; parte destra di str
		  (r1 (right-str lex 1))
		  (r2 (right-str lex 2))
		  (r3 (right-str lex 3)))

		(cond
			((or (equal num 0) (equal num 1)) ; Indefinito o Singolare
				(when (or (equal r1 "a") (equal r1 "o") (equal r1 "e"))
					  l1))
			 
			((equal num 2) ; Plurale
				(cond
					((or (equal r3 "chi") (equal r3 "che") (equal r3 "ghi") (equal r3 "ghe"))
					 l2)

					((or (equal r2 "ce") (equal r2 "ge")) ; quindi anche "sce"
					 (concatenate 'string l1 "i"))
					 
					((or (equal r3 "cie") (equal r3 "gie"))
					 l1)
					 
					((or (equal r1 "i") (equal r1 "e"))
					 l1))))))

(defun load-lemmi ()

	(setq *max-lemmi-loc* 0)
	
	; Metto tutti i *dat-lemmi* in |LEMMI|
	(setf |LEMMI| (make-hash-table :test #'equal))
	(let ((i 0))
		(dolist (lm *dat-lemmi*)
			(setf (gethash i |LEMMI|) lm)
			(incf i)))
	
	
	(let ((pb (tpbar-init (hash-table-count |LEMMI|)))) ; dimensione barra
	
	; scorro tutti i lemmi
	(loop for i from 0 to (1- (hash-table-count |LEMMI|)) do 
		(tpbar-inc pb)
		(tpbar-print pb)
				
		; per ogni lemma scorro la sua descrizione morfologica
		(dolist (mo (getf (gethash i |LEMMI|) :morph))
			; scorro le forme del caso morfologico corrente
			(dolist (fo (getf mo :forme))
				(when (getf fo :forma) ; diverso da nil
					(setf (getf fo :forma) (string-downcase (getf fo :forma)))) ; Metto in minuscolo tutte le forme!
				
                ; nuova gestione del 24 febbraio 2013
                (when (find 'ES (getf (gethash i |LEMMI|) :MUSO))
                    (setf (getf fo :num) 3))
                
				; conto massimo numero di parole che compongono una locuzione
				(let ((cnt-loc (1+ (count-char (getf fo :forma) #\Space))))
					(when (< *max-lemmi-loc* cnt-loc) (setq *max-lemmi-loc* cnt-loc))
				
					; se e' presente almeno un spazio salvo il lemma come locuzione
					(if (> cnt-loc 1) 
					
						(setf (gethash (getf fo :forma) |LEMMI-LOC|)
							  (if (gethash (getf fo :forma) |LEMMI-LOC|) 
								  (remove-duplicates (append (gethash (getf fo :forma) |LEMMI-LOC|) (list i)) :test #'equal) ; aggiungo cur-rad ed evito i duplicati
								  (list i)))
				
						(let ((cur-rad (crea-radice (getf fo :forma) (getf fo :num)))) ; creo radice con il numero
							(unless ; se cur-rad e' nil significa che non e' possibile generare la radice,
									; che quindi corrisponde direttamente alla forma corrente del lemma
								cur-rad (setq cur-rad (getf fo :forma)))
							
							(setq cur-rad (deacc cur-rad)) ; elimino accenti e apostrofi
							
							(setf (gethash cur-rad |LEMMI-RADICI|)
								(if (gethash cur-rad |LEMMI-RADICI|) ; se in |LEMMI-RADICI| e' presente cur-rad
									(remove-duplicates (append (gethash cur-rad |LEMMI-RADICI|) (list i)) :test #'equal) ; aggiungo cur-rad ed evito i duplicati
									(list i))))))))))) ; altrimenti primo inserimento dell' nth di |LEMMI| in |LEMMI-RADICI|

;***************************************************
;*** Converte una parola nota da singolare a plurale
(defun sing-plur (str sill gen)
	(let (; parte sinistra di str 
		  (l1 (left-str str (- (length str) 1)))
		  (l2 (left-str str (- (length str) 2)))
		  (l3 (left-str str (- (length str) 3)))
		  (l4 (left-str str (- (length str) 4)))
		  ;
		  ; parte destra di str
		  (r1 (right-str str 1))
		  (r2 (right-str str 2))
		  (r3 (right-str str 3))
		  (r4 (right-str str 4)))
	
		  (cond
			
			;-----------------------------
			((equal r1 "a") ; Prima classe
			;-----------------------------
			
				(cond
					((or (equal r2 "ca") (equal r2 "ga")) ; gruppo ca/ga genero /chi/che/ghi/ghe
					 (case gen
						((0 1) (concatenate 'string l1 "hi"))   ; Maschile o Indefinito
						(2     (concatenate 'string l1 "he")))) ; Femminile
						
					((and (equal r4 "scia") (equal (right-str sill 3) "ì|a")) ; scia i tonica
					 (concatenate 'string l4 "scie"))
					 
					((and (equal r4 "scia")) ; scia - i atona
					 (concatenate 'string l4 "sce"))
					 
					((or (equal r3 "cia") (equal r3 "gia")) ; gruppo cia/gia genero /cie/ce/gie/ge
					 (cond
						((zerop (length sill)) ; se non ho sillabazione, genero la forma in "e"
						 (concatenate 'string l2 "e"))
						
						((equal (right-str sill 3) "ì|a") ; i tonica
						 (concatenate 'string l2 "ie"))
						
						((vocale (right-str l3 1)) ; i atona - preceduto da vocale -> ce/ge
						 (concatenate 'string l2 "ie"))
						 
						(t ; i atona - preceduto da consonante -> cie/gie
						 (concatenate 'string l2 "e"))))
					
					(t
						(case gen
							((0 1) (concatenate 'string l1 "i")) 	 ; Maschile 
							(2     (concatenate 'string l1 "e")))))) ; Femminile o Indefinito

			;-------------------------------
			((equal r1 "o") ; Seconda classe
			;-------------------------------
			
				(cond
					((or (equal r2 "co") (equal r2 "go")) ; gruppo co/go genero /chi/ci/ghi/gi
					 (cond
						((equal (cadacc sill) 'PIANA)
						 (concatenate 'string l1 "hi"))
						
						((equal (cadacc sill) 'SDRUCCIOLA)
						 (concatenate 'string l1 "i"))
						
						(t (concatenate 'string l1 "hi"))))
						 
					((equal r2 "io")
					 (cond
						((zerop (length sill)) ; se non ho sillabazione, genero la forma in "ii"
						 (concatenate 'string l2 "i"))
						
						((equal (right-str sill 3) "ì|o") ; i tonica
						 (concatenate 'string l2 "ii"))
						
						(t (concatenate 'string l2 "i")))) ; i atona
						 
					(t (concatenate 'string l1 "i"))))
			
			;-----------------------------
			((equal r1 "e") ; Terza classe
			;-----------------------------
			
				(if (equal r2 "ie")
					(concatenate 'string l2 "ie")
					(concatenate 'string l1 "i"))))))

;*****************************************************
;**** genero un maschile singolare
(defun masch-sing (lex gen num)
	(let ((r1 (right-str lex 1)))
		 (when (and (or (equal r1 "a") (equal r1 "o") (equal r1 "e") ; desinenza "a" "o" "e"
					    (equal num 3)) ; ammetto desinenze diverse soltanto nel caso di numero invariante
				    (or (equal gen 0) (equal gen 1) (equal gen 3))  ; genere indefinito o maschile o maschile-e-femminile
				    (or (equal num 0) (equal num 1) (equal num 3))) ; numero indefinito o singolare o invariante
			  lex)))

;*****************************************************
;**** genero un maschile plurale
(defun masch-plur (lex gen num &optional (sill ""))
	(let ((r1 (right-str lex 1)))
		;*** genere indefinito o maschile o maschile-e-femminile (non prevedo di generare un maschile da un femminile)
		(when (or (equal gen 0) (equal gen 1) (equal gen 3))
			(case num
				;*** numero indefinito o singolare (il genere diventa 1 (caso mai avessi avuto gen=3))
				((0 1) (sing-plur lex sill 1)) 
				;*** numero plurale
				(2 (when (or (equal r1 "i") (equal r1 "e")) lex))
				;*** numero invariante
				(3 lex)))))

;*****************************************************
;**** genero un femminile singolare
(defun femm-sing (lex gen num)
	
	(let ((l1 (left-str lex (- (length lex) 1))) 
		  (r1 (right-str lex 1)))
	
		(case num
			((0 1) ; numero singolare
				
				(case gen
					((0 1) ; genere maschile o indefinito
						(cond
							((equal r1 "o")
							 (concatenate 'string l1 "a"))
							 
							((equal r1 "e")
							 lex)))
						
					(2 ; genere femminile
						(when (or (equal r1 "a") (equal r1 "e"))
							lex))
					
					(3 ; genere maschile-e-femminile
						(cond
							((equal r1 "o")
							 (concatenate 'string l1 "a"))
							 
							((or (equal r1 "a") (equal r1 "e"))
							 lex)))))
				
			(3 ; numero invariante 
				lex))))

;*****************************************************
;**** genero un femminile plurale
(defun femm-plur (lex gen num &optional (sill ""))

	(let ((r1 (right-str lex 1))
		  (r2 (right-str lex 2))
		  plma plma-rad plma-des2)

		(case num
			((0 1) ; numero singolare
				(case gen
					((0 1) ; genere maschile o indefinito
						(setf plma (sing-plur lex sill gen)) ; generare il plurale maschile
						(setf plma-rad (left-str plma (- (length plma) 1)))
						(setf plma-des2 (right-str plma 2))
						
						(cond
							((and (equal r1 "o") (equal plma-des2 "ci") (equal r2 "co")) ; es: barbairco > barbarici > barbariche
							 (concatenate 'string plma-rad "he"))
							 
							((and (equal r1 "o") (equal plma-des2 "gi") (equal r2 "go")) ; es: coprofago > coprofagi > coprofaghe
							 (concatenate 'string plma-rad "he"))
							
							;; (21 maggio 2009) Correzione "vermiglio"->"vermiglie" / "vetrario"->"vetrarie"
							((and (equal r2 "io") (> (length plma) 2) (equal (right-str plma 1) "i") (not (vocale (left-str (right-str plma 2) 1))))
							 (concatenate 'string plma "e"))
							
							((equal r1 "o")
							 (concatenate 'string plma-rad "e"))
							
							((equal r1 "e")
							 plma)))
						
					((2 3) ; genere femminile o maschile-e-femminile
						(sing-plur lex sill 2)))) ; riconduco il genere a 2 (nel caso di gen=3)
				
			(2  ; numero plurale
				(case gen
					((0 1) ; genere maschile o indefinito
						nil) ; il maschile plurale finisce in "i" sia che appartenga alla prima che alla seconda che alla terza classe. non posso distinguere la 
						
					((2 3) ; genere femminile o maschile-e-femminile
						(when (or (equal r1 "i") (equal r1 "e"))
							lex))))
				
			(3  ; numero invariante
				lex))))

;*****************************************************
;*** Ipotizza delle radici per un nome o un aggettivo
(defun lemmi-eur-rad (str)
	(let (; parte sinistra di str 
		  (l1 (left-str str (- (length str) 1)))
		  (l2 (left-str str (- (length str) 2)))
		  ;
		  ; parte destra di str
		  (r1 (right-str str 1))
		  (r2 (right-str str 2))
		  (r3 (right-str str 3)))

		(cond
			((or (equal r3 "chi") (equal r3 "che") (equal r3 "ghi") (equal r3 "ghe"))
			 (list l1 l2))

			((or (equal r2 "ce") (equal r2 "ge")) ; quindi anche "sce"
			 (list l1 (concatenate 'string l1 "i")))
			 
			((or (equal r3 "cie") (equal r3 "gie"))
			 (list l1))
			 
			((or (equal r1 "i") (equal r1 "e") (equal r1 "a") (equal r1 "o"))
			 (list l1)))))

(defun gen-lemma (id-lemma cat &optional (gen nil) (num nil) (no-direct-access nil))
	
	(let (my-morph 
		  direct-forma
		  ricerca
		  (lemma (gethash id-lemma |LEMMI|)))
		
		(setf my-morph (car (remove-if-not #'(lambda (x) (equal (getf x :cat) cat)) (getf lemma :morph))))
		
		
		;*** proseguo se ho trovato una corrispondenza di "cat" o, diversamente, se sto tentando di generare un avverbio da un aggettivo
		(when (or my-morph (or (equal cat 300) (equal cat 390)))
				
			(setf direct-forma ; Cerco in my-morph le forme declinate esattamente con "gen" e "num" della richiesta
				(unless no-direct-access (remove-if-not #'(lambda (x) 
						(and (or (equal gen (getf x :gen)) 							 	; corrisponde il genere
								 (and (find gen '(0 1)) (find (getf x :gen) '(0 1)))	; se richiedo 0 o 1 cerco contemporaneamente sia 0 che 1
								 (and (find gen '(0 1 2)) (equal (getf x :gen) 3))) 	; se richiedo 0 o 1 o 2 cerco 3
								 
							 (or (equal num (getf x :num))							 	; corrisponde il numero
								 (and (find num '(0 1)) (find (getf x :num) '(0 1)))	; se richiedo 0 o 1 cerco contemporaneamente sia 0 che 1
								 (and (find num '(0 1 2)) (equal (getf x :num) 3)))))	; se richiedo 0 o 1 o 2 cerco 3
											
					; ricerco tra le forme della corrispondenza morph-cat trovata
					(getf my-morph :forme)))) 
			
			(if direct-forma
				
				;*** se ho trovato la parola così com'è, ritorno le corrispondenze trovate
				(mapcar #'(lambda (x) (list :cat cat :num num :gen gen :forma (getf x :forma) :sem (getf x :sem))) direct-forma)
				
				;*** se non ho trovato la parola così com'è, inizia la generazione algoritmica distinguendo i vari casi 
				(cond 
				
					;*** la richiesta di indefinito/maschile indefinito/singolare prevedo di averla solo in direct-forma
					;((and (find gen '(0 1)) (find num '(0 1))) nil) 
					
					;*** La richiesta di un sostantivo femminile indefinito/singolare prevedo di averla solo in direct-forma
					;((and (equal cat 200) (equal gen 2) (find num '(0 1))) nil)

					;*** Richiesta di indefinito/maschile plurale
					((and (find gen '(0 1)) (equal num 2)) 
					
						;;*** Creo la forma a partire da un maschile singolare
						(when (setf ricerca (remove-if-not #'(lambda (x) (and (find (getf x :gen) '(0 1 3)) (find (getf x :num) '(0 1 3)))) (getf my-morph :forme)))
							(mapcar #'(lambda (x) (list :cat cat :num 2 :gen 1 :forma
								(masch-plur (getf x :forma) (getf x :gen) (getf x :num) 
											(if (equal (getf x :forma) (getf lemma :lex)) (getf lemma :sill) "")) :sem (getf x :sem))) ricerca)))
						
					;*** SOSTANTIVI => Richiesta di femminile plurale
					((and (or (equal cat 200) (equal cat 290)) (equal gen 2) (equal num 2))
					
						;;*** Creo la forma a partire da un femminile singolare
						(when (setf ricerca (remove-if-not #'(lambda (x) (and (find (getf x :gen) '(2 3)) (find (getf x :num) '(0 1 3)))) (getf my-morph :forme)))
							(mapcar #'(lambda (x) (list :cat cat :num 2 :gen 2 :forma
								(femm-plur (getf x :forma) (getf x :gen) (getf x :num)
										   (if (equal (getf x :forma) (getf lemma :lex)) (getf lemma :sill) "")) :sem (getf x :sem))) ricerca)))
						
					;*** AGGETTIVI => Richiesta di femminile indefinito/singolare
					((and (or (equal cat 400) (equal cat 490)) (equal gen 2) (find num '(0 1)))

						;;*** Creo la forma a partire da un maschile singolare
						(when (setf ricerca (remove-if-not #'(lambda (x) (and (find (getf x :gen) '(0 1 3)) (find (getf x :num) '(0 1 3)))) (getf my-morph :forme)))
							(mapcar #'(lambda (x) (list :cat cat :num 1 :gen 2 :forma (femm-sing (getf x :forma) (getf x :gen) (getf x :num)) :sem (getf x :sem))) ricerca)))
					
					;*** AGGETTIVI => Richiesta di femminile plurale
					((and (or (equal cat 400) (equal cat 490)) (equal gen 2) (equal num 2))

						;;*** Creo la forma a partire da un femminile singolare
						(setf ricerca (remove-if-not #'(lambda (x) (and (find (getf x :gen) '(2 3)) (find (getf x :num) '(0 1 3)))) (getf my-morph :forme)))
						;;*** Oppure creo la forma a partire da un maschile singolare
						(unless ricerca (setf ricerca (remove-if-not #'(lambda (x) (and (find (getf x :gen) '(0 1 3)) (find (getf x :num) '(0 1 3)))) (getf my-morph :forme))))
						;;*** Oppure creo la forma a partire da un maschile plurale
						(unless ricerca (setf ricerca (remove-if-not #'(lambda (x) (and (find (getf x :gen) '(0 1 3)) (find (getf x :num) '(2)))) (getf my-morph :forme))))
												
						(when ricerca
							(mapcar #'(lambda (x) (list :cat cat :num 2 :gen 2 :forma
								(femm-plur (getf x :forma) (getf x :gen) (getf x :num)
										   (if (equal (getf x :forma) (getf lemma :lex)) (getf lemma :sill) "")) :sem (getf x :sem))) ricerca)))
											   
					;*** AVVERBI auto-generati
					((equal cat 300) 
						;(print id-lemma)
						(let (adv)
							
							(setf adv (mapcar #'(lambda (x) 
								
								(when x (list 	:cat 300 
												
												:forma 
													(cond ((or (equal (right-str (getf x :forma) 2) "le") (and (> (length (getf x :forma)) 3) (vocale (subseq (right-str (getf x :forma) 3) 0 1)) (equal (right-str (getf x :forma) 2) "re"))); eliminata "ra" * ma come faccio andare "leggermente" "?"?"?
																; La regola prevede l'apocope per le forme derivanti da aggettivi terminanti in -le o in -re (Gramm. Serianni, XII.7).
																(concatenate 'string (left-str (getf x :forma) (- (length (getf x :forma)) 1)) "mente")) ; apocope
															(t (concatenate 'string (getf x :forma) "mente"))) 
												
												:sem (getf x :sem))))
							
								
								(remove-if-not #'(lambda(_) (getf _ :FORMA)) (gen-lemma id-lemma 400 2 1)))) ; forzo la generazione del femminile singolare (forma estesa)
						
							;(print adv)
							adv))))))) 


;*****************************************************************************
;*** restituisce descrizione morfologica nel caso str formasse una locuzione
(defun get-lemma-loc (str)
	(let ((ret nil))		
		(dolist (id (gethash str |LEMMI-LOC|))
			(let ((lm (gethash id |LEMMI|)))
				(dolist (mo (getf lm :morph))
					(dolist (fo (getf mo :forme))
						(when (equal str (getf fo :forma))
							(push (list :cat (getf mo :cat) :id id :lex (getf lm :lex) :num (getf fo :num) :gen (getf fo :gen) :forma (getf fo :forma) :sem (getf fo :sem)) ret))))))
		ret))

;*************************************************
;*** restituisce descrizione morfologica del lemma
(defun get-lemma (str)
	(let ((str-deacc (deacc str))
		  (ret nil)
		  (id-lemmi nil)
		  cur-lemma
		  eur-agg
		  eur-super
		  masch-plur)
	
			
		;; Ricerco le radici nell'hash-table |LEMMI-RADICI|. Accodo in "id-lemmi" tutti i risultati individuati
		(dolist (cur-rad (remove-duplicates (cons str-deacc (lemmi-eur-rad str-deacc)) :test #'equal)) ; Genero le radici possibili (accodo anche la parola intera)
			(when cur-rad
				(dolist (i (gethash cur-rad |LEMMI-RADICI|))
					(push i id-lemmi))))
		
		;; Mi assicuro di non avere duplicati nei risultati
		(setf id-lemmi (remove-duplicates id-lemmi :test #'equal))
		
		;; Esamino id-lemmi
		(dolist (id id-lemmi)
			(setf cur-lemma (gethash id |LEMMI|))
			
			(dolist (mo (getf cur-lemma :morph))
			
				(case (getf mo :cat)
				
					;------------------------------------------
					((200 290 400 490) ; sostantivi e Aggettivi
					;------------------------------------------
						(dolist (gen-num '((1 1) (1 2) (2 1) (2 2))) ; tutte le combinazioni di genere-numero
							(dolist (cur-dec (gen-lemma id (getf mo :cat) (nth 0 gen-num) (nth 1 gen-num))) ; nil = no-direct-access; t = extended-output
								(when (equal (deacc (getf cur-dec :forma)) str-deacc)
									(push (list :cat (getf cur-dec :cat) :id id :lex (getf cur-lemma :lex) :num (getf cur-dec :num) :gen (getf cur-dec :gen) :forma (getf cur-dec :forma) :sem (getf cur-dec :sem)) ret)))))
				
					;------------------------------------------------
					((300 390) ; avverbi (ricerco identita' perfetta)
					;------------------------------------------------
					
						(dolist (fo (getf mo :forme))
							(when (equal (getf fo :forma) str-deacc)
								(push (list :cat (getf mo :cat) :id id :lex (getf cur-lemma :lex) :forma (getf fo :forma) :sem (getf fo :sem)) ret)))) ; ? :cat 300
				
					;--------------------------------------
					(t ; altro (ricerco identita' perfetta)
					;--------------------------------------
					
						(dolist (fo (getf mo :forme))
							(when (equal (getf fo :forma) str-deacc)
								(push (list :cat (getf mo :cat) :id id :lex (getf cur-lemma :lex) :num (getf fo :num) :gen (getf fo :gen) :forma (getf fo :forma) :sem (getf fo :sem)) ret)))))))
		
		
		
		(setf ret (remove-by-acc str ret))
		
		(cond
			
			;----------------------------------------------------------------------------------
			((equal (right-str str 5) "mente") ; terminazione in "-mente
			;----------------------------------------------------------------------------------
			
				(if (or (equal (right-str str 6) "lmente") (equal (right-str str 6) "rmente")) ; es: facilmente / perpendicolarmente
					
					(setf eur-agg (list (concatenate 'string (left-str str (- (length str) 5)) "e")
										(concatenate 'string (left-str str (- (length str) 5)) "a")
										;(concatenate 'string (left-str str (- (length str) 5)) "o") ; anche "o" per "leggermente"
										))
						
					(setf eur-agg (list (left-str str (- (length str) 5))))) ; tutti gli altri casi, incluso "-emente"
				
				(dolist (cur-eur-agg eur-agg)
					
					;; ricerco e seleziono solo gli aggettivi femminili singolari (ricorsione)
					(dolist (cur-analisi 
						(remove-if-not #'(lambda (x) (and (equal (getf x :cat) 400) 
														  (find (getf x :gen) '(2 3)) 
														  (find (getf x :num) '(0 1 3))))
								; selezione dalla generazione del lemma										   
								(get-lemma cur-eur-agg)))
						
						;; per ciascun aggettivo trovato, rigeneraro l'avverbio
						(dolist (cur-avverbio (gen-lemma (getf cur-analisi :id) 300 nil nil t)) ; nil = gen; nil = num; t = no-direct-access; t = extended-output
							
							;(print (list (getf cur-avverbio :forma) str :sem (getf cur-avverbio :sem)))
							
							;; Attenzione! controllo di corrispondenza da verificare! (altrimenti riconosce "allegramente" -> "allegroneamente") 
							;(when (equal (getf cur-avverbio :forma) str) ; controllo di coorrispondenza ; NOTA. inibito controllo l'8gennaio2010, e sostituito con il controllo di :SEM
							(when (equal (getf cur-avverbio :sem) 0)
								(push (list :cat 300 :id (getf cur-analisi :id) :lex (getf cur-analisi :lex) :forma (getf cur-avverbio :forma) :sem (getf cur-analisi :sem)) ret))))))
			
			;---------------------------------------------------------------------------------------------------------
			((find (right-str str 6) '("issimo" "issimi" "issima" "issime") :test #'equal) ; terminazione in "-issim*"
			;---------------------------------------------------------------------------------------------------------

				; TODO: gestire forme in o/a-si-ssim* (es multistadio | *multistadiossimo -NO, multistadiosissimo -SI)

				(let (supergen supernum)
					
					(cond ;*** Definisco genere e numero dal suffisso del superlativo assoluto
						((equal (right-str str 1) "o")
							(setf supergen 1) (setf supernum 1))
							
						((equal (right-str str 1) "i")
							(setf supergen 1) (setf supernum 2))
							
						((equal (right-str str 1) "a")
							(setf supergen 2) (setf supernum 1))
							
						((equal (right-str str 1) "e")
							(setf supergen 2) (setf supernum 2)))
					  
					(if (and (equal (left-str (right-str str 7) 1) "h") (find (right-str str 1) '("o" "a") :test #'equal)) ; terminazione in "hissimo" o "hissima"
						(setf eur-super (list (concatenate 'string (left-str str (- (length str) 7)) (right-str str 1)))) ; Taglio la "h"
						(setf eur-super (list (concatenate 'string (left-str str (- (length str) 6)) (right-str str 1)))))
					
					(when (or (equal (right-str str 1) "a") (equal (right-str str 1) "o")) ; "semplicissima" deriva da "semplice" !
						(push (concatenate 'string (left-str str (- (length str) 6)) "e") eur-super))
					
					(push (concatenate 'string (left-str str (- (length str) 6)) "i" (right-str str 1)) eur-super) ;(!! "latifondiarissimo")
					(push (left-str str (- (length str) 5)) eur-super) ; zooprofilattici
					
					;(print eur-super)
					;(print "---")
					
					;; ottengo la lista di id plurali (terminanti in -i, perciò o prima classe m.p. o seconda classe sia m.p. che f.p.)
					;(let ((sega (if (equal (right-str (left-str str (- (length str) 6)) 1) "i") 6 5)))
					(let ((sega 5))
					(setf masch-plur (mapcar #'(lambda(x) (getf x :id)) 
						(remove-if-not #'(lambda (x) (and (equal (getf x :cat) 400)
														  (or (equal (getf x :num) 2)
															  (equal (getf x :num) 3))))
							  
							  
							  (append (get-lemma (left-str str (- (length str) sega)))
									  (get-lemma  (concatenate 'string (left-str str (- (length str) 6)) "e"))
									  ;(get-lemma  (concatenate 'string (left-str str (- (length str) 6)) "o"))
									  ;(get-lemma  (concatenate 'string (left-str str (- (length str) 6)) "a"))
									  
									  )
							  
							  )))
					
					(setf masch-plur (remove-duplicates masch-plur :test 'equal))
					;(print (list (left-str str (- (length str) sega)) masch-plur))
					;(print "---")
					)
					
					; TODO:
					;(!! "latifondiariissimo")
					;("latifondiarie" "latifondiario") 
					;("latifondiarii" NIL) 
					; ->> se prima di issimo c'e' ancora una "i" segare non 5 ma 6 !

					;; ricerco e seleziono solo gli aggettivi
					(dolist (cur-analisi (remove-if-not #'(lambda (x) (and (equal (getf x :cat) 400)
					
																			;(or (equal (right-str (getf x :LEX) 1) "e") ; fortissimo -> fort[e] 
																			;	(and (equal (getf x :num) supernum)
																			;		 (equal (getf x :gen) supergen)))
																			; Nota. Inibito il 10 gennaio 2011 controllo morfologia
																					 ))
																			
											(let (ritorno)
												(dolist (cur-eur eur-super ritorno)
													(setf ritorno (append ritorno (get-lemma cur-eur))))
													
													
													;(print ritorno)
													)))
						
						;(print (list (getf cur-analisi :id) masch-plur))
						
						(when (find (getf cur-analisi :id) masch-plur)
							(push (append (list :cat 400 
												:id    (getf cur-analisi :id)
												:lex   (getf cur-analisi :lex)
												:num   supernum ;(getf cur-analisi :num)
												:gen   supergen ;(getf cur-analisi :gen)
												:forma (getf cur-analisi :forma)
												:sem   5)) ; 5 = superlativo
								ret))))
								
				(setf ret (remove-duplicates ret :test 'equal))
								))
								
		; Raggruppo il raggruppabile...
		(let (new-ret cur-el)
		
			;(print ret)
		
			(loop for i from 0 to (1- (length ret)) do ; scorro tutti i chunk analizzati
				(when (nth i ret) ; se 'nil' significa che l'ho già preso e rimosso
		
					(setf cur-el (nth i ret))
					
					(loop for j from (1+ i) to (1- (length ret)) do
						(when (and (nth j ret) (equal (getf cur-el :CAT) (getf (nth j ret) :CAT)) (equal (getf cur-el :ID) (getf (nth j ret) :ID)))
							(cond
								
								; generi diversi, stesso numero
								((and (not (equal (getf cur-el :GEN) (getf (nth j ret) :GEN))) (equal (getf cur-el :NUM) (getf (nth j ret) :NUM)))
									(setf (getf cur-el :GEN) 3)
									(setf (nth j ret) nil))
									
								; numeri diversi, stesso genere
								((and (not (equal (getf cur-el :NUM) (getf (nth j ret) :NUM))) (equal (getf cur-el :GEN) (getf (nth j ret) :GEN)))
									(setf (getf cur-el :NUM) 3)
									(setf (nth j ret) nil)))))
							
					(push cur-el new-ret)))
					
			;(print new-ret)
			(setf ret new-ret))
				
		ret))

;*****************************************************
;*** verifica se str corrisponde a un lemma in |LEMMI|
(defun get-lex-lex (str)
	(when (get-lemma str)
		(car (remove-if-not #'(lambda(_) (equal (getf _ :lex) str)) (get-lemma str)))))
		
		
(defun load-clclass ()
	(setq *max-clclass-loc* 0)

	; Metto tutti i *dat-lemmi* in |LEMMI|
	(setf |CLCLASS| (make-hash-table :test #'equal))
	(let ((i 0))
		(dolist (lm (remove-duplicates *clclass* :test #'equal))
			(setf (getf lm :FORMA) (getf lm :LEX)) ; aggiungo la forma che coincide con il LEX
			(setf (gethash i |CLCLASS|) lm)
			(incf i)))
	
	(let (def-id0)
		;*** scorro tutte le classi chiuse
		(loop for i from 0 to (1- (hash-table-count |CLCLASS|)) do 
		
			;*** conto massimo numero di parole che compongono una locuzione
			(let ((cnt-loc (1+ (count-char (getf (gethash i |CLCLASS|) :lex) #\Space))))
				(when (< *max-clclass-loc* cnt-loc) (setq *max-clclass-loc* cnt-loc)))
			
			(let (clex)
			
				(setq clex (deacc (getf (gethash i |CLCLASS|) :lex))) ; elimino accenti e apostrofi
				
				;*** se id=0 o nil definisco id
				(when (or (equal (getf (gethash i |CLCLASS|) :ID) 0) (not (getf (gethash i |CLCLASS|) :ID)))
					(if (assoc (getf (gethash i |CLCLASS|) :CAT) def-id0)
						(setf (getf (gethash i |CLCLASS|) :ID) (incf (cdr (assoc (getf (gethash i |CLCLASS|) :CAT) def-id0))))
						(push (cons (getf (gethash i |CLCLASS|) :CAT) (getf (gethash i |CLCLASS|) :ID)) def-id0)))
					
				;*** inserisco elemento nell'hash la classe chiusa
				(setf (gethash clex |CLCLASS-LEX|)
					(if (gethash clex |CLCLASS-LEX|)
						(append (gethash clex |CLCLASS-LEX|) (list i))
						(list i))))
						
				;*** inserisco elemento nella lista di hash puntate dal nome della classe
				(setf (gethash (getf (gethash i |CLCLASS|) :cat) |CLCLASS-CAT|)
					(append (gethash (getf (gethash i |CLCLASS|) :cat) |CLCLASS-CAT|) (list i)))

						)))
				

#|
 | Classe 670 : numerali cardinali {due, tre...}
 |
 | Indicano una quantità determinata (maggiore di uno) e introducono quindi dei SN plurali. Seguono
 | l'articolo determinativo o il dimostrativo, oppure -in un SN indeterminato- la forma nulla dell'articolo
 | (tre uomini) o il denotativo di alterità (altri tre uomini). Quest'ultimo può anche seguire il cardinale
 | (tre altri uomini).
 | Sono compatibili con il denotativo di identità (gli stessi due uomini) e con il possessivo (due miei
 | amici).
 | Possono assumere la funzione pronominale.
|#
	
(defun strumber-string-starts-with (str start)
	(and (> (length str) 0)
		 (>= (length str) (length start))
		 (equal (subseq str 0 (length start)) start)))
	
(defun strumber-string-ends-with (str end)
	(and (> (length str) 0)
		 (>= (length str) (length end))
		 (equal (subseq str (- (length str) (length end))) end)))

(defun strumber-vocale (str)
	(or (equal str "a") (equal str "e") (equal str "i") (equal str "o") (equal str "u")))

;; TODO: d(i)ecimiliardesimo
;;        ^^^
(defun 670->lex (num)

	(let ((strumber-unita '("" "uno" "due" "tre" "quattro" "cinque" "sei" "sette" "otto" "nove" "dieci" "undici" "dodici" "tredici" "quattordici" "quindici" "sedici" "diciassette" "diciotto" "diciannove"))
		  (strumber-decine '("" "" "venti" "trenta" "quaranta" "cinquanta" "sessanta" "settanta" "ottanta" "novanta"))
		  (strumber-oltre-mille '(("" "") ("mille" "mila") ("unmilione" "milioni") ("unmiliardo" "miliardi") ("unbiliardo" "biliardi")))
		  (snum (format nil "~a" num)) ; Converto il numero in stringa per poterlo mangiare meglio
		  (num-trio 0) ; Conta le terne di cifre
		  (str "")) ; Valore stringa restituito
			
		;; TODO: gestire numeri negativi
		;; TODO: controllare all'inizio che num sia un numero
		;; TODO: bloccare oltre i 999.999.999.999 (miliardi)!

		(if (equal num 0)
			
			(setf str "zero")
			
			(loop while (> (length snum) 0) do ; Mangio il numero come un grissino
							
				(let ((temp-cento "") (temp-decine "") (temp-add-trio "") ; stringhe temporanee
					  (cifra-cent 0) (cifre-decuni 0) (cifre-dec 0) (cifre-uni 0) ; valori temporanei per le cifre
					  trio) ; conterra' la terna di cifre corrente
					  
					(if (> (length snum) 3)
						(progn ; Se mi avanzano piu' di tre cifre
							(setf trio (subseq snum (- (length snum) 3))) ; prendo il trio di cifre piu' a destra (piu' significative)
							(setf snum (subseq snum 0 (- (length snum) 3)))) ; mangio il numero come un grissino
						(progn ; Se ho tre o meno cifre
							(setf trio snum) ; prendo tutto cio' che mi avanza (al piu' 3 cifre)
							(setf snum ""))) ; ultima mangiata del numero
				  
					;; ---------
					;; CENTINAIA
					(when (= (length trio) 3)
						(setf cifra-cent (parse-integer (subseq trio 0 1))) ; Rendo numerica la cifra delle centinaia
						(when (> cifra-cent 0) ; Se maggiore di zero, ottengo la parte "cento" "duecento" "trecento"...
							(setf temp-cento (concatenate 'string (when (> cifra-cent 1) (nth cifra-cent strumber-unita)) "cento")))
						(setf trio (subseq trio 1))) ; Mangio la cifra delle centinaia da trio (che diventa un duo)
					
					;; -------------
					;; DECINE-UNITA'
					(setf cifre-decuni (parse-integer trio))
					(when (> cifre-decuni 0)
						(if (< cifre-decuni 20)
							;; Se il numero e' minore di 20, lo prendo direttamente da strumber-unita
							(setf temp-decine (nth cifre-decuni strumber-unita))
							;; Se il numero e' maggiore o uguale a 20, lo prendo da strumber-decine concatenato a strumber-unita
							(progn
								(setf cifre-dec (parse-integer (subseq trio 0 1))) ; Rendo numerica la cifra delle decine
								(setf cifre-uni (parse-integer (subseq trio 1 2))) ; Rendo numerica la cifra delle unita
								(if (find cifre-uni '(1 8))
									;; Per "uno" e "otto" devo abbreviare: "vent-(i)-otto" "novant-(a)-uno"
									(setf temp-decine (concatenate 'string (subseq (nth cifre-dec strumber-decine) 0 (1- (length (nth cifre-dec strumber-decine)))) (nth cifre-uni strumber-unita)))
									;; Casi normali
									(setf temp-decine (concatenate 'string (nth cifre-dec strumber-decine) (nth cifre-uni strumber-unita)))))))
					
					;; --------------------------------------------------
					;; MIGLIAIA / MILIONI ecc... (dipendenti da num-trio)
					(when (and (> num-trio 0) (< num-trio 5))
						;; Prendo da strumber-oltre-mille le diciture che mi ineressano. Se necessario, svuotero' temp-decine.
						(unless (equal (concatenate 'string temp-cento temp-decine) "")
							(if (equal (concatenate 'string temp-cento temp-decine) "uno")
								(progn (setf temp-decine "") (setf temp-add-trio (first (nth num-trio strumber-oltre-mille))))
								(setf temp-add-trio (second (nth num-trio strumber-oltre-mille))))))
					
					;; "Ventunomila" diventa "Ventunmila"
					(when (and (not (equal temp-add-trio "")) (strumber-string-ends-with temp-decine "uno"))
						(setf temp-decine (subseq temp-decine 0 (1- (length temp-decine)))))
					
					;; Accodo le stringhe calcolate alla stringa finale
					(setf str (concatenate 'string temp-cento temp-decine temp-add-trio str))
					
					;; Tengo traccia di quante terne di cifre ho mangiato
					(incf num-trio))))
		
		;; Restituisco str
		(when (equal str "") (setf str nil))
			
		str))

(defun lex->670 (str)

	(let ((strumber-unita '("" "uno" "due" "tre" "quattro" "cinque" "sei" "sette" "otto" "nove" "dieci" "undici" "dodici" "tredici" "quattordici" "quindici" "sedici" "diciassette" "diciotto" "diciannove"))
		  (strumber-decine '("" "" "venti" "trenta" "quaranta" "cinquanta" "sessanta" "settanta" "ottanta" "novanta"))
		  (strumber-oltre-cento-molt '(("mille" 1000) ("mila" 1000) ("milione" 1000000) ("milioni" 1000000) ("miliardo" 1000000000) ("miliardi" 1000000000) ("biliardo" 1000000000000) ("biliardi" 1000000000000)))
		  (strumber-centinaia '("" "cento" "duecento" "trecento" "quattrocento" "cinquecento" "seicento" "settecento" "ottocento" "novecento"))
		  (num nil)
		  
		  ho-segato

		  (flag-oltre-cento)
		  (flag-centinaia)
		  (flag-decine)
		  (flag-unita)
		  (cur-trio)
		  (moltiplicatore)
		  orig-str)
		
		(setf str (string-downcase (string-trim " " str)))
		(setf orig-str str)
		
		;; TODO: gestire ordinali
		
		(if (equal str "zero")
			
			(setf num 0)

			(progn
				
				(setf num 0) ; inizializzo

				(loop while (> (length str) 0) do ; Mangio il numero come un grissino
					
					(setf ho-segato nil)
					(setf flag-centinaia nil)
					(setf flag-decine nil)
					(setf flag-unita nil)
					(setf moltiplicatore 1)
					(setf cur-trio 0)
					
					;; ---------
					;; CENTINAIA
					(block cur-block
						(loop for i from 1 to 9 do
							(when (strumber-string-starts-with str (nth i strumber-centinaia))
								  (setf cur-trio (* i 100))
								  (setf str (subseq str (length (nth i strumber-centinaia))))
								  (setf flag-centinaia t)
								  (return-from cur-block))))
					
					;; ------
					;; DECINE
					(block cur-block
						(loop for i from 9 downto 2 do
							(cond
								;; decina scritta interamente (es. venti)
								((strumber-string-starts-with str (nth i strumber-decine))	
									(setf cur-trio (+ cur-trio (* i 10)))
									(setf str (subseq str (length (nth i strumber-decine))))
									(setf flag-decine t)
									(return-from cur-block))
								
								;; decina scritta senza l'ultima vocale (es. vent)
								((strumber-string-starts-with str (subseq (nth i strumber-decine) 0 (1- (length (nth i strumber-decine)))))
									(setf cur-trio (+ cur-trio (* i 10)))
									(setf str (subseq str (1- (length (nth i strumber-decine)))))
									(setf flag-decine t)
									(return-from cur-block)))))
					
					
					;; ------
					;; UNITA'
					(block cur-block
						(loop for i from 19 downto 1 do ; al contrario altrimenti tredici diventa tre-!
							(when (strumber-string-starts-with str (nth i strumber-unita))
								(setf cur-trio (+ cur-trio i))
								(setf str (subseq str (length (nth i strumber-unita))))
								(setf flag-unita t)
								(return-from cur-block))))
					
					(when (and (not flag-unita) (strumber-string-starts-with str "un")) ; caso "un" (ma non "uno")
						(setf cur-trio (+ cur-trio 1))
						(setf str (subseq str 2))
						(setf flag-unita t))
						
					
					;; ------------------
					;; OLTRE LE CENTINAIA
					(block cur-block
						(dolist (socm strumber-oltre-cento-molt)
							(when (strumber-string-starts-with str (car socm))
								(setf str (subseq str (length (car socm))))
								(setf moltiplicatore (cadr socm))
								(setf flag-oltre-cento t)
								(return-from cur-block))))
					
					(when (and (equal moltiplicatore 1000) (equal cur-trio 0))
						(setf cur-trio 1))
					
					(if (> cur-trio 0)

						(setf num (+ num (* cur-trio moltiplicatore)))

						(progn ; Uscita forzata
							(setf str "")
							(setf num nil))))))
	(when (and num (not (equal orig-str (670->lex num))))
			(setf num nil))
					
	num))


(defun strnum (num)
    (if (numberp num)
        (670->lex num)
        (lex->670 num)))
        
(defun 672->lex (num)
	(let ((str-ord '("" "primo" "secondo" "terzo" "quarto" "quinto" "sesto" "settimo" "ottavo" "nono" "decimo")))
		(cond
			((<= num 0) nil) ; numero minore di zero
			
			((<= num 10) (nth num str-ord)) ; numero minore di 10
				
			(t (let ((nts (670->lex num)) rchar r2char)
					(when nts
						(setf rchar (subseq nts (1- (length nts)) (length nts)))
						(setf r2char (subseq nts (- (length nts) 2) (1- (length nts))))
						
						;; tolgo ultima lettera del numero ordinale + "esimo" se:
						;; 		-(consonante)i / -e / -o / -a
						(when (or (and (equal rchar "e") (not (equal r2char "r")))
								  (equal rchar "o")
								  (equal rchar "a")
								  (and (equal rchar "i") (not (strumber-vocale r2char))))
							(setf nts (subseq nts 0 (1- (length nts)))))
						
						(when (strumber-string-ends-with nts "mil") ;ventunmila -> ventunmil-l-esimo
							(setf nts (concatenate 'string nts "l")))
						
						(concatenate 'string nts "esimo")))))))

(defun lex->672 (str)
	(let ((str-ord '("" "primo" "secondo" "terzo" "quarto" "quinto" "sesto" "settimo" "ottavo" "nono" "decimo"))
		  (num nil)
		  orig-str)
	
		(setf str (string-downcase (string-trim " " str)))
		(setf orig-str str)
		
		(if (strumber-string-ends-with str "esimo")
			;; Stringa in "esimo"
			(progn
				(setf str (subseq str 0 (- (length str) 5)))
				(setf num (lex->670 str))
				(unless num (setf num (lex->670 (concatenate 'string str "e"))))
				(unless num (setf num (lex->670 (concatenate 'string str "o"))))
				(unless num (setf num (lex->670 (concatenate 'string str "a"))))
				(unless num (setf num (lex->670 (concatenate 'string str "i"))))
				(when (and (not num) (strumber-string-ends-with str "mill"))
					(setf str (subseq str 0 (1- (length str))))
					(setf num (lex->670 (concatenate 'string str "i")))))
			;; Stringa non in "esimo"
			(progn
				(block cur-block
					(loop for i from 1 to 10 do
						(when (equal str (nth i str-ord))
							(setf num i)
							(return-from cur-block))))))
			
		(when (and num (not (equal orig-str (672->lex num))))
			(setf num nil))
			
		num))


(defun get-clclass (str &optional (class nil))
	(remove-by-acc str
		(if class
			(remove-if-not #'(lambda (x) (find (getf x :cat) class)) (mapcar #'(lambda (cl) (gethash cl |CLCLASS|)) (gethash (deacc str) |CLCLASS-LEX|)))
			(mapcar #'(lambda (cl) (gethash cl |CLCLASS|)) (gethash (deacc str) |CLCLASS-LEX|)))))

(defun get-clclass-fast (cat)
	(let (ret)
		(dolist (idc (gethash cat |CLCLASS-CAT|) ret) ;*** considero solo gli elementi della classe richiesta
			(setf ret (append ret (list (gethash idc |CLCLASS|)))))))
	
(defun gen-clclass (cat id)
	(cond 
		((equal cat 670) ; generazione numeri cardinali
			(if (< id 0)
				(list :lex (concatenate 'string "meno " (670->lex (- id))) :cat 670 :id id :num 
					(cond 
						((= id 0) nil)
						((= id 1) 1)
						((> id 1) 2)) :gen 3)
						
				(list :lex (670->lex id) :cat 670 :id id :num 
					(cond 
						((= id 0) nil)
						((= id 1) 1)
						((> id 1) 2)) :gen 3)))
		
		((equal cat 672) ; generazione numeri ordinali
			(print "genero numero"))
		
		(t ; classi chiuse da dizionario
			(car (remove-if-not #'(lambda (x) (equal (getf x :id) id)) (get-clclass-fast cat))))))

(defun gen-clclass-from-gen-num-fon (cat gen num &optional fon)
	(car (remove-if-not #'(lambda (x) 
		(and (or (equal (getf x :gen) 3) 
			 	 (equal (getf x :gen) gen))
				 
			 (or (equal (getf x :num) 3) 
			 	(equal (getf x :num) num))
	
			;(or ;(not fon)
			 	 ;(not (getf x :fsc))
			 	 (find fon (if (listp (getf x :fsc)) (getf x :fsc) (list (getf x :fsc))))
				 
				 ;)
				 
				 ))
			
		(get-clclass-fast cat))))
						
(defun an-clclass(wrd)
"analisi classi chiuse singole, non agglutinate ad altri costituenti"

	(let (tmp ret)
	
		(setf ret
			; Per la classe 516. Solo le combinazioni nella terza persona singolare e plurale formano una sola parola
			; anche se non agglutinate al verbo
			(remove-if #'(lambda(x) 
					(or (and (equal  (getf x :cat) 516) ; glie-(lo la li le ne)
							  (not (search (list (getf x :id)) '(21 22 23 24 25))))
						 
						 ; inoltre per essere accettate non devo richiedere ENCL (solo enclitici)
						 (getf x :ENCL)))
				(get-clclass wrd)))
	
		(cond

			;; 1, 2, 3...
			((and (setf tmp (parse-integer wrd :junk-allowed t)) (equal wrd (format nil "~a" tmp)))
				(if (< tmp 0)
					(push (list :lex (concatenate 'string "meno " (670->lex (- tmp))) :cat 670 :id tmp :num (cond 
						((= tmp 0) nil)
						((= tmp 1) 1)
						((> tmp 1) 2)
						) :gen 3) ret)
					(push (list :lex (670->lex tmp) :cat 670 :id tmp :num (cond 
						((= tmp 0) nil)
						((= tmp 1) 1)
						((> tmp 1) 2)
						) :gen 3) ret)))
					
			;; uno, due, tre...
			((setf tmp (lex->670 wrd))
				(push (list :lex wrd :cat 670 :id tmp :num (cond 
					((= tmp 0) nil)
					((= tmp 1) 1)
					((> tmp 1) 2)
					) :gen 3) ret))
			
			;; primo, secondo...
			((setf tmp (lex->672 wrd))
				(push (list :lex wrd :cat 672 :id tmp :num 1) ret))
			
			;; primi, secondi...
			((and (equal (right-str wrd 1) "i") (setf tmp (lex->672 (concatenate 'string (subseq wrd 0 (1- (length wrd))) "o"))))
				(push (list :lex wrd :cat 672 :id tmp :num 2) ret)))
				
		ret))



#|
(load-verbi)
(verb-mod-rad &key rad des con ii)
(verb-eur-mod-rad &key rad des con)
(get-verbo verbo &optional (sem 0))
(gen-verbo verbo)
(list-verb-paradigm verbo)

:trans  1 transitivo / 2 intransitivo / 3 transitivo e intransitivo
:aux 	1 essere 	 / 2 avere 		  / 3 essere e avere


Nota.  i totalmente regolari sono riconsciuti dalla presenza del solo primo tema
Nota2  i verbi difettivi emergono automaticamente essendo mancanti di alcune forme.
Nota3. i verbi solo riflessivi hanno campo rifl ed emergono automaticamente avendo valenza solo sogg v

Nota4. mancano i verbi da mettere in irregolari:
	("addirsi" :VALENZA (SOGG V PREP-ARG))
	("calere" :VALENZA (NON-SOGG V PREP-ARG PREP-ARG)) 
	("molcere" :VALENZA (SOGG V ARG)) 
	("tangere" :VALENZA (SOGG V ARG)) 
|#

(defun load-verbi ()

	(loop for i from 0 to (1- (length *desinenze*)) do
		(let ((des (deacc (getf (nth i *desinenze*) :des)))) ; tolgo accenti e apostrofi
			(setf (gethash des |VERBI-DES|) 
				(if (gethash des |VERBI-DES|)
					(append (gethash des |VERBI-DES|) (list i))
					(list i)))))


	;;
	;; Indicizzo radici verbi regolari (key => radice | value => nth in *verbi*)
	;;

	; Metto tutti i *dat-lemmi* in |LEMMI|
	(setf |VERBI| (make-hash-table :test #'equal))
	(let ((i 0))
		(dolist (lm *dat-verbi*)
			(setf (gethash i |VERBI|) lm)
			(incf i)))
	
	; scorro tutti i verbi
	(loop for i from 0 to (1- (hash-table-count |VERBI|)) do 
		(dolist (radici (getf (gethash i |VERBI|) :tm))
			(dolist (str radici) ; cercando in (gethash str |VERBI-RADICI|) NON devo trovare i
				(setq str (deacc str))  ; tolgo accenti e apostrofi
				(unless (search (list i) (gethash str |VERBI-RADICI|))					
						(setf (gethash str |VERBI-RADICI|) 
							(if (gethash str |VERBI-RADICI|)
								(append (gethash str |VERBI-RADICI|) (list i)) ; salvo se stesso piu' le info nuove
								(list i))))))) ; salvo solo le info nuove


	;;
	;; Indicizzo forme verbi irregolari
	;;
	(setf |VERBI-IRR| (make-hash-table :test #'equal))
	(let ((i 0))
		(dolist (lm *dat-verbi-irr*)
			(setf (gethash i |VERBI-IRR|) lm)
			(incf i)))
			
	(loop for i from 0 to (1- (hash-table-count |VERBI-IRR|)) do 
		(let ((cur-forme (getf (gethash i |VERBI-IRR|) :forme)))
			(loop for j from 0 to (1- (length cur-forme)) do
			;(print (list (getf (nth j cur-forme) :forma) i j)) ; debug
				(let ((forma (deacc (getf (nth j cur-forme) :forma)))) ; tolgo accenti e apostrofi
					(setf (gethash forma |VERBI-IRR-FORME|)
						(if (gethash forma |VERBI-IRR-FORME|)
							(append (gethash forma |VERBI-IRR-FORME|) (list (list i j)))
							(list (list i j)))))))))
				
				
;**************************************************************************************************
;*** Restituisce il verbo costruito secondo le regole morfologiche dei verbi per
;    l'unione radice+desinenza
;*** Le regole sono state dedotte da una grammatica italiana e riviste da Matteo, quindi sono scientificamente corrette... ;)
;*** Restituisce nil se non e' consentito l'accostamento morfologico. Una forma che si e' scelto 
;    di non contemplare riguarda il caso della seconda coniugazione in cui la radice termina 
;    per 'e' e la desinenza corrispondente a 'e', che dovrebbero generare una 'e'' accentata
;    (es. 'crescee' -> 'cresce'')
(defun verb-mod-rad (&key rad des con ii)	
	(let ((radm rad)  ; radice modificata secondo le regole morfologiche
		  (last-rad   (right-str rad 1)) ; ultimo carattere della radice
		  (last2-rad  (right-str rad 2)) ; ultimi due caratteri della radice
		  (last3-rad  (right-str rad 3)) ; ultimi tre caratteri della radice
		  (first-des  (left-str des 1))) ; primo carattere della desinenza
			
		
		;*** Genero radice modificata secondo regole morfologiche
		(case con
			(1  ;*** prima coniugazione
			
				(if (or ; ci/gi/sci/ (lanciare, mangiare, strisciare) ma non sci-are!
						(and (and (not (equal rad "sci"))								 ; se la radice non corrisponde a "sci"
								  (or  (equal last2-rad "ci") (equal last2-rad "gi")))   ; e se la radice termina per "ci" o"gi"
							 (or (equal first-des "e") (equal first-des "i")))			 ; e la desinenza inizia per "e" o "i"
						
						; cominc(i)-are, stud(i)-are
						(and (= ii 0) (equal last-rad "i") (equal first-des "i"))	; se il verbo non vuole la doppia "i" e la desinenza inizia per "i"
						
						; pigli-iamo
						(and (equal last3-rad "gli") ; se la radice finisce per "gli"
							 (equal first-des "i"))	 ; la desinenza inizia con "i"
						
						; (x)-i-iamo/e
						(and (equal last-rad "i") 							; se la radice termina con "i"
							 (or (equal des "iamo") (equal des "iate"))))	; e la desinenza e' "iamo" o "iate"
						  
					;; allora tolgo l'ultima lettera dalla radice
					(setq radm (left-str rad (1- (length rad))))

					(when
						; c-h-i/g-h-i (recare, pregare)
						(and (or (equal last-rad "c") (equal last-rad "g"))		; se la radice finisce con "c" o "g"
							 (or (equal first-des "e") (equal first-des "i")))	; e se la desinenza inizia per "e" o "i"

						;; allora inserisco una "h" tra radice e desinenza
						(setq radm (concatenate 'string rad "h")))))
					
			(2  ;*** seconda coniugazione
			
				(when ; toglie "i" della radice dopo "gl" o "g" o "c"
					(and (equal first-des "i")															; se la desinenza finisce per "i"
						 (or (equal last3-rad "gli") (equal last2-rad "gi") (equal last2-rad "ci")))	; e se la radice finisce per "gli", "gi" o "ci"
					 (setq radm (left-str rad (1- (length rad)))))										; allora elimino la "i" finale della radice
			
			
				(when (and (equal des "e")	(equal last-rad "e")) ; radice finisce per 'e' e desinenza e' 'e'
					(return-from verb-mod-rad nil))) ; unico caso in cui si rifiuta l'accostamento radice+desinenza e si restituisce nil
					
			((3 4)	;*** terza coniugazione (o incoativo)
				
				(when (and (equal last2-rad "sc") (or (equal des "iente") (equal des "ienti"))) ; il caso errato di "usc[i]enti"
					; elimino primo carattere della desinenza
					(setq des (right-str des (1- (length des)))))
		
					
				(when ; consentire -> consen-z-iente
					(and (or (equal des "iente") (equal des "ienti"))								; se la desinenza e' "iente" o "ienti"
						 (equal last2-rad "nt"))													; e se la radice finisce per "nt" allora
					(setq radm (concatenate 'string (left-str rad (1- (length rad))) "z")))))		; tolgo la "t" finale dalla radice e la sostituisco con una "z"
				
		
		(concatenate 'string radm des))) ; restituisco la radice modificata
			

;*******************************************************************************************
;*** Si esegue un procedimento euristico per restituire possibili forme della
;    radice che non riportino le regole morfologiche per l'accordo fonetico radice+desinenza
;*** Per la natura della funzione stessa e' possibile che i valori di ritorno non siano attendibili
;    servono quindi unicamente per essere confrontati e validati in un momento successivo
;    dalle procedure di ricerca. L'utilizzo di questa euristica permette di restringere 
;    il tempo di analisi di un verbo, in quanto limita gli errori sempre e comunque al massimo
;    agli ultimi due caratteri della radice. L'algoritmo si basa sulla rielaborazione inversa 
;    delle regole utilizzate in verb-mod-rad.
;*** Nota: il programma non si occupa di evitare la generazione della forma non contemplata 
;    di 'ee' in verb-mod-rad.
(defun verb-eur-mod-rad (&key rad des con)
	(let ((radm (list rad)) ; assegno come primo elemento la radice non mutata (potrebbe infatti essere anch'essa corretta)
		  (last-rad   (right-str rad 1)) ; ultimo carattere della radice
		  (last2-rad  (right-str rad 2)) ; ultimi due caratteri della radice
		  (first-des  (left-str des 1))) ; primo carattere della desinenza
			
		(case con
			(1  ;*** prima coniugazione
			
				(when
					; cominc(i)-are, stud(i)-are, [avviare], (x)-i-iamo
					; se la desinenza inizia per "i" e' anche sottointeso il caso
					; della radice finisce per "gl" e la desinenza inizia per "i" (pigli-iamo)
					(or (equal first-des "i") 	
					
						 ; ci/gi/sci/ (lanciare, mangiare, strisciare) ma non sciare
						(and (and (not (equal rad "sc"))							; se la radice e' diversa da "sc"
								   (or (equal last-rad "c") (equal last-rad "g"))) 	; e finisce per "c" o "g" (quindi anche (equal left2-rad "sc"))
							  (or (equal first-des "e") (equal first-des "i")))) 	; e la desinenza inizia per "e" o "i"
						
					(push (concatenate 'string rad "i") radm)) ; provo ad aggiungere una 'i' alla radice
					
				(when
					; c-h-i/g-h-i (recare, pregare)
					(and (or (equal last2-rad "ch") (equal last2-rad "gh")) ; se la radice finisce per 'ch' o 'gh'
						 (or (equal first-des "e") (equal first-des "i"))) 	; e la desinenza inizia per 'e' o per 'i'
						 
					(push (left-str rad (1- (length rad))) radm))) 	; provo a sottrarre la 'h' dalla radice
						 
			(2  ;*** seconda coniugazione
			
				; toglie "i" della radice dopo 'gl' o 'g' o 'c'
				(when 
					(and (equal first-des "i")														; se la desinenza inizia per "i"
						 (or (equal last2-rad "gl") (equal last-rad "g") (equal last-rad "c")))		; e se la radice finisce per "gl" o "g" o "c"
					(push (concatenate 'string rad "i") radm))) 									; provo ad aggiungere una 'i' alla radice

			((3 4) ;*** terza coniugazione (e incoativo)
			
				(when
					(and (or (equal des "iente") (equal des "ienti")) 							; se la desinenza e' 'ienti' o 'iente'
						 (equal last2-rad "nz")) 												; e se la radice finisce per 'nz'
					(push (concatenate 'string (left-str rad (1- (length rad))) "t") radm)))) 	; provo a sostituire la 'z' con la "t"
		radm))


;**********************************************************
;*** Data una forma coniugata del verbo ritorna una lista
;    di verbi associati (normalizzata per i regolari e regolari)
(defun get-verbo (verbo &optional &key (sem 0) 
					; per validazione
					mdv tmp prs num gen)
					
	(let ((verbo-deacc (deacc verbo))
		  (ret nil)   ; ritorno
		  max-len-des ; massima lunghezza della desinenza
		  radx desx  ; parte destra del verbo (radice) e parte sinistra del verbo (desinenza)
		  scorri-verbo-found)
		  
		;; ricerca irregolari (id corrisponde a nth in |VERBI-IRR| per informazioni morfologiche)
		(setq ret (mapcar #'(lambda (x)
							(append (list :cat (getf (gethash (car x) |VERBI-IRR|) :cat) 		; -> categoria
										  :id (car x) 								 		    ; -> puntatore a nth |VERBI-IRR|
										  :lex (getf (gethash (car x) |VERBI-IRR|) :lex))		; -> lemma base
									(nth (cadr x) (getf (gethash (car x) |VERBI-IRR|) :forme))  ; -> forma del verbo in input
									(list :sem sem 												; -> indicatore prefisso valutativo
										  :tran (getf (gethash (car x) |VERBI-IRR|) :tran)
										  :aux (getf (gethash (car x) |VERBI-IRR|) :aux))))  
				
					(gethash verbo-deacc |VERBI-IRR-FORME|)))
		
		;; ricerca regolari o parzialmente regolari (elaborazione stringa) todo: ricerca secca per infinito
		(if (< (length verbo-deacc) 9) 					; Se la lunghezza del verbo e' minore di nove allora la lunghezza massima della desinenza 
			(setq max-len-des (1- (length verbo-deacc)))	; deve lasciare almeno un carattere alla radice altrimenti la desinenza puo'
			(setq max-len-des 8))					; raggiungere la sua massima lunghezza di otto caratteri
		
		;; mangio la stringa da sinistra a destra
		(loop for i from 1 to max-len-des do
			(setq radx (left-str verbo-deacc (- (length verbo-deacc) i))) ; parte sinistra del verbo, radice
			(setq desx (right-str verbo-deacc i)) 				  ; parte destra del verbo, desinenza
			
			;;
			;; verifico se desx e' una vera desineza. In caso affermativo scorro le desinenze (declinazioni) associate
			;; quindi se desx corrisponde a una desinenza cur-des contiene la lista di desinenze associate
			;;
			(dolist (cur-des (mapcar #'(lambda (x) (nth x *desinenze*)) (gethash desx |VERBI-DES|)))
			
				;;
				;; se desx e' una desinenza provo a generare delle forme alternative morfologicamente possibili della radice
				;; da radx vengono generate delle forme alternative derivate da regole morfologiche
				;;
				(dolist (eur-rad (verb-eur-mod-rad	:rad radx  ; radice corrente
													:des desx  ; desinenza corrente
													:con (getf cur-des :con))) ; coniugazione della desinenza che sto ciclando
					;;
					;; verifica validita' delle radici euristiche
					;; verbo-found = se il verbo in input soddisfa i criteri di ricerca viene trovato e restituito il verbo (infinito) associato
					;;
					
					(setf scorri-verbo-found nil)
					
					(dolist (verbx (mapcar #'(lambda (x) (append (list :id x) (gethash x |VERBI|)) ) (gethash eur-rad |VERBI-RADICI|)))
				
						(block ricerca-ntm ; blocco di ricerca del tema (da numero temi compatibili (ntm))
							
							; la coniugazione della desinenza deve coincidere con quella del verbo
							; ma non escludo le desienze con coniugazione 5 6 7 che verranno filtrate successivamente 
							(unless (or (equal (getf verbx :con) (getf cur-des :con))  
										(> (getf cur-des :con) 4))	
									(return-from ricerca-ntm nil))

							; filtro per dittongo mobile (dm), seleziono solo le restrizioni compatibili con il dm del verbo
							(let* ((r1 (getf (car (remove-if-not #'(lambda(x) (equal (getf x :dm) (getf verbx :dm))) (getf cur-des :restr))) :ntm))
								   (restrizione (if r1 r1 (getf (car (getf cur-des :restr)) :ntm)))) ; in estremo, prendo la prima restrizione (dm generico)
								
								(dolist (cur-ntm restrizione)
									
									(when (nth (1- cur-ntm) (getf verbx :tm)) ; verifico se il tema corrente e' popolato nel verbx (i temi partono da indice 0 in verbx :tm)
												 ;
												 ; se e la coniugazione della desinenza coincide con quella del verbo oppure coincide con il tema nei casi 5 6 7
										(if (and (equal (if (> cur-ntm 4) cur-ntm (getf verbx :con)) (getf cur-des :con))  
												 ;
												 ; e se eur-rad corrente e' disponibile nelle radici per il tema individuato
												 (search (list eur-rad) (nth (1- cur-ntm) (getf verbx :tm)) :test #'equalp)
												 ;
												 ; e se la radice euristica generata algoritmicamente deriva effettivamente dal verbo (radx +desx)
												 (equal verbo-deacc (deacc (setf (getf verbx :forma)
																				(verb-mod-rad :rad eur-rad
																							  :des (getf cur-des :des)
																							  :con (getf verbx :con)
																							  :ii (getf verbx :ii))))))
											;
											; allora posso essere sicuro di aver trovato il verbo!
											;(return-from ricerca-ntm 't) ; = non rimuovo (sara' ciclato in verbo-found)
											(push verbx scorri-verbo-found)
											;
											; altrimenti, dato che il tema richiesto e popolato (sono passato nel when) ma non sono stati soddisfatti
											; i criteri di compatibilita', scarto il verbx bloccano la ricerca di altre restrizioni
											(return-from ricerca-ntm nil))))))) ; = rimuovo (no verbo-found)

					(dolist (verbo-found scorri-verbo-found) ; scorro esclusivamente i verbi 
						;;
						;; salvo le informazioni da restituire per i verbi individuati (id corrisponde a nth in *verbi* per informazioni morfologiche)
						;;
						(push (append ;verbo-found cur-des
								(list :cat (getf verbo-found :cat) 
								      :id (getf verbo-found :id) 
									  :lex (getf verbo-found :lex)
									  :mdv (getf cur-des :mdv) 
									  :tmp (getf cur-des :tmp) 
									  :prs (getf cur-des :prs) 
									  :num (getf cur-des :num) 
									  :gen (getf cur-des :gen) 
									  :forma (getf verbo-found :forma) 
									  :tran (getf verbo-found :tran) 
									  :aux (getf verbo-found :aux) 
									  :sem sem)) ret)))))

		(setf ret (remove-by-acc verbo ret))
		
		;;
		;; ricerca prefisso valutativo (sem)
		;; il tratto semantico (sem) indica la presenza e la tipologia del prefisso valutativo individuato
		;; emergono una dimensione quantitativa: small/big, e una qualitativa: bad/good
		;;
		(unless ret
			(when (> (length verbo) 3)
				(let (max-len-pref prefx verbx)
					(if (< (length verbo) 6)
						(setq max-len-pref (1- (length verbo)))
						(setq max-len-pref 5))
				
					(loop for i from 3 to max-len-pref do
						(setq prefx  (left-str verbo i))
						(setq verbx (right-str verbo (- (length verbo) i)))
						
						(cond
							((or (equal prefx "intra") (equal prefx "ipo") (equal prefx "semi") (equal prefx "sotto"))
							 (return (setf ret (get-verbo verbx :sem 1)))) ; small, bad
							
							((or (equal prefx "iper") (equal prefx "sopra") (equal prefx "sur"))
							 (return (setf ret (get-verbo verbx :sem 2)))) ; big, bad
							
							((equal prefx "stra")
							 (return (setf ret (get-verbo verbx :sem 3)))) ; big, bad, good
							
							((equal prefx "super")
							 (return (setf ret (get-verbo verbx :sem 4))))))))) ; big, good
		
			(when (and ret mdv tmp prs num gen) ; se devo validare
				(setf ret (remove-if-not #'(lambda(x)  (and (equal (getf x :mdv) mdv) 
															(equal (getf x :tmp) tmp)
															(equal (getf x :prs) prs)
															(equal (getf x :num) num)
															(equal (getf x :gen) gen))) ret)))
			
			ret))

(defun gen-verbo (verbo)
	
	(unless verbo (return-from gen-verbo nil))
	
	(let (ret restrizione)
		(if (< (getf verbo :cat) 120) 
	
			;;
			;; regolari o parzialmente irregolari (cat 100)
			;;
			(progn (setf ret nil)			

				;; se non ho passato a gen-verbo id, mi aspetto di trovare (con, dm, ii, tm), diversamente
				(when (getf verbo :id) ;; accordo al verbo le informazioni ricavate da *verbi* tramite puntatore id
					(setf verbo (append verbo 
						(list :con (getf (gethash (getf verbo :id) |VERBI|) :con)    ; coniugazione
							  :dm  (getf (gethash (getf verbo :id) |VERBI|) :dm)     ; dittongo mobile
							  :ii  (getf (gethash (getf verbo :id) |VERBI|) :ii)     ; doppia i
							  :tm  (getf (gethash (getf verbo :id) |VERBI|) :tm))))) ; radici tematiche
				
				
				;;
				;; Coniugo i verbi regolari o parzialmente irregolari
				;;
				(dolist (desinenza ; scorro esclusivamente le desinenze compatibili per modo tempo persona genere e numero
						(remove-if-not #'(lambda (x) (and (equal (getf verbo :mdv) (getf x :mdv))
														  (equal (getf verbo :tmp) (getf x :tmp))
														  (equal (getf verbo :prs) (getf x :prs))
														  (equal (getf verbo :num) (getf x :num))
														  (equal (getf verbo :gen) (getf x :gen))
														  ;
														  ; la coniugazione delle desinenze deve coincidere con quella del verbo
														  ; ma non escludo dalla rimozione le desienze con coniugazione 5 6 7 che verranno filtrate successivamente 
														  (or (equal (getf verbo :con) (getf x :con))  
															  (> (getf x :con) 4)))) *desinenze*))
					
					
					(setf restrizione nil) ; Seleziono solo le restrizioni compatibili con il dittongo mobile del verbo
					(setf restrizione (getf (car (remove-if-not #'(lambda(x) (equal (getf x :dm) (getf verbo :dm))) (getf desinenza :restr))) :ntm))
					(unless restrizione (setf restrizione (getf (car (getf desinenza :restr)) :ntm))) ; Se non ho individuato restrizioni prendo comunque la prima
					
					(block ricerca-ntm
						(dolist (cur-ntm restrizione)
							(when (nth (1- cur-ntm) (getf verbo :tm)) ; controllo se il tema cercato e' presente
											
								; se e la coniugazione della desinenza coincide con quella del verbo oppure coincide con il tema nei casi 5 6 7
								(if (equal (if (> cur-ntm 4) cur-ntm (getf verbo :con)) (getf desinenza :con))  
										 
									(return-from ricerca-ntm (setf ret (append ret ; aggiungo a ret il nuovo verbo composto da radice + desinenza
										(mapcar #'(lambda (x) (verb-mod-rad :rad x ; radice
                                                                               :des (getf desinenza :des) ; desinenza
                                                                               :con (getf verbo :con) ; coniugazione vera del verbo
                                                                               :ii (getf verbo :ii))) ; richiesta della doppia i
											
											(nth (1- cur-ntm) (getf verbo :tm))))))
																		
									(return-from ricerca-ntm nil)))))) ; se il tema e' presente ma non soddisfa le condizioni allora non esamino altre restrizioni di questo verbo
				
				(setf ret (remove-if-not #'(lambda(x) (not (null x))) ret))) ; elimino gli elementi nulli dovuti a incompatibilita' individuate in verb-mod-rad
			
			
			;;---------------------------------------------------------
			;; Coniugo i verbi completamente irregolari (cat 120 o 130)
			;;
			(setf ret (mapcar #'(lambda (x) (getf x :forma))
				(remove-if-not #'(lambda (x) (and (equal (getf verbo :mdv) (getf x :mdv))
												  (equal (getf verbo :tmp) (getf x :tmp))
												  (equal (getf verbo :prs) (getf x :prs))
												  (equal (getf verbo :num) (getf x :num))
												  (equal (getf verbo :gen) (getf x :gen)))) ;(getf verbo :forme)))))))
							;
							; accesso alle informazioni morfologihe da nth id in |VERBI-IRR|
							(getf (gethash (getf verbo :id) |VERBI-IRR|) :forme)))))
							
			
			
			(let (final-ret)
                ; aggiungo forma tronca per l'infinito
				(when (and ret (equal (getf verbo :mdv) 6) (equal (getf verbo :tmp) 1))
					(dolist (x ret) 
						(cond
							((equal (right-str x 3) "rre")	(push (left-str x (- (length x) 2)) final-ret))
							((equal (right-str x 2) "re")	(push (left-str x (- (length x) 1)) final-ret)))))
                            
                ; aggiungo forma tronca per la terza persona plurale es. "mangian"            
                (when (and ret (equal (getf verbo :prs) 3) (equal (getf verbo :num) 2))
					(dolist (x ret) 
						(cond
							((equal (right-str x 3) "nno")	(push (left-str x (- (length x) 2)) final-ret))
							((equal (right-str x 2) "no")	(push (left-str x (- (length x) 1)) final-ret)))))
                
                
				(append ret final-ret))))
							

(defun coniuga (verbo)
	(list-verb-paradigm (car (get-verbo verbo))))

(defun mconiuga (verbo)
    (let ((exist (car (get-verbo verbo))))
        (if (null exist)
            (list-verb-paradigm (askv) nil t t)
            (list-verb-paradigm exist nil t t))))

(defun list-verb-paradigm (verbo &optional (clit nil) (out t) (rewrite nil))
"Ottengo tutti i modi, tempi, persone, numeri, generi gia' ordinati per poter successivamente mostrare l'intero paradigma del verbo."

	; Per comodita' prendo le declinazioni dalla lista di desinenze della prima coniugazione (55 forme standard)z
	;(dolist (decl (reverse (remove-if-not #'(lambda (x) (and (equal (getf x :con) 1) (or (not i-mdv) (equal (getf x :mdv) i-mdv)))) *desinenze*)))
	(dolist (decl (reverse (remove-if-not #'(lambda (x) (or (and (not clit) (equal (getf x :con) 1))
															(and clit (equal (getf x :con) 1) 
																 (or (equal (getf x :mdv) 5)
																	 (equal (getf x :mdv) 6)
																	 (equal (getf x :mdv) 7)
 																	 (and (equal (getf x :mdv) 4) (equal (getf x :tmp) 3)))))) *desinenze*)))
		
		(let (cur-ret)
			(if (getf verbo :id)
				(dolist (_ (gen-verbo (list :cat (getf verbo :cat) :id (getf verbo :id) :lex (getf verbo :lex)
										;;
										;; genera nella forma nella declinazione
										:mdv (getf decl :mdv) :tmp (getf decl :tmp) :prs (getf decl :prs) :num (getf decl :num) :gen (getf decl :gen)))) (block block-gen-verbo
						
						
							; (get-clclass pronx '(512 513 515 516) -> aumento la forma e il lemma per comporre il clitico e ne restringo i modi!
						
						(if (not clit)
							;(setf cur-ret (format out "(~a)[~a]tpos=~a|mdv=~a|tmp=~a|prs=~a|num=~a|gen=~a~%"
							
							(if rewrite
								(setf cur-ret (format out "(\"~a\" ((:lex \"~a\" :cat ~a :mdv ~a :tmp ~a :prs ~a :num ~a :gen ~a :gcase ~a :dgr ~a)))~%" 
									_ (getf verbo :lex) (rewrite-cat 100) (rewrite-mdv (getf decl :mdv)) (rewrite-tmp (getf decl :tmp)) (rewrite-prs (getf decl :prs)) (rewrite-num (getf decl :num)) (rewrite-gen (getf decl :gen)) (rewrite-case 0) (rewrite-dgr 0))) ; (getf verbo :cat)  !
									
								(setf cur-ret (format out "(\"~a\" ((:lex \"~a\" :cat ~a :mdv ~a :tmp ~a :prs ~a :num ~a :gen ~a :gcase ~a :dgr ~a)))~%" 
									_ (getf verbo :lex) 100 (getf decl :mdv) (getf decl :tmp) (getf decl :prs) (getf decl :num) (getf decl :gen) 0 0)) ; (getf verbo :cat)  !
								)
								
							;-----clit
							
							(let (vcf retclit clit-features clit-cat)
								(dolist (clit ; elemento di clclass
												(dolist (class-clit '(512 515 516) retclit) ; classi interessanti
													(dolist (pronx (gethash class-clit |CLCLASS-CAT|)) ; id elementi nelle classi interessanti
														(push (gethash pronx |CLCLASS|) retclit)))) ; accodo a retclit i record singoli delle classi
										
									(block block-clit
									
									;-- verifico se il verbo può supportare il clitico
									#|
									(print verbo)
									(when (and (equal (getf verbo :TRAN) 1) (not (search (getf clit :lex)
									
												(list 
												"mi" "ti" "gli" "le" "ci" "vi" "si"
												)
												
													)))
													
											(return-from block-clit))
									|#		
											
											
									
									(setf vcf (concatenate 'string
												(cond
													((and (equal (getf decl :mdv) 6)
													      (equal (right-str _ 1) "e"))
										 
														(return-from block-gen-verbo))
														
													((equal (right-str _ 1) "à")
														(deacc _))
													
													(t 	_))
					
												(getf clit :lex)))
									
									
									(when (an-verbo vcf :mdv (getf decl :mdv) 
														:tmp (getf decl :tmp) 
														:prs (getf decl :prs) 
														:num (getf decl :num) 
														:gen (getf decl :gen)
														:clit-imposto clit)
									
										#|
										(if (equal (getf clit :cat) 516)
											(let ((gclcl1 (gen-clclass (getf clit :cat1) (getf clit :id1)))
												  (gclcl2 (gen-clclass (getf clit :cat2) (getf clit :id2))))
												
												(setf clit-features (format nil "+~a|~a|~a|~a|~a+~a|~a|~a|~a|~a" 
													0 0 (getf gclcl1 :prs) (getf gclcl1 :num) (getf gclcl1 :gen)
													0 0 (getf gclcl2 :prs) (getf gclcl2 :num) (getf gclcl2 :gen)))
													
												(setf clit-cat (format nil "+~a+~a" (getf gclcl1 :cat) (getf gclcl2 :cat))))
											
											(progn
												; singolo
												(setf clit-features (format nil "+~a|~a|~a|~a|~a" 
													  0 0 (getf clit :prs) (getf clit :num) (getf clit :gen)))
												
												(setf clit-cat (format nil "+~a" (getf clit :cat)))))
										(setf cur-ret (format out "~a|~a|100~a|~a|~a|~a|~a|~a~a~%" 
											vcf (concatenate 'string (getf verbo :lex) "+" (if (getf clit :LEX-COMPOSITE) (getf clit :LEX-COMPOSITE) (getf clit :lex)))
											clit-cat (getf decl :mdv) (getf decl :tmp) (getf decl :prs) (getf decl :num) (getf decl :gen)
											clit-features))
										|#
										
										
										
										;-----------------------------------------------
										; 20 oct 2011 - verifico compatibilita' fonetica
										;(or (not (2-consonanti (getf cur-pron :lex)))
										;    (and (2-consonanti-uguali (getf cur-pron :lex)) (voc-acc (right-str (getf x :forma) 1)))
										;    (and (2-consonanti (getf cur-pron :lex)) (not (2-consonanti-uguali (getf cur-pron :lex)))))
										
										; promemoria
										; vcf  -> forma del verbo declinata
										; clit -> clitico singolo o composto
										;
										; forma-clit -> lex di clit anche se e' composite (516)
																				
										(let ((forma-clit (getf clit :lex))) 
											
											;(format t "###########################~%")
											;(format t "2-consonanti-uguali = ~a~%" (2-consonanti-uguali forma-clit))
											;(format t "voc-acc vcf = ~a~%" (voc-acc (right-str vcf 1)))
											;(format t "voc-acc _ = ~a~%" (voc-acc (right-str _ 1)))
											
											(when (or (and (2-consonanti-uguali forma-clit) (voc-acc (right-str _ 1)))
												  (not (2-consonanti-uguali forma-clit)))
												
												(if (equal (getf clit :cat) 516)
													; doppio, composite
													(let* ((gclcl1 (gen-clclass (getf clit :cat1) (getf clit :id1)))
													      (gclcl2 (gen-clclass (getf clit :cat2) (getf clit :id2)))
													      (clit2case (if (equal (rewrite-clit (getf gclcl2 :lex)) "ne") 'PART+LOC 'OBJ))
													      
													      )
														
														(if rewrite
															(setf clit-features (format nil "(:lex \"~a\" :cat ~a :mdv ~a :tmp ~a :prs ~a :num ~a :gen ~a :gcase ~a :dgr ~a) (:lex \"~a\" :cat ~a :mdv ~a :tmp ~a :prs ~a :num ~a :gen ~a :gcase ~a :dgr ~a)" 
																(rewrite-clit (getf gclcl1 :lex)) (rewrite-cat-encl (getf gclcl1 :cat)) (rewrite-mdv 0) (rewrite-tmp 0) (rewrite-prs (getf gclcl1 :prs)) (rewrite-num (getf gclcl1 :num)) (rewrite-gen (getf gclcl1 :gen)) (rewrite-case 'iobj) (rewrite-dgr 0)
																(rewrite-clit (getf gclcl2 :lex)) (rewrite-cat-encl (getf gclcl2 :cat)) (rewrite-mdv 0) (rewrite-tmp 0) (rewrite-prs (getf gclcl2 :prs)) (rewrite-num (getf gclcl2 :num)) (rewrite-gen (getf gclcl2 :gen)) (rewrite-case clit2case) (rewrite-dgr 0)))
															
															(setf clit-features (format nil "(:lex \"~a\" :cat ~a :mdv ~a :tmp ~a :prs ~a :num ~a :gen ~a :gcase ~a :dgr ~a) (:lex \"~a\" :cat ~a :mdv ~a :tmp ~a :prs ~a :num ~a :gen ~a :gcase ~a :dgr ~a)" 
																(rewrite-clit (getf gclcl1 :lex)) (getf gclcl1 :cat) 0 0 (getf gclcl1 :prs) (getf gclcl1 :num) (getf gclcl1 :gen) (rewrite-case 'iobj) 0
																(rewrite-clit (getf gclcl2 :lex)) (getf gclcl2 :cat) 0 0 (getf gclcl2 :prs) (getf gclcl2 :num) (getf gclcl2 :gen) (rewrite-case 'obj) 0))
															
															))
															
													; singolo
													
													(progn 
													
														;(unless (find (getf clit :lex) '("cci" "ci" "gli" "la" "le" "li" "lla" "lle" "lli" "llo" "lo" "mi" "mmi" "ne" "nne" "si" "ti" "tti" "vi") :test #'equal)
														;	(break))
															
														(if rewrite
															(setf clit-features (format nil "(:lex \"~a\" :cat ~a :mdv ~a :tmp ~a :prs ~a :num ~a :gen ~a :gcase ~a :dgr ~a)" 
																(rewrite-clit (getf clit :lex)) (rewrite-cat-encl (getf clit :cat)) (rewrite-mdv 0) (rewrite-tmp 0) (rewrite-prs (getf clit :prs)) (rewrite-num (getf clit :num)) (rewrite-gen (getf clit :gen)) (rewrite-case (get-gcase-from-lex (rewrite-clit (getf clit :lex)))) (rewrite-dgr 0)))
															
															(setf clit-features (format nil "(:lex \"~a\" :cat ~a :mdv ~a :tmp ~a :prs ~a :num ~a :gen ~a :gcase ~a :dgr ~a)" 
																(rewrite-clit (getf clit :lex)) (getf clit :cat) 0 0 (getf clit :prs) (getf clit :num) (getf clit :gen) 0 0))
															
															)
														
														))))
									
										(setf cur-ret (format out "(\"~a\" ((:lex \"~a\" :cat ~a :mdv ~a :tmp ~a :prs ~a :num ~a :gen ~a :gcase ~a :dgr ~a) ~a))~%" 
											vcf 
											
											;-- feat
											(getf verbo :lex) (rewrite-cat 100) (rewrite-mdv (getf decl :mdv)) (rewrite-tmp (getf decl :tmp)) (rewrite-prs (getf decl :prs)) (rewrite-num (getf decl :num)) (rewrite-gen (getf decl :gen)) (rewrite-case 0) (rewrite-dgr 0)
											
											;-- clit
											clit-features))
											
											
											)))))))				
										
				(let ((vforms (gen-verbo (list :cat (getf verbo :cat) :id (getf verbo :id) :lex (getf verbo :lex)
							;; nel caso avessi comunque le informazioni morfologiche, le accodo (3giugno2009)
							;;
							:con (getf verbo :con) :ii (getf verbo :ii) :dm (getf verbo :dm) :tm (getf verbo :tm)
							;;
							;; genera nella forma nella declinazione
							:mdv (getf decl :mdv) :tmp (getf decl :tmp) :prs (getf decl :prs) :num (getf decl :num) :gen (getf decl :gen)))))
                            
                
                    (dolist (_ vforms)
                        (setf cur-ret (format out "(\"~a\" ((:lex \"~a\" :cat ~a :mdv ~a :tmp ~a :prs ~a :num ~a :gen ~a :gcase ~a :dgr ~a)))~%" 
                            _ (getf verbo :lex) 100 (getf decl :mdv) (getf decl :tmp) (getf decl :prs) (getf decl :num) (getf decl :gen) 0 0)))))))) ; (getf verbo :cat)  !
                                    
                        

;*************************************
;*** ridefinisco il lemma clit
(defun rewrite-clit (forma-clit)
	(cond 
		((2-consonanti-uguali forma-clit)
		   (subseq forma-clit 1))
		   
		((equal forma-clit "glie")
			"gli")
			
		(t forma-clit)))

(defun rewrite-cat-encl (cat)
	"PRON-PERS-ENCLIT")


(defun get-gcase-from-lex(lex)
	(cond 
	   ((equal lex "ci")
		'OBJ+IOBJ)
	   ((equal lex "gli")
  	       'IOBJ)
	   ((equal lex "la")
  	       'OBJ)
	   ((equal lex "le")
  	       'OBJ+IOBJ)
	   ((equal lex "li")
  	       'OBJ)
	   ((equal lex "lo")
  	       'OBJ)	  
	   ((equal lex "mi")
  	       'OBJ+IOBJ)
	   ((equal lex "ne")
  	       'PART+LOC)
	   ((equal lex "si")
  	       'OBJ+IOBJ)
	   ((equal lex "ti")
  	       'OBJ+IOBJ)
	   ((equal lex "vi")
  	       'OBJ+IOBJ)
	   (t (break))))


;**************************************************
;*** analisi costituente verbale VERBO+CLT
(defun an-verbo(wrd &optional 
			; per validazione
			&key mdv tmp prs num gen
			clit-imposto)

	(let ((ret nil)
		  verbo
		  pronomi
		  verbx
		  verbx-orig ; a verbx può essere modificato l'accento 
		  pronx
		  max-len-pron
		  only-verbi
		  single-analisi
		  verbi-new)
		
		;; Ricerco se la parola così com'è è un verbo
		(unless clit-imposto ; a meno che non sia imposto un clitico!
			(when (setf verbo (get-verbo wrd))
				(push (list :v verbo :p nil) ret)))
		
		;; Ricerco se la parola è una forma tronca dell'infinito del verbo
		(when (equal (right-str wrd 1) "r") ; se la radice finisce in /r/
			; desinenza fittizia - mangiarlo => mangiar-lo => mangiar- => mangiar-e => mangiare
			(unless (and (not (equal (right-str wrd 2) "rr")) ; controllo che non finisca in doppia /r/ -> astra/r/r/si
						 (setf verbo (get-verbo (concatenate 'string wrd "e")))) ; verbo in -re
					(setf verbo (get-verbo (concatenate 'string wrd "re") ; verbo in -rre
										:mdv mdv :tmp tmp :prs prs :num num :gen gen)))     
			
			(when verbo
				(push (list :v verbo :p nil) ret)))
			
		
		;; Ricerco se la parola è un verbo con annesso uno o più un pronomi clitici
		(if (< (length wrd) 7) ; 6 = Massima lunghezza di un pronome enclitico
			(setq max-len-pron (1- (length wrd))) ; Se la parola è corta, faccio sì che la desinenza sia di almeno un carattere
			(setq max-len-pron 6))
			
		(loop for i from 1 to max-len-pron do
			(setq verbx (left-str wrd (- (length wrd) i))) ; parte sinistra, potenziale radice
			(setq verbx-orig verbx)
			(setq pronx (right-str wrd i)) 				   ; parte destra, potenziale pronome enclitico
			
			(when (and (not (equal (right-str pronx 1) "'")) 	; Non considero pronomi come v' o t'
					   (not (equal (right-str verbx 2) "re"))   ; Prevedo che non esistano forme in -regli o -rrgli (se alla radice aggiungessi poi la 'e' fittizia, verrebbero presi per giusti)
					   (or (vocale (right-str verbx 1)) ; o il verbo termina con una vocale
						   (not (2-consonanti-uguali pronx)))) ; o se termina per consonante, non lo posso accostare con pronomi con doppia consonante

				; Se il pronome inizia per doppia consonante, mi assicuro di avere l'accento sull'ultima lettera del verbo
				(when (and (vocale (right-str verbx 1)) (2-consonanti pronx))
					(setf verbx (concatenate 'string (left-str verbx (1- (length verbx))) (accento-grave (right-str verbx 1)))))
				
				(when (setf pronomi (get-clclass pronx '(512 515 516))) ; pronomi usati anche agglutinati al verbo
					
					; TODO verbx originale senza modifica di accento
				
					(if (equal (right-str verbx 1) "r") ; se la radice finisce in /r/
						; desinenza fittizia - mangiarlo => mangiar-lo => mangiar- => mangiar-e => mangiare
						(unless (and (not (equal (right-str verbx 2) "rr")) ; controllo che non finisca in doppia /r/ -> astra/r/r/si
									 (setf verbo (get-verbo (concatenate 'string verbx "e") ; verbo in -re
													:mdv mdv :tmp tmp :prs prs :num num :gen gen))) 
								(setf verbo (get-verbo (concatenate 'string verbx "re") ; verbo in -rre
												:mdv mdv :tmp tmp :prs prs :num num :gen gen)))     
						;
						; analisi senza alcuna desinenza fittizia
						(setf verbo (append (get-verbo verbx :mdv mdv :tmp tmp :prs prs :num num :gen gen)
											(when (not (equal verbx verbx-orig)) (get-verbo verbx-orig :mdv mdv :tmp tmp :prs prs :num num :gen gen)))))
				
					
				
					(setf only-verbi nil)
					(setf single-analisi nil)
				
					;;
					;; Ciclo i pronomi individuati controllando che ci siano solo le occorrenze 
					;; che possono davvero essere accostate a dei pronomi enclitici.
					;; Nota. eliminando le combinazioni sintattiche non permesse 
					;;       gia' a questo livello si evita di portare il controllo nella procedura
					;;       ad alto livello per l'analisi sintattica (CLT).
					;;
					(dolist (cur-pron pronomi)
						(setf verbi-new (remove-if-not #'(lambda (x) 
							
							(and ; sega di controllo del 24feb10
							(or (not clit-imposto)	
							;	(print (list clit-imposto '> cur-pron)) ; --> se passa di qui devono essere uguali!
								(equal clit-imposto cur-pron))
							
							;(print (list clit-imposto '> cur-pron)) ; --> se passa di qui devono essere uguali!
							
							;(print (list clit-imposto '> x)) ; --> se passa di qui devono essere uguali!

							(or  (and (equal (getf x :mdv) 5) ; = GERUNDIO
							
										(or (not (2-consonanti (getf cur-pron :lex)))
														  (and (2-consonanti-uguali (getf cur-pron :lex)) (voc-acc (right-str (getf x :forma) 1)))
														  (and (2-consonanti (getf cur-pron :lex)) (not (2-consonanti-uguali (getf cur-pron :lex))))))
							
							
							
								 (equal (getf x :mdv) 6) ; = INFINITO

								 ;-----------------------------------------------
								 (and (equal (getf x :mdv) 7) ; = IMPERATIVO
								 ;-----------------------------------------------
								 
									  (and (not (equal (getf x :prs) 3))
								 
										  (not (equal (getf x :LEX) "si")) ; peso del 24feb10
								 
										#|
											(or (not (2-consonanti (getf cur-pron :lex)))
											  (voc-acc (right-str (getf x :forma) 1)))
										|#
										
										
										  (or (not (2-consonanti (getf cur-pron :lex)))
											  (and (2-consonanti-uguali (getf cur-pron :lex)) (voc-acc (right-str (getf x :forma) 1)))
											  (and (2-consonanti (getf cur-pron :lex)) (not (2-consonanti-uguali (getf cur-pron :lex)))))
										
                                            ;(print x)
                                            ;(print cur-pron)
                                            ;(and (2-consonanti (getf cur-pron :lex)) (not (voc-acc (right-str (getf x :forma) 1)))))
										
										
										
										  
                                            
										  ;(not (and (vocale (right-str (getf x :FORMA) 1)) (vocale (subseq (right-str (getf x :FORMA) 2) 0 1)))) ; sega del 22feb10 !
										  (not (and (equal (right-str (getf x :FORMA) 1) "i") (equal (subseq (right-str (getf x :FORMA) 2) 0 1) "a"))) ; sega sostitutiva del 24feb10 ! * "ai"
											
										  (or (and (not (equal (getf cur-pron :cat) 516)) ; tutti i pronomi clitici eccetti quelli composti
										  
												   (or (and (equal (getf cur-pron :prs) 1) (equal (getf cur-pron :num) 1)
															(equal (getf x :prs) 2))
														   
													   (and (equal (getf cur-pron :prs) 2) (equal (getf cur-pron :num) 1)
															(equal (getf x :prs) 2) (equal (getf x :num) 1))
												  
													   (and (equal (getf cur-pron :prs) 2) (equal (getf cur-pron :num) 2)
															(equal (getf x :prs) 2) (equal (getf x :num) 2))
														   
														(and (equal (getf cur-pron :prs) 0) (equal (getf cur-pron :num) 3) (equal (getf cur-pron :gen) 3))	    ; sega 24geb10
														   
													   (and (equal (getf cur-pron :prs) 1) (equal (getf cur-pron :num) 2))
													   (and (equal (getf cur-pron :prs) 3) (equal (getf cur-pron :num) 1))
													   (and (equal (getf cur-pron :prs) 3) (equal (getf cur-pron :num) 2))
                                                       (and (equal (getf cur-pron :prs) 3) (equal (getf cur-pron :num) 3)) ; porgigli (13 sept 2016)
                                                       ))
											  
											  (and (equal (getf cur-pron :cat) 516) ; pronomi clitici composti

												   (or (and (equal (getf cur-pron :id1) 1)
															(equal (getf x :prs) 2))
													
													   (and (equal (getf cur-pron :id1) 2)
															(equal (getf x :prs) 2) (equal (getf x :num) 1))
															
													   (and (equal (getf cur-pron :id1) 4)
															(equal (getf x :prs) 2) (equal (getf x :num) 2))
															
													   (equal (getf cur-pron :id1) 5)
													   (equal (getf cur-pron :id1) 3))))))
								 
								 
								  ;-----------------------------------------------
								  (and (equal (getf x :mdv) 4) (equal (getf x :tmp) 3) ; = PARTICIPIO PASSATO
								  
										(or (not (2-consonanti (getf cur-pron :lex)))
														  (and (2-consonanti-uguali (getf cur-pron :lex)) (voc-acc (right-str (getf x :forma) 1)))
														  (and (2-consonanti (getf cur-pron :lex)) (not (2-consonanti-uguali (getf cur-pron :lex)))))
								  
									   (or 
										   ;(equal (getf cur-pron :cat) 513)
										   (equal (getf cur-pron :cat) 515)
											
										   (and (or (equal (getf cur-pron :cat) 512)
													(equal (getf cur-pron :cat) 516))
												(equal (getf x :gen) 1)
												(equal (getf x :num) 1))

										   (and (equal (getf cur-pron :cat) 512) (equal (getf cur-pron :lex) "si"))

										   (and (equal (getf cur-pron :cat) 512)
												(or (equal (getf cur-pron :gen) 0)
													(equal (getf cur-pron :gen) (getf x :gen)))
												(or (equal (getf cur-pron :num) 0)
													(equal (getf cur-pron :num) (getf x :num))))
											
										   (and (equal (getf cur-pron :cat) 516)
												(or (equal (getf (gen-clclass (getf cur-pron :cat2) (getf cur-pron :id2)) :gen) 0)
													(equal (getf (gen-clclass (getf cur-pron :cat2) (getf cur-pron :id2)) :gen) (getf x :gen)))
												(or (equal (getf (gen-clclass (getf cur-pron :cat2) (getf cur-pron :id2)) :num) 0)
													(equal (getf (gen-clclass (getf cur-pron :cat2) (getf cur-pron :id2)) :num) (getf x :num))))))
								  ;-----------------------------------------------
								  
								  ))) verbo))
				
						(push verbi-new only-verbi) ; accodo a solo verbi i verbi con esito positivo dalle restrizioni
						
						(when verbi-new ; se ho almeno un verbo accodo pronome individuato
							(push (list :v verbi-new :p (list cur-pron)) single-analisi)))
				
					(if (block ciclo-corrispondenze
							; scorro i verbi individuati
							(loop for i from 0 to (1- (length only-verbi)) do
								; scorro i verbi individuati succesivi al precedente (i)
								(loop for j from (1+ i) to (1- (length only-verbi)) do
									; se torvo anche solo una differenza ritorno nil
									(when (not (equal (nth i only-verbi) (nth j only-verbi))) (return-from ciclo-corrispondenze nil))))
							(return-from ciclo-corrispondenze t)) ; diversamente esito positivo (tutti i verbi coincidono)
						
						; SEGA - verificare qui l'uso di verbi-new, il cui utilizzo sembrerebbe essere esclusivo del precedente ciclo
						(when verbi-new (push (list :v verbi-new :p pronomi) ret)) ; se esito positivo accodo tutti i pronomi a tutti i verbi (verbi-new uguale a verbo)
						
						(when single-analisi (dolist (sa single-analisi) (push sa ret))))))) ; diversamente restituisco i singoli incroci tra verbi e pronomi
			ret))
		
;********************************************
;*** Popolazione dei dizionari morfologici
(defun load-ddi()
	
	;*** Caricamento delle basi dati
	(load "dat-clclass")   	; *clclass*
	(load "dat-verbi-des") 	; *desinenze*
	(load "dat-verbi-irr") 	; *dat-verbi-irr*
	(load "dat-verbi")  	; *dat-verbi*
	(load "dat-lemmi")		; *dat-lemmi*

	; (hash-table-size |NOME-HASHTABE|) -> da definire il size!
	(setf |VERBI-DES|    	(make-hash-table :test #'equal))
	(setf |VERBI-RADICI| 	(make-hash-table :test #'equal))
	(setf |VERBI-IRR-FORME| (make-hash-table :test #'equal))
	(setf |LEMMI-RADICI|  	(make-hash-table :test #'equal))
	(setf |LEMMI-LOC|  		(make-hash-table :test #'equal))
	(setf |CLCLASS-LEX| 	(make-hash-table :test #'equal))
	(setf |CLCLASS-CAT| 	(make-hash-table :test #'equal))
	
	;*** Caricamento memoria verbi
	(format t ";; Caricamento memoria verbi...")
	(load-verbi)
	(format t " fatto~%")

	;*** Caricamento memoria lemmi
	(format t ";; Caricamento memoria lemmi...~%")
	(load-lemmi)
	(format t "~%")
	;(format t " fatto~%")
	
	;*** Caricamento memoria classi chiuse
	(format t ";; Caricamento memoria classi chiuse...")
	(load-clclass)
	(format t " fatto~%")
	
	;*** Libero memoria (non svuoto *clclass* perché viene ancora utilizzato)
	;(setf *dat-lemmi* nil)
	(setf *dat-verbi* nil)
	(setf *dat-verbi-irr* nil)
	(force-garbage 4)
	
	(setq *max-len-loc* 
		(if (> *max-lemmi-loc* *max-clclass-loc*) 
			*max-lemmi-loc*      ; la locuzione più lunga è dei lemmi
			*max-clclass-loc*))) ; la locuzione più lunga è delle classi chiuse


;;
;; FUNZIONI DI ESPORTAZIONE DIZIONARI
;;
;(with-open-file (out "dat-verbi2.lisp" :direction :output) (print-verbi-reg out))
	
(defun print-verbo-reg (v fout)
	(let (spazio1 spazio2)
		(format fout "~4t(:LEX \"~a\" :SILL \"~a\" :CAT ~a :TRAN ~a :AUX ~a" (getf v :lex) (getf v :sill) (getf v :cat) (getf v :tran) (getf v :aux))
		(when (getf v :no-prs) (format fout " :NO-PRS ~a" (getf v :no-prs)))
		(when (getf v :rifl) (format fout " :RIFL ~a" (getf v :rifl))) ; riflessivi
		(format fout " :ETIMO (~{\"~a\"~}) :FONTE ~a :MUSO ~a~%" (getf v :etimo) (getf v :fonte) (getf v :muso))
		(when (getf v :valenza) (format fout "~8t:VALENZA ~a~%" (getf v :valenza)))
		(format fout "~8t:CON ~a :DM ~a :II ~a :TM (" (getf v :con) (getf v :dm) (getf v :ii))
		
		(setf spazio1 nil)
		(dolist (tm (getf v :tm))
			(when spazio1 (format fout " "))
			(format fout "(")
			(setf spazio2 nil)
			(dolist (str tm)
				(when spazio2 (format fout " "))
				(format fout "\"~a\"" str)
				(unless spazio2 (setf spazio2 t)))
			(format fout ")")
			(unless spazio1 (setf spazio1 t)))
			
		(format fout "))~%~%")))

(defun print-verbi-reg (fout)
	(format fout "(setf *dat-verbi* '(~%~%")
	(dolist (v *dat-verbi*)
		(print-verbo-reg v fout))
	(format fout "))"))

;(with-open-file (out "dat-verbi-irr2.lisp" :direction :output) (print-verbi-irr out))
(defun print-verbi-irr (fout)
	(let (spazio1)
		(format fout "(setf *dat-verbi-irr* '(~%~%")

		(dolist (v *dat-verbi-irr*)
			(format fout "~4t(:LEX \"~a\" :SILL \"~a\" :CAT ~a :TRAN ~a :AUX ~a" (getf v :lex) (getf v :sill) (getf v :cat) (getf v :tran) (getf v :aux))
			(when (getf v :no-prs) (format fout " :NO-PRS ~a" (getf v :no-prs))) ; impersonali
			(when (getf v :rifl) (format fout " :RIFL ~a" (getf v :rifl))) ; riflessivi
			(format fout " :ETIMO (~{\"~a\"~}) :FONTE ~a :MUSO ~a~%" (getf v :etimo) (getf v :fonte) (getf v :muso))
			(when (getf v :valenza) (format fout "~8t:VALENZA ~a~%" (getf v :valenza)))
			(format fout "~8t:con ~a :forme (" (getf v :con))
			(setf spazio1 nil)
			(dolist (f (getf v :forme))
				(when spazio1 (format fout "~%~23t"))
				(format fout "(:MDV ~a :TMP ~a :PRS ~a :NUM ~a :GEN ~a :FORMA \"~a\")" (getf f :mdv) (getf f :tmp) (getf f :prs) (getf f :num) (getf f :gen) (getf f :forma))
				(unless spazio1 (setf spazio1 t)))
			(format fout "))~%~%"))
			
		(format fout "))")))


(defun print-lemma (fout lemma)
"Scrive su fout il lemma in una forma leggibile"
; TODO: a tutti quelli con muso inserire fonte demauro

	(let (primo primo2)

		;--------------------- non scrivo se è un :FONTE (DEMAURO DEVOLI) :MUSO (TS) e non ha nemmeno una :cat 400
		#|
		(let ((aggettivo nil))
			(dolist (cmorph (getf lemma :morph) aggettivo)
				(when (equal (getf cmorph :cat) 400)
					(setf aggettivo t)))
	
			(unless aggettivo
				(when (and (equal (getf lemma :fonte) '(DEMAURO DEVOLI))
						   (equal (getf lemma :muso) '(TS)))
						   
					(return-from print-lemma))))
		|#
		;--------------------------------------

    

		(if (getf lemma :v-deriv)
			(if (getf (getf lemma :v-deriv) :lex)
				(format fout "(:LEX \"~a\" :SILL \"~a\" :ETIMO (~{\"~a\"~}) :V-DERIV (:LEX \"~a\" :TRAN ~a) :FONTE ~a :MUSO ~a~%" (getf lemma :lex) (getf lemma :sill) (getf lemma :etimo) (getf (getf lemma :v-deriv) :lex) (getf (getf lemma :v-deriv) :tran) (getf lemma :fonte) (getf lemma :muso))
				(format fout "(:LEX \"~a\" :SILL \"~a\" :ETIMO (~{\"~a\"~}) :V-DERIV (:LEX NIL :TRAN ~a) :FONTE ~a :MUSO ~a~%" (getf lemma :lex) (getf lemma :sill) (getf lemma :etimo) (getf (getf lemma :v-deriv) :tran) (getf lemma :fonte) (getf lemma :muso)))
			(format fout "(:LEX \"~a\" :SILL \"~a\" :ETIMO (~{\"~a\"~}) :V-DERIV () :FONTE ~a :MUSO ~a~%" (getf lemma :lex) (getf lemma :sill) (getf lemma :etimo) (getf lemma :fonte) (getf lemma :muso)))
			
		(format fout "~4t:MORPH (")
		(setq primo t)
		(dolist (cmorph (getf lemma :morph))
			(if primo (setq primo nil) (format fout "~%~12t"))
			(format fout "(:CAT ~a :FORME (" (getf cmorph :cat))
			(setq primo2 t)
			(dolist (cforma (getf cmorph :forme))
				(if primo2 (setq primo2 nil) (format fout "~%~30t"))
				(if (or (equal (getf cmorph :cat) 300) (equal (getf cmorph :cat) 390))
					(format fout "(:FORMA \"~a\" :SEM ~a)" (getf cforma :forma) (getf cforma :sem))
					(if (getf cforma :forma)
						(format fout "(:FORMA \"~a\" :GEN ~a :NUM ~a :SEM ~a)" (getf cforma :forma) (getf cforma :gen) (getf cforma :num) (getf cforma :sem))
						(format fout "(:FORMA NIL :GEN ~a :NUM ~a :SEM ~a)" (getf cforma :gen) (getf cforma :num) (getf cforma :sem)))))
			(format fout "))"))
		(format fout "))~%~%")))
		
;(with-open-file (out "dat-lemmi2.lisp" :direction :output) (print-lemmi out))
(defun print-lemmi (fout)

	(format fout "(setf *dat-lemmi* '(~%~%")
	(dolist (lm (sort *dat-lemmi* #'(lambda(x y) (string< (string-downcase (getf x :lex)) (string-downcase (getf y :lex))))))
		(print-lemma fout lm))
	(format fout "))"))



;=====================================================================
; 				   FUNZIONI ARRICCHIMENTO DIZIONARIO
;=====================================================================

(defun select-rad (rads)
	(block fnc
		(dolist (x rads)
			(when x
				(return-from fnc x)))))

(defun mix-rad-des (cur-rad cur-des con ii)
	(mapcar #'(lambda (x) (verb-mod-rad :rad x :des cur-des :con con :ii ii)) cur-rad))

(defun del-des (vox lendes)
	(remove-duplicates (mapcar #'(lambda (x) (subseq x 0 (- (length x) lendes))) vox) :test #'string-equal))

(defun askv ()
"Procedura per la creazione semi-automatica di informazioni per la generazione delle forme flesse dei verbi"
	(block fnc
		(let ((tm (list nil nil nil nil nil nil nil)) ; radici tematiche
			  (cat   100)     ; inizializzo la categoria come verbo regolare
			  (fonte 'utente) ; ovvero apprendimento manuale
			  (con   0) 	  ; coniugazione non definita
			  (dm    0) 	  ; dittongo mobile a 0
			  (ii    1) 	  ; doppia i presente
			  (irr   0) 	  ; supporto per riconoscimento irregolari (irr = 3, verbo completamente irregolare)
			  
			  inf ; forma del verbo all'infinito
			  inf-des ; desinenza del verbo all'infinito presente
			  
			  cur-gen ; unione radice+desinenza
			  usr-verbo ; verbo chiesto all'utente
			  
			  cur-rad ; radice di ogni tema
			  cur-des ; desinenza di ogni tema
			  
			  tmp)
				
				(setf inf (prompt-read "Infinito: ")) ; richesta all'utente della forma all'infinito presente
				
				(setf inf-des (right-str inf 3)) ; determino la coniugazione ("are", "ere", "ire" o altro)
				(push (subseq inf 0 (- (length inf) 3)) (first tm)) ; potenziale radice tematica regolare ottenuta dall'infinito
				
				;; ----------------------------------------------------------
				;; Creazione TEMA-1: Indicativo presente 1a persona singolare
				;; ----------------------------------------------------------
				
				(cond
					((equal inf-des "are")
						(setq con 1))
						
					((equal inf-des "ere")
						(setq con 2))
						
					((equal inf-des "ire")
						(format t "Indicativo presente 1a persona singolare:~%")
						(format t "~4t1) ~ao~%" (first (first tm)))
						(format t "~4t2) ~aisco~%" (first (first tm))) ; verbi incoativi
						(format t "~4t3) altro~%")
						(setf con (str-to-int (prompt-read "> ")))
						(when (equal con 3) (setf con 1))
						(setf con (+ con 2)))
						
					(t
						(setf cat 110) ; potenzialmente irregolare
						(setf con 2)   ; probabile seconda coniugazione
						(setf usr-verbo (arr-prompt-read "Indicativo presente 1a persona plurale> "))
						(setf (first (first tm)) (subseq inf 0 (- (length inf) 2))) ; estrazione radice irregolare 1 (es. verbi "sedurre"/"condurre")
						(setf (second tm) (del-des usr-verbo 4)))) ; estrazione radice irregolare 2 (toglie des "-iamo")
						
				;; ------------------------------------------------------------
				;; Creazione TEMA-2: Indicativo imperfetto 1a persona singolare
				;; ------------------------------------------------------------
				
				(setf cur-rad (select-rad (list (second tm) (first tm))))
				(setf cur-des (nth (1- con) '("avo" "evo" "ivo" "ivo")))
				
				(setf cur-gen (mix-rad-des cur-rad cur-des con ii))
				
				(setf usr-verbo (arr-prompt-read (format nil "Indicativo imperfetto 1a persona singolare ~a: " cur-gen)))
				(when (a-eq-b cur-gen usr-verbo) (setf usr-verbo nil)) ; Se inserito manualmente e' uguale a quello auto-generato, non lo considero
				(when usr-verbo
					;; Se almeno una delle radici in tm2 è uguale a tm1, allora non sovrascrivo tm1 e tm2
					;; ma assegno usr-verbo direttamente a tm1 e lascio tm2 vuoto.
					(setf tmp (del-des usr-verbo 3)) ; toglie des "evo"
					(if (find (first (first tm)) tmp :test #'equal)
						(progn
							(setf (first tm) tmp)
							(setf (second tm) nil))
						(progn
							(incf irr)
							(setf cat 110) ; verbo parzialmente regolare
							(setf (first (first tm)) (subseq inf 0 (1- (length inf)))) ; "dire" => "dir-" / serve per enclisi!
							(setf (second tm) tmp))))

				;; ---------------------------------
				;; Creazione TEMA-3: Dittongo mobile
				;; ---------------------------------
			  
				(setf cur-rad (select-rad (list (second tm) (first tm))))
				(setf cur-des (nth (1- con) '("i" "i" "i" "isci")))
				
				(setf cur-gen (mix-rad-des cur-rad cur-des con ii)) ; ottengo la lista di radici+desinenze
			  
				(setf usr-verbo (arr-prompt-read (format nil "Indicativo presente 2a persona singolare  ~a: " cur-gen)))
				(when (a-eq-b cur-gen usr-verbo) (setf usr-verbo nil))
				(when usr-verbo
					(if (a-eq-b (first tm) usr-verbo) ; studi / stud(i) prima di togliere la i
						
						(setf ii 0)
						
						(progn
							(incf irr)
							(setf cat 110)
							(setf dm 1) ; dittongo mobile 1
							
							(when (equal con 1)
								(setf usr-verbo (mapcar #'(lambda (x)
									(if (or (equal (right-str x 3) "chi") (right-str x 3) "ghi") ; giochi / manchi
										(concatenate 'string (subseq x 0 (- (length x) 2)) "i")
										x
										)) usr-verbo)))

							(setf (third tm) (del-des usr-verbo 1)))))
				
				;; --------------------------------
				;; Creazione TEMA-4: Alternanza 'g'
				;; --------------------------------
				
				(setf cur-rad (select-rad (list (third tm) (second tm) (first tm))))
				(setf cur-des (nth (1- con) '("o" "o" "o" "isco")))
				
				(setf cur-gen (mix-rad-des cur-rad cur-des con ii))
				
				(setf usr-verbo (arr-prompt-read (format nil "Indicativo presente 1a persona singolare  ~a: " cur-gen)))
				(when (a-eq-b cur-gen usr-verbo) (setf usr-verbo nil))
				(when usr-verbo
					(incf irr)
					(setf cat 110)
					(setf dm 2) ; dittongo mobile 2
					(setf (fourth tm) (del-des usr-verbo 1)))
					
				#| IRREGOLARE |#
				
				(when (equal irr 3)
					(setf tm (list nil nil nil nil nil nil nil))
					(setf con 0)
					(setf dm 0)
					(setf cat 120)
					(format t "Verbo completamente irregolare.~%")
					(return-from fnc nil))
				
				;; ---------------------------------------
				;; Creazione TEMA 5: Futuro e condizionale
				;; ---------------------------------------
				
				(setf cur-rad (first tm))
				(setf cur-des (nth (1- con) '("erò" "erò" "irò" "irò")))
				
				(setf cur-gen (mix-rad-des cur-rad cur-des con ii))
				
				(setf usr-verbo (arr-prompt-read (format nil "Indicativo futuro 1a persona singolare  ~a: " cur-gen)))
				(when (a-eq-b cur-gen usr-verbo) (setf usr-verbo nil))
				(when usr-verbo
					(setf cat 110)
					(setf (fifth tm) (del-des usr-verbo 1)))
				
				;; --------------------------------
				;; Creazione TEMA-6: Passato remoto
				;; --------------------------------
			  
				(setf cur-rad (first tm))
				(setf cur-des (nth (1- con) '("ai" "etti" "ii" "ii")))
				
				(setf cur-gen (mix-rad-des cur-rad cur-des con ii))
				(when (equal con 2)
					(setf cur-gen (append cur-gen (mix-rad-des cur-rad "ei" con ii))))
				
				(setf usr-verbo (arr-prompt-read (format nil "Indicativo passato 1a persona singolare  ~a: " cur-gen)))
				
				(when (a-eq-b cur-gen usr-verbo) (setf usr-verbo nil))
				(when (a-in-b usr-verbo cur-gen) (setf usr-verbo nil))
				(when usr-verbo
					(setf cat 110)
					
					(setf (sixth tm) (del-des usr-verbo 1)))
				
				;; ------------------------------------
				;; Creazione TEMA 7: Participio passato
				;; ------------------------------------
			  
				(setf cur-rad (first tm))
				(setf cur-des (nth (1- con) '("ato" "uto" "ito" "ito")))
				
				(setf cur-gen (mix-rad-des cur-rad cur-des con ii))
				
				(setf usr-verbo (arr-prompt-read (format nil "Participio passato maschile singolare  ~a: " cur-gen)))
				(when (a-eq-b cur-gen usr-verbo) (setf usr-verbo nil))
				(when usr-verbo
					(setf cat 110)
					(setf (seventh tm) (del-des usr-verbo 1)))
			  
					;if (ret.con=0) then ret.con:=2; // sperimentale, provvisorio
					;makeVerbo:=ret;
			  
				(list :lex inf :sill "" :cat cat :sub-cat nil :aux nil :fonte (list fonte) :muso nil :valenza nil :con con :dm dm :ii ii :tm tm))))


;
;--------------------------------- Funzioni di rewrite
;

(defun rewrite-cat (cat)

	(cond ((equal cat 200)
		'NOUN-COMMON)
	      ((equal cat 290)
		'NOUN-COMMON)		
	      ((equal cat 400)
	        'ADJ-QUALIF)
	      ((equal cat 490)
	        'ADJ-QUALIF-POST)
	      ((equal cat 300)
	        'ADV)
	      ((equal cat 100)
	        'VERB)
	      (t (progn (print cat) 
		(break)
		))))
	
(defun rewrite-num (num)
	(cond ((equal num 1)
		'SING)
	      ((equal num 2)
	        'PL)
	      ((equal num 3)
	        'ALLVAL)
	      ((equal num 0)
	        "_")
	      (t (progn (format t "error rewrite num: ~a~%" num) (break)))))
	      
(defun rewrite-gen (gen)
	(cond ((equal gen 1)
		'M)
	      ((equal gen 2)
	        'F)
	      ((equal gen 3)
	        'ALLVAL)
	      ((equal gen 0)
	        "_")
	      (t (progn (format t "error rewrite gen: ~a~%" gen) (break)))))

(defun rewrite-mdv (mdv)
	(cond ((equal mdv 1)
		'IND)
	      ((equal mdv 2)
	        'CONDIZ)
	      ((equal mdv 3)
	        'CONG)
	      ((equal mdv 4)
	        'PARTICIPLE)
	      ((equal mdv 5)
	        'GERUND)
	      ((equal mdv 6)
	        'INFINITE)		
	      ((equal mdv 7)
	        'IMPER)
	      ((equal mdv 0)
	        "_")
	      (t (progn (format t "error rewrite mdv: ~a~%" mdv) (break)))))
	      

(defun rewrite-tmp (tmp)
	(cond ((equal tmp 1)
		'PRESENT)
	      ((equal tmp 2)
	        'IMPERFECT)
	      ((equal tmp 3)
	        'PAST)
	      ((equal tmp 4)
	        'FUTURE)
	      ((equal tmp 0)
	        "_")
	      (t (progn (format t "error rewrite tmp: ~a~%" tmp) (break)))))
	      
	
(defun rewrite-prs (prs &optional (cat 000))
	(cond ((equal prs 1)
		1)
	      ((equal prs 2)
	        2)
	      ((equal prs 3)
	        3)
	      ((equal prs 4) ; componente riflessiva trasformata in terza persona
	        3) 
	      ((and (equal prs 0) (equal cat 400))
		    3)
		  ((equal prs 0)
	        "_")
	      (t (progn (format t "error rewrite prs: ~a~%" prs) (break)))))

(defun rewrite-dgr (dgr)
	(cond ((null dgr)
		"_")
	      ((equal dgr 0)
	        "_")
	      (t dgr)))
	      
(defun rewrite-case (gcase)
	(cond ((null gcase)
		"_")
	      ((equal gcase 0)
	        "_")
	      (t gcase)))


;
; -------------------------------- utilities
;

#|
(defun purifica (lemma)
	(let ((new-lemma (copy-tree lemma)) ; il lemma 'purificato' che verra' restituito dalla funzione
		  (str-init-codec (str-declina-tutto lemma))) ; stringa di cofifica declinazione iniziale
		  
			(dolist (mo (getf lemma :morph)) ; scorro :morph (restituiro' new-lemma)
				(dolist (fo (getf mo :forme)) ; scorro :forme
					;; Creo il nuovo lemma purificato da una forma e la relativa codifica
					(let* ((tmp-lemma (elimina-mo-fo-da-lm mo fo new-lemma))
						   (str-tmp-codec (str-declina-tutto tmp-lemma)))
								;; Se la codifica coincide con l'originale, new-lemma viene purificato
								(when (equal str-init-codec str-tmp-codec)
									(setf new-lemma (copy-tree tmp-lemma))))))
	;; Se ho ottenuto un risultato differente dal lemma passato, provo a vedere se ho ancora delle possibilita' di purificazione
	(if (equal lemma new-lemma) new-lemma (purifica new-lemma))))
	
(defun elimina-mo-fo-da-lm (in-mo in-fo in-lemma)
	(let ((new-lemma (copy-tree in-lemma)))
		;; Faccio si' che new-lemma sia uguale a in-lemma senza :morph
		(setf (getf new-lemma :morph) nil) 
		(dolist (mo (getf in-lemma :morph) new-lemma) ; scorro :morph (restituiro' new-lemma)
			(let ((new-forme nil)) ; azzero le nuove forme per questa :cat
				(dolist (fo (getf mo :forme)) ; scorro :forme
					;; Prendo tutte le forme tranne la forma in-fo contenuta nella :cat di in-mo
					(unless (and (equal (getf mo :cat) (getf in-mo :cat)) (equal fo in-fo))
						(push fo new-forme)))
				(when new-forme
					(push (list :cat (getf mo :cat) :forme new-forme) (getf new-lemma :morph)))))))

(defun str-declina-tutto (lm)
"Crea una stringa contenente una particolare codifica univoca relativa all'intera coniugazione del lemma, per tutte le forme di tutti i :morph possibili"
	(let ((str ""))
	
		(setf *lemmi* (list lm)) ;; Inserisco lm all'nth 0 di *lemmi*

		;; Sostantivi e aggettivi
		(dolist (cur-cat '(200 400)) 
			(setf str (concatenate 'string
								   str
								   "(CAT" (format nil "~a" cur-cat)
								   "(11:" (str-by-strlist (gen-lemma 0 cur-cat 1 1) ",")
								   ";12:" (str-by-strlist (gen-lemma 0 cur-cat 1 2) ",")
								   ";21:" (str-by-strlist (gen-lemma 0 cur-cat 2 1) ",")
								   ";22:" (str-by-strlist (gen-lemma 0 cur-cat 2 2) ",")
								   ")")))
									
		;; Avverbi
		(setf str (concatenate 'string
							   str
							   "(CAT300"
							   "(nrm:" (str-by-strlist (gen-lemma 0 300) ",")
							   ";nda:" (str-by-strlist (gen-lemma 0 300 nil nil t) ",") ; nil = gen; nil = num; t = no-direct-access;
							   ")"))
					
		;; Altro
		(dolist (cur-cat '(970 980))
			(setf str (concatenate 'string
								   str
								   "(CAT" (format nil "~a" cur-cat)
								   "(##:" (str-by-strlist (gen-lemma 0 cur-cat) ",")
								   ")")))
			
		str))

|#

