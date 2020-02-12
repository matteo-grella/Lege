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

#|
 | INFORMAZIONI MORFOLOGICHE.
 |
 | Numero
 | 	SINGOLARE 	1
 | 	PLURALE 	2
 | 	INVARIABILE 3
 | 
 | Genere
 | 	MASCHILE	1
 |  FEMMINILE	2
 | 
 | Persona
 | 	PRIMA_PERSONA 	  1
 | 	SECONDA_PERSONA	  2
 | 	TERZA_PERSONA	  3
 |
 | Modo
 | 	INDICATIVO	  1
 | 	CONDIZIONALE  2
 | 	CONGIUNTIVO   3
 | 	PARTICIPIO 	  4
 | 	GERUNDIO 	  5
 | 	INFINITO	  6
 | 	IMPERATIVO 	  7
 |	
 | Tempi semplici
 | 	PRESENTE 	1
 | 	IMPERFETTO  2
 | 	PASSATO     3 {remoto}
 | 	FUTURO 	    4 {semplice}
 | 	
 | Tempi composti (valore composto da [tempo_semplice_ausiliare][modo_participio_passato]) 
 | 	C_PASSATO			  14 { Prossimo }
 | 	C_TRAPASSATO 		  24 { Prossimo }
 | 	C_TRAPASSATO_REMOTO   34
 | 	C_FUTURO_ANTERIORE 	  44
 | 
 |  
 | TEMI.
 | Il formalismo utilizzato per la rappresentazione delle desinenze e delle radici tematiche 
 | e' stato dedotto da diverse grammatiche, in particolare da un capitolo della grammatica italiana di Lepschky.
 | 
 | Numero radice tematica (valida solo per verbi con cat = 100 e 110):
 | 	1=radice regolare
 | 	2=irregolare
 | 	3=irregolare dittongo mobile 1
 | 	4=irregolare dittongo mobile 2
 | 	5=irregolare futuro e condizionale
 | 	6=irregolare passato remoto
 | 	7=irregolare participio passato
 | 
|#



; elenco delle desinenze disponibili corredate di restrizioni per temi (mix di *lista-des* e *tm-assoc*)

(let ((ldes '(
		(:con 3 :mdv 4 :tmp 1 :prs 0 :num 2 :gen 0 :des "ienti")
		(:con 3 :mdv 4 :tmp 1 :prs 0 :num 1 :gen 0 :des "iente")
		(:con 5 :mdv 1 :tmp 4 :prs 3 :num 1 :gen 0 :des "à")
		(:con 5 :mdv 1 :tmp 4 :prs 1 :num 1 :gen 0 :des "ò")
		(:con 4 :mdv 7 :tmp 1 :prs 3 :num 2 :gen 0 :des "iscano")
		(:con 4 :mdv 7 :tmp 1 :prs 3 :num 1 :gen 0 :des "isca")
		(:con 4 :mdv 7 :tmp 1 :prs 2 :num 1 :gen 0 :des "isci")
		(:con 4 :mdv 3 :tmp 1 :prs 3 :num 2 :gen 0 :des "iscano")
		(:con 4 :mdv 3 :tmp 1 :prs 3 :num 1 :gen 0 :des "isca")
		(:con 4 :mdv 3 :tmp 1 :prs 2 :num 1 :gen 0 :des "isca")
		(:con 4 :mdv 3 :tmp 1 :prs 1 :num 1 :gen 0 :des "isca")
		(:con 4 :mdv 1 :tmp 1 :prs 3 :num 2 :gen 0 :des "iscono")
		(:con 4 :mdv 1 :tmp 1 :prs 3 :num 1 :gen 0 :des "isce")
		(:con 4 :mdv 1 :tmp 1 :prs 2 :num 1 :gen 0 :des "isci")
		(:con 4 :mdv 1 :tmp 1 :prs 1 :num 1 :gen 0 :des "isco")
		(:con 7 :mdv 4 :tmp 3 :prs 0 :num 2 :gen 1 :des "i")
		(:con 7 :mdv 4 :tmp 3 :prs 0 :num 1 :gen 2 :des "a")
		(:con 7 :mdv 4 :tmp 3 :prs 0 :num 2 :gen 2 :des "e")
		(:con 7 :mdv 4 :tmp 3 :prs 0 :num 1 :gen 1 :des "o")
		(:con 6 :mdv 1 :tmp 3 :prs 3 :num 2 :gen 0 :des "ero")
		(:con 6 :mdv 1 :tmp 3 :prs 1 :num 1 :gen 0 :des "i")
		(:con 6 :mdv 1 :tmp 3 :prs 3 :num 1 :gen 0 :des "e")
		(:con 6 :mdv 1 :tmp 1 :prs 1 :num 1 :gen 0 :des "o")
		(:con 5 :mdv 2 :tmp 1 :prs 1 :num 2 :gen 0 :des "emmo")
		(:con 5 :mdv 2 :tmp 1 :prs 3 :num 1 :gen 0 :des "ebbe")
		(:con 5 :mdv 2 :tmp 1 :prs 2 :num 2 :gen 0 :des "este")
		(:con 5 :mdv 2 :tmp 1 :prs 1 :num 1 :gen 0 :des "ei")
		(:con 5 :mdv 2 :tmp 1 :prs 2 :num 1 :gen 0 :des "esti")
		(:con 5 :mdv 2 :tmp 1 :prs 3 :num 2 :gen 0 :des "ebbero")
		(:con 5 :mdv 1 :tmp 4 :prs 3 :num 2 :gen 0 :des "anno")
		(:con 5 :mdv 1 :tmp 4 :prs 1 :num 2 :gen 0 :des "emo")
		(:con 5 :mdv 1 :tmp 4 :prs 2 :num 1 :gen 0 :des "ai")
		(:con 5 :mdv 1 :tmp 4 :prs 2 :num 2 :gen 0 :des "ete")
		(:con 5 :mdv 1 :tmp 3 :prs 2 :num 1 :gen 0 :des "esti")
		(:con 5 :mdv 1 :tmp 3 :prs 2 :num 2 :gen 0 :des "este")
		(:con 3 :mdv 6 :tmp 1 :prs 0 :num 0 :gen 0 :des "ire")
		(:con 2 :mdv 6 :tmp 1 :prs 0 :num 0 :gen 0 :des "ere")
		(:con 1 :mdv 6 :tmp 1 :prs 0 :num 0 :gen 0 :des "are")
		(:con 3 :mdv 4 :tmp 3 :prs 0 :num 2 :gen 2 :des "ite")
		(:con 3 :mdv 4 :tmp 3 :prs 0 :num 2 :gen 1 :des "iti")
		(:con 3 :mdv 4 :tmp 3 :prs 0 :num 1 :gen 2 :des "ita")
		(:con 3 :mdv 4 :tmp 3 :prs 0 :num 1 :gen 1 :des "ito")
		(:con 2 :mdv 4 :tmp 3 :prs 0 :num 2 :gen 2 :des "ute")
		(:con 2 :mdv 4 :tmp 3 :prs 0 :num 2 :gen 1 :des "uti")
		(:con 2 :mdv 4 :tmp 3 :prs 0 :num 1 :gen 2 :des "uta")
		(:con 2 :mdv 4 :tmp 3 :prs 0 :num 1 :gen 1 :des "uto")
		(:con 1 :mdv 4 :tmp 3 :prs 0 :num 2 :gen 2 :des "ate")
		(:con 1 :mdv 4 :tmp 3 :prs 0 :num 2 :gen 1 :des "ati")
		(:con 1 :mdv 4 :tmp 3 :prs 0 :num 1 :gen 2 :des "ata")
		(:con 1 :mdv 4 :tmp 3 :prs 0 :num 1 :gen 1 :des "ato")
		(:con 3 :mdv 4 :tmp 1 :prs 0 :num 2 :gen 0 :des "enti")
		(:con 3 :mdv 4 :tmp 1 :prs 0 :num 1 :gen 0 :des "ente")
		(:con 2 :mdv 4 :tmp 1 :prs 0 :num 2 :gen 0 :des "enti")
		(:con 2 :mdv 4 :tmp 1 :prs 0 :num 1 :gen 0 :des "ente")
		(:con 1 :mdv 4 :tmp 1 :prs 0 :num 2 :gen 0 :des "anti")
		(:con 1 :mdv 4 :tmp 1 :prs 0 :num 1 :gen 0 :des "ante")
		(:con 3 :mdv 5 :tmp 1 :prs 0 :num 0 :gen 0 :des "endo")
		(:con 2 :mdv 5 :tmp 1 :prs 0 :num 0 :gen 0 :des "endo")
		(:con 1 :mdv 5 :tmp 1 :prs 0 :num 0 :gen 0 :des "ando")
		(:con 3 :mdv 7 :tmp 1 :prs 3 :num 2 :gen 0 :des "ano")
		(:con 3 :mdv 7 :tmp 1 :prs 2 :num 2 :gen 0 :des "ite")
		(:con 3 :mdv 7 :tmp 1 :prs 1 :num 2 :gen 0 :des "iamo")
		(:con 3 :mdv 7 :tmp 1 :prs 3 :num 1 :gen 0 :des "a")
		(:con 3 :mdv 7 :tmp 1 :prs 2 :num 1 :gen 0 :des "i")
		(:con 2 :mdv 7 :tmp 1 :prs 3 :num 2 :gen 0 :des "ano")
		(:con 2 :mdv 7 :tmp 1 :prs 2 :num 2 :gen 0 :des "ete")
		(:con 2 :mdv 7 :tmp 1 :prs 1 :num 2 :gen 0 :des "iamo")
		(:con 2 :mdv 7 :tmp 1 :prs 3 :num 1 :gen 0 :des "a")
		(:con 2 :mdv 7 :tmp 1 :prs 2 :num 1 :gen 0 :des "i")
		(:con 1 :mdv 7 :tmp 1 :prs 3 :num 2 :gen 0 :des "ino")
		(:con 1 :mdv 7 :tmp 1 :prs 2 :num 2 :gen 0 :des "ate")
		(:con 1 :mdv 7 :tmp 1 :prs 1 :num 2 :gen 0 :des "iamo")
		(:con 1 :mdv 7 :tmp 1 :prs 3 :num 1 :gen 0 :des "i")
		(:con 1 :mdv 7 :tmp 1 :prs 2 :num 1 :gen 0 :des "a")
		(:con 3 :mdv 2 :tmp 1 :prs 3 :num 2 :gen 0 :des "irebbero")
		(:con 3 :mdv 2 :tmp 1 :prs 2 :num 2 :gen 0 :des "ireste")
		(:con 3 :mdv 2 :tmp 1 :prs 1 :num 2 :gen 0 :des "iremmo")
		(:con 3 :mdv 2 :tmp 1 :prs 3 :num 1 :gen 0 :des "irebbe")
		(:con 3 :mdv 2 :tmp 1 :prs 2 :num 1 :gen 0 :des "iresti")
		(:con 3 :mdv 2 :tmp 1 :prs 1 :num 1 :gen 0 :des "irei")
		(:con 2 :mdv 2 :tmp 1 :prs 3 :num 2 :gen 0 :des "erebbero")
		(:con 2 :mdv 2 :tmp 1 :prs 2 :num 2 :gen 0 :des "ereste")
		(:con 2 :mdv 2 :tmp 1 :prs 1 :num 2 :gen 0 :des "eremmo")
		(:con 2 :mdv 2 :tmp 1 :prs 3 :num 1 :gen 0 :des "erebbe")
		(:con 2 :mdv 2 :tmp 1 :prs 2 :num 1 :gen 0 :des "eresti")
		(:con 2 :mdv 2 :tmp 1 :prs 1 :num 1 :gen 0 :des "erei")
		(:con 1 :mdv 2 :tmp 1 :prs 3 :num 2 :gen 0 :des "erebbero")
		(:con 1 :mdv 2 :tmp 1 :prs 2 :num 2 :gen 0 :des "ereste")
		(:con 1 :mdv 2 :tmp 1 :prs 1 :num 2 :gen 0 :des "eremmo")
		(:con 1 :mdv 2 :tmp 1 :prs 3 :num 1 :gen 0 :des "erebbe")
		(:con 1 :mdv 2 :tmp 1 :prs 2 :num 1 :gen 0 :des "eresti")
		(:con 1 :mdv 2 :tmp 1 :prs 1 :num 1 :gen 0 :des "erei")
		(:con 3 :mdv 3 :tmp 2 :prs 3 :num 2 :gen 0 :des "issero")
		(:con 3 :mdv 3 :tmp 2 :prs 2 :num 2 :gen 0 :des "iste")
		(:con 3 :mdv 3 :tmp 2 :prs 1 :num 2 :gen 0 :des "issimo")
		(:con 3 :mdv 3 :tmp 2 :prs 3 :num 1 :gen 0 :des "isse")
		(:con 3 :mdv 3 :tmp 2 :prs 2 :num 1 :gen 0 :des "issi")
		(:con 3 :mdv 3 :tmp 2 :prs 1 :num 1 :gen 0 :des "issi")
		(:con 2 :mdv 3 :tmp 2 :prs 3 :num 2 :gen 0 :des "essero")
		(:con 2 :mdv 3 :tmp 2 :prs 2 :num 2 :gen 0 :des "este")
		(:con 2 :mdv 3 :tmp 2 :prs 1 :num 2 :gen 0 :des "essimo")
		(:con 2 :mdv 3 :tmp 2 :prs 3 :num 1 :gen 0 :des "esse")
		(:con 2 :mdv 3 :tmp 2 :prs 2 :num 1 :gen 0 :des "essi")
		(:con 2 :mdv 3 :tmp 2 :prs 1 :num 1 :gen 0 :des "essi")
		(:con 1 :mdv 3 :tmp 2 :prs 3 :num 2 :gen 0 :des "assero")
		(:con 1 :mdv 3 :tmp 2 :prs 2 :num 2 :gen 0 :des "aste")
		(:con 1 :mdv 3 :tmp 2 :prs 1 :num 2 :gen 0 :des "assimo")
		(:con 1 :mdv 3 :tmp 2 :prs 3 :num 1 :gen 0 :des "asse")
		(:con 1 :mdv 3 :tmp 2 :prs 2 :num 1 :gen 0 :des "assi")
		(:con 1 :mdv 3 :tmp 2 :prs 1 :num 1 :gen 0 :des "assi")
		(:con 3 :mdv 3 :tmp 1 :prs 3 :num 2 :gen 0 :des "ano")
		(:con 3 :mdv 3 :tmp 1 :prs 2 :num 2 :gen 0 :des "iate")
		(:con 3 :mdv 3 :tmp 1 :prs 1 :num 2 :gen 0 :des "iamo")
		(:con 3 :mdv 3 :tmp 1 :prs 3 :num 1 :gen 0 :des "a")
		(:con 3 :mdv 3 :tmp 1 :prs 2 :num 1 :gen 0 :des "a")
		(:con 3 :mdv 3 :tmp 1 :prs 1 :num 1 :gen 0 :des "a")
		(:con 2 :mdv 3 :tmp 1 :prs 3 :num 2 :gen 0 :des "ano")
		(:con 2 :mdv 3 :tmp 1 :prs 2 :num 2 :gen 0 :des "iate")
		(:con 2 :mdv 3 :tmp 1 :prs 1 :num 2 :gen 0 :des "iamo")
		(:con 2 :mdv 3 :tmp 1 :prs 3 :num 1 :gen 0 :des "a")
		(:con 2 :mdv 3 :tmp 1 :prs 2 :num 1 :gen 0 :des "a")
		(:con 2 :mdv 3 :tmp 1 :prs 1 :num 1 :gen 0 :des "a")
		(:con 1 :mdv 3 :tmp 1 :prs 3 :num 2 :gen 0 :des "ino")
		(:con 1 :mdv 3 :tmp 1 :prs 2 :num 2 :gen 0 :des "iate")
		(:con 1 :mdv 3 :tmp 1 :prs 1 :num 2 :gen 0 :des "iamo")
		(:con 1 :mdv 3 :tmp 1 :prs 3 :num 1 :gen 0 :des "i")
		(:con 1 :mdv 3 :tmp 1 :prs 2 :num 1 :gen 0 :des "i")
		(:con 1 :mdv 3 :tmp 1 :prs 1 :num 1 :gen 0 :des "i")
		(:con 3 :mdv 1 :tmp 3 :prs 3 :num 2 :gen 0 :des "irono")
		(:con 3 :mdv 1 :tmp 3 :prs 2 :num 2 :gen 0 :des "iste")
		(:con 3 :mdv 1 :tmp 3 :prs 1 :num 2 :gen 0 :des "immo")
		(:con 3 :mdv 1 :tmp 3 :prs 3 :num 1 :gen 0 :des "ì")
		(:con 3 :mdv 1 :tmp 3 :prs 2 :num 1 :gen 0 :des "isti")
		(:con 3 :mdv 1 :tmp 3 :prs 1 :num 1 :gen 0 :des "ii")
		(:con 2 :mdv 1 :tmp 3 :prs 3 :num 2 :gen 0 :des "ettero")
		(:con 2 :mdv 1 :tmp 3 :prs 3 :num 2 :gen 0 :des "erono")
		(:con 2 :mdv 1 :tmp 3 :prs 2 :num 2 :gen 0 :des "este")
		(:con 2 :mdv 1 :tmp 3 :prs 1 :num 2 :gen 0 :des "emmo")
		(:con 2 :mdv 1 :tmp 3 :prs 3 :num 1 :gen 0 :des "ette")
		(:con 2 :mdv 1 :tmp 3 :prs 3 :num 1 :gen 0 :des "é")
		(:con 2 :mdv 1 :tmp 3 :prs 2 :num 1 :gen 0 :des "esti")
		(:con 2 :mdv 1 :tmp 3 :prs 1 :num 1 :gen 0 :des "etti")
		(:con 2 :mdv 1 :tmp 3 :prs 1 :num 1 :gen 0 :des "ei")
		(:con 1 :mdv 1 :tmp 3 :prs 3 :num 2 :gen 0 :des "arono")
		(:con 1 :mdv 1 :tmp 3 :prs 2 :num 2 :gen 0 :des "aste")
		(:con 1 :mdv 1 :tmp 3 :prs 1 :num 2 :gen 0 :des "ammo")
		(:con 1 :mdv 1 :tmp 3 :prs 3 :num 1 :gen 0 :des "ò")
		(:con 1 :mdv 1 :tmp 3 :prs 2 :num 1 :gen 0 :des "asti")
		(:con 1 :mdv 1 :tmp 3 :prs 1 :num 1 :gen 0 :des "ai")
		(:con 3 :mdv 1 :tmp 2 :prs 3 :num 2 :gen 0 :des "ivano")
		(:con 3 :mdv 1 :tmp 2 :prs 2 :num 2 :gen 0 :des "ivate")
		(:con 3 :mdv 1 :tmp 2 :prs 1 :num 2 :gen 0 :des "ivamo")
		(:con 3 :mdv 1 :tmp 2 :prs 3 :num 1 :gen 0 :des "iva")
		(:con 3 :mdv 1 :tmp 2 :prs 2 :num 1 :gen 0 :des "ivi")
		(:con 3 :mdv 1 :tmp 2 :prs 1 :num 1 :gen 0 :des "ivo")
		(:con 2 :mdv 1 :tmp 2 :prs 3 :num 2 :gen 0 :des "evano")
		(:con 2 :mdv 1 :tmp 2 :prs 2 :num 2 :gen 0 :des "evate")
		(:con 2 :mdv 1 :tmp 2 :prs 1 :num 2 :gen 0 :des "evamo")
		(:con 2 :mdv 1 :tmp 2 :prs 3 :num 1 :gen 0 :des "eva")
		(:con 2 :mdv 1 :tmp 2 :prs 2 :num 1 :gen 0 :des "evi")
		(:con 2 :mdv 1 :tmp 2 :prs 1 :num 1 :gen 0 :des "evo")
		(:con 1 :mdv 1 :tmp 2 :prs 3 :num 2 :gen 0 :des "avano")
		(:con 1 :mdv 1 :tmp 2 :prs 2 :num 2 :gen 0 :des "avate")
		(:con 1 :mdv 1 :tmp 2 :prs 1 :num 2 :gen 0 :des "avamo")
		(:con 1 :mdv 1 :tmp 2 :prs 3 :num 1 :gen 0 :des "ava")
		(:con 1 :mdv 1 :tmp 2 :prs 2 :num 1 :gen 0 :des "avi")
		(:con 1 :mdv 1 :tmp 2 :prs 1 :num 1 :gen 0 :des "avo")
		(:con 3 :mdv 1 :tmp 4 :prs 3 :num 2 :gen 0 :des "iranno")
		(:con 3 :mdv 1 :tmp 4 :prs 2 :num 2 :gen 0 :des "irete")
		(:con 3 :mdv 1 :tmp 4 :prs 1 :num 2 :gen 0 :des "iremo")
		(:con 3 :mdv 1 :tmp 4 :prs 3 :num 1 :gen 0 :des "irà")
		(:con 3 :mdv 1 :tmp 4 :prs 2 :num 1 :gen 0 :des "irai")
		(:con 3 :mdv 1 :tmp 4 :prs 1 :num 1 :gen 0 :des "irò")
		(:con 2 :mdv 1 :tmp 4 :prs 3 :num 2 :gen 0 :des "eranno")
		(:con 2 :mdv 1 :tmp 4 :prs 2 :num 2 :gen 0 :des "erete")
		(:con 2 :mdv 1 :tmp 4 :prs 1 :num 2 :gen 0 :des "eremo")
		(:con 2 :mdv 1 :tmp 4 :prs 3 :num 1 :gen 0 :des "erà")
		(:con 2 :mdv 1 :tmp 4 :prs 2 :num 1 :gen 0 :des "erai")
		(:con 2 :mdv 1 :tmp 4 :prs 1 :num 1 :gen 0 :des "erò")
		(:con 1 :mdv 1 :tmp 4 :prs 3 :num 2 :gen 0 :des "eranno")
		(:con 1 :mdv 1 :tmp 4 :prs 2 :num 2 :gen 0 :des "erete")
		(:con 1 :mdv 1 :tmp 4 :prs 1 :num 2 :gen 0 :des "eremo")
		(:con 1 :mdv 1 :tmp 4 :prs 3 :num 1 :gen 0 :des "erà")
		(:con 1 :mdv 1 :tmp 4 :prs 2 :num 1 :gen 0 :des "erai")
		(:con 1 :mdv 1 :tmp 4 :prs 1 :num 1 :gen 0 :des "erò")
		(:con 3 :mdv 1 :tmp 1 :prs 3 :num 2 :gen 0 :des "ono")
		(:con 3 :mdv 1 :tmp 1 :prs 2 :num 2 :gen 0 :des "ite")
		(:con 3 :mdv 1 :tmp 1 :prs 1 :num 2 :gen 0 :des "iamo")
		(:con 3 :mdv 1 :tmp 1 :prs 3 :num 1 :gen 0 :des "e")
		(:con 3 :mdv 1 :tmp 1 :prs 2 :num 1 :gen 0 :des "i")
		(:con 3 :mdv 1 :tmp 1 :prs 1 :num 1 :gen 0 :des "o")
		(:con 2 :mdv 1 :tmp 1 :prs 3 :num 2 :gen 0 :des "ono")
		(:con 2 :mdv 1 :tmp 1 :prs 2 :num 2 :gen 0 :des "ete")
		(:con 2 :mdv 1 :tmp 1 :prs 1 :num 2 :gen 0 :des "iamo")
		(:con 2 :mdv 1 :tmp 1 :prs 3 :num 1 :gen 0 :des "e")
		(:con 2 :mdv 1 :tmp 1 :prs 2 :num 1 :gen 0 :des "i")
		(:con 2 :mdv 1 :tmp 1 :prs 1 :num 1 :gen 0 :des "o")
		(:con 1 :mdv 1 :tmp 1 :prs 3 :num 2 :gen 0 :des "ano")
		(:con 1 :mdv 1 :tmp 1 :prs 2 :num 2 :gen 0 :des "ate")
		(:con 1 :mdv 1 :tmp 1 :prs 1 :num 2 :gen 0 :des "iamo")
		(:con 1 :mdv 1 :tmp 1 :prs 3 :num 1 :gen 0 :des "a")
		(:con 1 :mdv 1 :tmp 1 :prs 2 :num 1 :gen 0 :des "i")
		(:con 1 :mdv 1 :tmp 1 :prs 1 :num 1 :gen 0 :des "o")
		;-------- desinenze di con 4 uguali a con 3------------;
		(:con 4 :mdv 4 :tmp 1 :prs 0 :num 2 :gen 0 :des "ienti")
		(:con 4 :mdv 4 :tmp 1 :prs 0 :num 1 :gen 0 :des "iente")
		(:con 4 :mdv 6 :tmp 1 :prs 0 :num 0 :gen 0 :des "ire") 
		(:con 4 :mdv 4 :tmp 3 :prs 0 :num 2 :gen 2 :des "ite")
		(:con 4 :mdv 4 :tmp 3 :prs 0 :num 2 :gen 1 :des "iti") 
		(:con 4 :mdv 4 :tmp 3 :prs 0 :num 1 :gen 2 :des "ita")
		(:con 4 :mdv 4 :tmp 3 :prs 0 :num 1 :gen 1 :des "ito")
		(:con 4 :mdv 4 :tmp 1 :prs 0 :num 2 :gen 0 :des "enti")
		(:con 4 :mdv 4 :tmp 1 :prs 0 :num 1 :gen 0 :des "ente") 
		(:con 4 :mdv 5 :tmp 1 :prs 0 :num 0 :gen 0 :des "endo")
		(:con 4 :mdv 7 :tmp 1 :prs 2 :num 2 :gen 0 :des "ite") 
		(:con 4 :mdv 7 :tmp 1 :prs 1 :num 2 :gen 0 :des "iamo")
		(:con 4 :mdv 2 :tmp 1 :prs 3 :num 2 :gen 0 :des "irebbero")
		(:con 4 :mdv 2 :tmp 1 :prs 2 :num 2 :gen 0 :des "ireste")
		(:con 4 :mdv 2 :tmp 1 :prs 1 :num 2 :gen 0 :des "iremmo") 
		(:con 4 :mdv 2 :tmp 1 :prs 3 :num 1 :gen 0 :des "irebbe")
		(:con 4 :mdv 2 :tmp 1 :prs 2 :num 1 :gen 0 :des "iresti") 
		(:con 4 :mdv 2 :tmp 1 :prs 1 :num 1 :gen 0 :des "irei")
		(:con 4 :mdv 3 :tmp 2 :prs 3 :num 2 :gen 0 :des "issero")
		(:con 4 :mdv 3 :tmp 2 :prs 2 :num 2 :gen 0 :des "iste")
		(:con 4 :mdv 3 :tmp 2 :prs 1 :num 2 :gen 0 :des "issimo") 
		(:con 4 :mdv 3 :tmp 2 :prs 3 :num 1 :gen 0 :des "isse")
		(:con 4 :mdv 3 :tmp 2 :prs 2 :num 1 :gen 0 :des "issi") 
		(:con 4 :mdv 3 :tmp 2 :prs 1 :num 1 :gen 0 :des "issi")
		(:con 4 :mdv 3 :tmp 1 :prs 2 :num 2 :gen 0 :des "iate")
		(:con 4 :mdv 3 :tmp 1 :prs 1 :num 2 :gen 0 :des "iamo")
		(:con 4 :mdv 1 :tmp 3 :prs 3 :num 2 :gen 0 :des "irono")
		(:con 4 :mdv 1 :tmp 3 :prs 2 :num 2 :gen 0 :des "iste")
		(:con 4 :mdv 1 :tmp 3 :prs 1 :num 2 :gen 0 :des "immo") 
		(:con 4 :mdv 1 :tmp 3 :prs 3 :num 1 :gen 0 :des "ì")
		(:con 4 :mdv 1 :tmp 3 :prs 2 :num 1 :gen 0 :des "isti") 
		(:con 4 :mdv 1 :tmp 3 :prs 1 :num 1 :gen 0 :des "ii")
		(:con 4 :mdv 1 :tmp 2 :prs 3 :num 2 :gen 0 :des "ivano") 
		(:con 4 :mdv 1 :tmp 2 :prs 2 :num 2 :gen 0 :des "ivate")
		(:con 4 :mdv 1 :tmp 2 :prs 1 :num 2 :gen 0 :des "ivamo") 
		(:con 4 :mdv 1 :tmp 2 :prs 3 :num 1 :gen 0 :des "iva")
		(:con 4 :mdv 1 :tmp 2 :prs 2 :num 1 :gen 0 :des "ivi")
		(:con 4 :mdv 1 :tmp 2 :prs 1 :num 1 :gen 0 :des "ivo")
		(:con 4 :mdv 1 :tmp 4 :prs 3 :num 2 :gen 0 :des "iranno")
		(:con 4 :mdv 1 :tmp 4 :prs 2 :num 2 :gen 0 :des "irete")
		(:con 4 :mdv 1 :tmp 4 :prs 1 :num 2 :gen 0 :des "iremo")
		(:con 4 :mdv 1 :tmp 4 :prs 3 :num 1 :gen 0 :des "irà")
		(:con 4 :mdv 1 :tmp 4 :prs 2 :num 1 :gen 0 :des "irai") 
		(:con 4 :mdv 1 :tmp 4 :prs 1 :num 1 :gen 0 :des "irò")
		(:con 4 :mdv 1 :tmp 1 :prs 2 :num 2 :gen 0 :des "ite") 
		(:con 4 :mdv 1 :tmp 1 :prs 1 :num 2 :gen 0 :des "iamo")))
		
	 (tm-assoc '( ; associazioni del numero di temi compatibili (ntm) con le diverse declinazioni
		(:mdv 6 :tmp 1 :prs 0 :num 0 :gen 0
			:restr ((:dm 0 :ntm (1))))
			
		(:mdv 4 :tmp 3 :prs 0 :num 0 :gen 0
			:restr ((:dm 0 :ntm (7 1))))
			
		(:mdv 4 :tmp 1 :prs 0 :num 0 :gen 0
			:restr ((:dm 0 :ntm (2 1))))
			
		(:mdv 5 :tmp 1 :prs 0 :num 0 :gen 0
			:restr ((:dm 0 :ntm (2 1))))
		
		(:mdv 7 :tmp 1 :prs 2 :num 2 :gen 0
			:restr ((:dm 0 :ntm (2 1))))
		
		(:mdv 7 :tmp 1 :prs 1 :num 2 :gen 0
			:restr ((:dm 0 :ntm (2 1))))
		
		(:mdv 7 :tmp 1 :prs 3 :num 2 :gen 0
			:restr ((:dm 0 :ntm (4 2 1)) 
					(:dm 1 :ntm (4 3 2 1)) 
					(:dm 2 :ntm (4 3 2 1))))
		
		(:mdv 7 :tmp 1 :prs 3 :num 1 :gen 0 
			:restr ((:dm 0 :ntm (4 2 1))
					(:dm 1 :ntm (4 3 2 1))
					(:dm 2 :ntm (4 3 2 1))))
		
		(:mdv 7 :tmp 1 :prs 2 :num 1 :gen 0
			:restr ((:dm 0 :ntm (2 1))
					(:dm 1 :ntm (3 2 1))
					(:dm 2 :ntm (3 2 1))))
		
		(:mdv 2 :tmp 1 :prs 0 :num 0 :gen 0
			:restr ((:dm 0 :ntm (5 1))
					(:dm 1 :ntm (5 1)))) ; eliminato tema 3 il 7 aprile 2012
		
		(:mdv 3 :tmp 2 :prs 0 :num 0 :gen 0
			:restr ((:dm 0 :ntm (2 1))))
		
		(:mdv 3 :tmp 1 :prs 2 :num 2 :gen 0
			:restr ((:dm 0 :ntm (2 1))))
			
		(:mdv 3 :tmp 1 :prs 1 :num 2 :gen 0
			:restr ((:dm 0 :ntm (2 1))))
		
		(:mdv 3 :tmp 1 :prs 3 :num 2 :gen 0
			:restr ((:dm 0 :ntm (4 2 1))
					(:dm 1 :ntm (4 3 2 1))))
		
		(:mdv 3 :tmp 1 :prs 3 :num 1 :gen 0
			:restr ((:dm 0 :ntm (4 2 1))
					(:dm 1 :ntm (4 3 2 1))))
		
		(:mdv 3 :tmp 1 :prs 2 :num 1 :gen 0
			:restr ((:dm 0 :ntm (4 2 1))
					(:dm 1 :ntm (4 3 2 1))))
		
		(:mdv 3 :tmp 1 :prs 1 :num 1 :gen 0
			:restr ((:dm 0 :ntm (4 2 1))
					(:dm 1 :ntm (4 3 2 1))))
		
		(:mdv 1 :tmp 4 :prs 0 :num 0 :gen 0 
			:restr ((:dm 0 :ntm (5 1))
					(:dm 1 :ntm (5 1)))) ; eliminato tema 3 il 7 aprile 2012
		
		(:mdv 1 :tmp 3 :prs 2 :num 2 :gen 0
			:restr ((:dm 0 :ntm (2 1))))
			
		(:mdv 1 :tmp 3 :prs 1 :num 2 :gen 0
			:restr ((:dm 0 :ntm (2 1))))
			
		(:mdv 1 :tmp 3 :prs 2 :num 1 :gen 0
			:restr ((:dm 0 :ntm (2 1))))
		
		(:mdv 1 :tmp 3 :prs 3 :num 2 :gen 0
			:restr ((:dm 0 :ntm (6 2 1))))
		
		(:mdv 1 :tmp 3 :prs 3 :num 1 :gen 0
			:restr ((:dm 0 :ntm (6 2 1))))
			
		(:mdv 1 :tmp 3 :prs 1 :num 1 :gen 0
			:restr ((:dm 0 :ntm (6 2 1))))
			
		(:mdv 1 :tmp 2 :prs 0 :num 0 :gen 0
			:restr ((:dm 0 :ntm (2 1))))
		
		(:mdv 1 :tmp 1 :prs 3 :num 2 :gen 0
			:restr ((:dm 0 :ntm (4 2 1))
					(:dm 1 :ntm (4 3 2 1))
					(:dm 2 :ntm (4 3 2 1))))
		
		(:mdv 1 :tmp 1 :prs 2 :num 2 :gen 0
			:restr ((:dm 0 :ntm (2 1))))
		
		(:mdv 1 :tmp 1 :prs 1 :num 2 :gen 0
			:restr ((:dm 0 :ntm (2 1))))
		
		(:mdv 1 :tmp 1 :prs 3 :num 1 :gen 0
			:restr ((:dm 0 :ntm (2 1))
					(:dm 1 :ntm (3 2 1))
					(:dm 2 :ntm (3 2 1))))
		
		(:mdv 1 :tmp 1 :prs 2 :num 1 :gen 0
			:restr ((:dm 0 :ntm (2 1))
					(:dm 1 :ntm (3 2 1))
					(:dm 2 :ntm (3 2 1))))
		
		(:mdv 1 :tmp 1 :prs 1 :num 1 :gen 0
			:restr ((:dm 0 :ntm (4 2 1))
					(:dm 1 :ntm (4 3 2 1)))))))


	;; Carica *desinenze* unendo le restrizioni di *tm-assoc* con la lista di desinenze *lista-des*
	(defvar *desinenze* (mapcar #'(lambda (p-des) (append p-des (list :restr ; di seguito, ciclo per catturare le restrizioni
		(dolist (tema tm-assoc)
			(when (and
					;
					; Modo e tempo del verbo devono coincidere 
					(equal (getf p-des :mdv) (getf tema :mdv))
					(equal (getf p-des :tmp) (getf tema :tmp))
					;
					; Persona, Numero, Genere sono generici (compatibilita con zero)
					(or (equal (getf p-des :prs) (getf tema :prs)) (zerop (getf tema :prs)))
					(or (equal (getf p-des :num) (getf tema :num)) (zerop (getf tema :num)))
					(or (equal (getf p-des :gen) (getf tema :gen)) (zerop (getf tema :gen))))
				  
				  (return (getf tema :restr)))))))
		ldes)))
