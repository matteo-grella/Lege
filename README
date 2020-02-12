# Lege 

Lege is a rule-based morphological analyzer for the Italian language written in Common Lisp.
The algorithms for the analysis and the generation of surface word forms are based on phonological and morphological rules of the Italian language. Particular attention has been given to the verbs, for which rules have been extracted from the famous A.L e G. Lepschy, La lingua italiana.

Main Author: Matteo Grella

Collaborators: 
- Marco Nicola
- Daniel Christen

## Requirements

- CLISP

## Usage

1. exec start_lege.sh
2. enjoy :)

## Top functions

### Morphological analysis
> (analyze "Lei suona il piano e lui la tromba")
> 
p,wrd,parola
0,0,lei
0,1,suona
0,2,il
0,3,piano
0,4,e
0,5,lui
0,6,la
0,7,tromba
0,8,.

p-cnt,wrd-cnt,te,cnt,cnt-next,cat,id,mdv,tmp,prs,num,gen,aux,tran,sem,lex,forma
1,0,-1,0,1,510,8,-1,-1,3,1,2,-1,-1,-1,lei,lei
1,0,-1,0,1,511,4,-1,-1,3,1,2,-1,-1,-1,lei,lei
1,1,1,1,2,100,8525,1,1,3,1,0,3,3,0,suonare,suona
1,1,1,1,2,100,8525,7,1,2,1,0,3,3,0,suonare,suona
1,2,-1,2,3,613,1,-1,-1,-1,1,1,-1,-1,-1,il,il
1,3,-1,3,4,200,43878,-1,-1,-1,1,1,-1,-1,0,pianoforte,piano
1,3,-1,3,4,400,43873,-1,-1,-1,1,1,-1,-1,0,piano,piano
1,3,-1,3,4,200,43873,-1,-1,-1,1,1,-1,-1,0,piano,piano
1,3,1,3,4,100,4923,1,1,3,2,0,2,1,0,piare,piano
1,3,1,3,4,100,4915,1,1,1,1,0,2,2,0,pianare,piano
1,4,-1,4,5,711,1,-1,-1,-1,-1,-1,-1,-1,1,e,e
1,5,-1,5,6,510,5,-1,-1,3,1,1,-1,-1,-1,lui,lui
1,5,-1,5,6,511,3,-1,-1,3,1,1,-1,-1,-1,lui,lui
1,6,-1,6,7,200,32426,-1,-1,-1,3,1,-1,-1,0,la,la
1,6,-1,6,7,613,2,-1,-1,-1,1,2,-1,-1,-1,la,la
1,6,-1,6,7,512,10,-1,-1,3,1,2,-1,-1,-1,la,la
1,7,-1,7,-1,200,61345,-1,-1,-1,1,2,-1,-1,0,tromba,tromba
1,7,1,7,-1,100,8919,1,1,3,1,0,2,3,0,trombare,tromba
1,7,1,7,-1,100,8919,7,1,2,1,0,2,3,0,trombare,tromba

### Convert a word in number
> (strnum "quarantadue")

42

### Convert a number in word
> (strnum 42)

"quarantadue"

### Analyze a verb
> (get-verbo "programmo")

((:CAT 100 :ID 5157 :LEX "programmare" :MDV 1 :TMP 1 :PRS 1 :NUM 1 :GEN 0
  :FORMA "programmo" :TRAN 2 :AUX 2 :SEM 0))

### Analyze a compound form verb+clt
> (an-verbo "chiamala")

((:V
  ((:CAT 100 :ID 1382 :LEX "chiamare" :MDV 7 :TMP 1 :PRS 2 :NUM 1 :GEN 0 :FORMA
    "chiama" :TRAN 2 :AUX 2 :SEM 0))
  :P
  ((:FORMA "la" :LEX "la" :CAT 512 :ID 10 :PRS 3 :NUM 1 :GEN 2 :FSC (1 2 3)))))

### Analyze nouns/adjs/advs
> (get-lemma "infermiere")

((:CAT 490 :ID 29930 :LEX "infermiere" :NUM 1 :GEN 1 :FORMA "infermiere" :SEM 0)
 (:CAT 490 :ID 29930 :LEX "infermiere" :NUM 2 :GEN 2 :FORMA "infermiere" :SEM 0)
 (:CAT 200 :ID 29930 :LEX "infermiere" :NUM 1 :GEN 1 :FORMA "infermiere" :SEM 0)
 (:CAT 200 :ID 29930 :LEX "infermiere" :NUM 2 :GEN 2 :FORMA "infermiere" :SEM 0))

### Coniugate a known lemma (e.g. 29930 = infermiere)
> (gen-lemma 29930 200 1 2)

((:CAT 200 :NUM 2 :GEN 1 :FORMA "infermieri" :SEM 0))


### Analyze functional words
> (an-clclass "della")

((:FORMA "della" :LEX "della" :CAT 612 :ID 2 :NUM 1 :GEN 2 :FSC (1 3)
  :NEW-LABEL ("ART" "PART"))
 (:FORMA "della" :LEX "della" :LEX-COMPOSITE "di+la" :CAT 810 :ID 2 :CAT1 800
  :ID1 1 :CAT2 613 :ID2 2))


### Create all conjugated variants of a particular verb (if verb doesn't exist it ask some information to user)
> (mconiuga "amare")

("amo" ((:lex "amare" :cat VERB :mdv IND :tmp PRESENT :prs 1 :num SING :gen _ :gcase _ :dgr _)))
("ami" ((:lex "amare" :cat VERB :mdv IND :tmp PRESENT :prs 2 :num SING :gen _ :gcase _ :dgr _)))
("ama" ((:lex "amare" :cat VERB :mdv IND :tmp PRESENT :prs 3 :num SING :gen _ :gcase _ :dgr _)))
("amiamo" ((:lex "amare" :cat VERB :mdv IND :tmp PRESENT :prs 1 :num PL :gen _ :gcase _ :dgr _)))
("amate" ((:lex "amare" :cat VERB :mdv IND :tmp PRESENT :prs 2 :num PL :gen _ :gcase _ :dgr _)))
("amano" ((:lex "amare" :cat VERB :mdv IND :tmp PRESENT :prs 3 :num PL :gen _ :gcase _ :dgr _)))
("aman" ((:lex "amare" :cat VERB :mdv IND :tmp PRESENT :prs 3 :num PL :gen _ :gcase _ :dgr _)))
("amerò" ((:lex "amare" :cat VERB :mdv IND :tmp FUTURE :prs 1 :num SING :gen _ :gcase _ :dgr _)))
("amerai" ((:lex "amare" :cat VERB :mdv IND :tmp FUTURE :prs 2 :num SING :gen _ :gcase _ :dgr _)))
("amerà" ((:lex "amare" :cat VERB :mdv IND :tmp FUTURE :prs 3 :num SING :gen _ :gcase _ :dgr _)))
("ameremo" ((:lex "amare" :cat VERB :mdv IND :tmp FUTURE :prs 1 :num PL :gen _ :gcase _ :dgr _)))
("amerete" ((:lex "amare" :cat VERB :mdv IND :tmp FUTURE :prs 2 :num PL :gen _ :gcase _ :dgr _)))
("ameranno" ((:lex "amare" :cat VERB :mdv IND :tmp FUTURE :prs 3 :num PL :gen _ :gcase _ :dgr _)))
("ameran" ((:lex "amare" :cat VERB :mdv IND :tmp FUTURE :prs 3 :num PL :gen _ :gcase _ :dgr _)))
("amavo" ((:lex "amare" :cat VERB :mdv IND :tmp IMPERFECT :prs 1 :num SING :gen _ :gcase _ :dgr _)))
...

### Analyzer "loop"
> (lege-console)

## Reference
Christen D., Grammatica computazionale della lingua italiana: un modello. Rapporto sul progetto di ricerca (1990-1999)

## License

This program is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.

This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.

You should have received a copy of the GNU General Public License along with this program. If not, see  [http://www.gnu.org/licenses/](http://www.gnu.org/licenses/).