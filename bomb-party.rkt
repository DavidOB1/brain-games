;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname bomb-party) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))

; BombParty is a webgame playable on HTTPS://JKLM.FUN
; This version of the game was inspired by them and was not my original idea
; !!!This game will crash if the words.txt file is not installed!!!

(require 2htdp/universe)
(require 2htdp/image)
(require 2htdp/batch-io)

(define BACKGROUND (overlay (rectangle 390 190 "solid" "gray")
                            (rectangle 400 200 "solid" "black")))
(define DICT-FILE "words.txt")
(define SEQ
  (list "aba" "abb" "abi" "abl" "abo" "aca" "acc" "ace" "ach" "aci" "ack" "act" "ada" "add" "ade" "adi" "ado" "ads" "aff" "aft" "age" "agg" "agi" "ago" "ahs" "ail" "ain" "air" "ais" "ake"
        "aki" "ala" "ale" "ali" "alk" "all" "alm" "alo" "als" "alt" "ama" "amb" "ame" "ami" "amm" "amo" "amp" "ams" "ana" "anc" "and" "ane" "ang" "ani" "ank" "ann" "ano" "ans" "ant" "ape"
        "aph" "api" "apo" "app" "aps" "apt" "ara" "arb" "arc" "ard" "are" "arg" "ari" "ark" "arl" "arm" "arn" "aro" "arp" "arr" "ars" "art" "ary" "ase" "ash" "asi" "ass" "ast" "ata" "atc"
        "ate" "ath" "ati" "ato" "ats" "att" "aul" "aun" "aut" "ava" "ave" "avi" "avo" "awa" "awn" "axi" "aye" "ays" "aze" "bac" "bag" "bal" "ban" "bar" "bas" "bat" "bbe" "bbi" "bbl" "bea"
        "bed" "bel" "ben" "ber" "bes" "bet" "bie" "bil" "bin" "bir" "bit" "bla" "ble" "bli" "blo" "blu" "bly" "boa" "bol" "bon" "boo" "bor" "bos" "bou" "bra" "bre" "bri" "bro" "bru" "bul"
        "bur" "bus" "but" "cab" "cal" "cam" "can" "cap" "car" "cas" "cat" "ced" "cel" "cen" "cer" "ces" "cha" "che" "chi" "cho" "chu" "cid" "cin" "cis" "cit" "cke" "cki" "ckl" "cks" "cla"
        "cle" "cli" "clo" "coa" "coc" "cod" "coi" "col" "com" "con" "coo" "cop" "cor" "cos" "cot" "cou" "cra" "cre" "cri" "cro" "cru" "cti" "cto" "cts" "cul" "cur" "cus" "cut" "dal" "dam"
        "dan" "dar" "dat" "dde" "ddi" "ddl" "dea" "dec" "ded" "def" "del" "dem" "den" "der" "des" "dge" "dia" "dic" "die" "din" "dis" "dit" "dle" "dli" "dly" "dog" "dom" "don" "dor" "dos"
        "dou" "dow" "dra" "dre" "dri" "dro" "duc" "eac" "ead" "eak" "eal" "eam" "ean" "ear" "eas" "eat" "eba" "ebo" "eca" "ech" "eci" "eck" "eco" "ect" "eda" "ede" "edi" "eds" "eed" "eek"
        "eel" "een" "eep" "eer" "ees" "eet" "efi" "efu" "ega" "ein" "eis" "ela" "eld" "ele" "eli" "ell" "elo" "els" "elt" "ely" "ema" "emb" "eme" "emi" "emo" "emp" "ena" "enc" "end" "ene"
        "eni" "enn" "eno" "ens" "ent" "epa" "epe" "epi" "epo" "ept" "era" "erb" "erc" "ere" "erg" "eri" "erl" "erm" "ern" "ero" "err" "ers" "ert" "erv" "ery" "esc" "ese" "esh" "esi" "esp"
        "ess" "est" "eta" "ete" "eth" "eti" "eto" "etr" "ets" "ett" "eve" "evi" "ewa" "ewe" "exe" "eye" "eys" "fac" "far" "fed" "fer" "ffe" "ffi" "ffl" "ffs" "fie" "fil" "fin" "fir" "fis"
        "fla" "fle" "fli" "flo" "flu" "fol" "foo" "for" "fra" "fre" "fri" "fro" "ful" "fus" "gal" "gam" "gan" "gar" "gas" "gat" "ged" "gel" "gen" "ger" "ges" "gge" "ggi" "ggl" "ght" "gie"
        "gin" "gla" "gle" "gli" "glo" "gly" "gon" "gra" "gre" "gri" "gro" "gue" "gul" "hal" "ham" "han" "hap" "har" "has" "hat" "hea" "hed" "hee" "hel" "hem" "hen" "her" "hes" "hic" "hie"
        "hil" "hin" "hip" "hir" "his" "hit" "hol" "hom" "hon" "hoo" "hor" "hos" "hot" "hou" "how" "hro" "hum" "ial" "ian" "ias" "iat" "ibb" "ibl" "ica" "ice" "ich" "ici" "ick" "ico" "ics"
        "ict" "ida" "idd" "ide" "idi" "ids" "ied" "ien" "ier" "ies" "iff" "ifi" "ift" "ify" "iga" "igg" "igh" "ign" "ike" "ila" "ild" "ile" "ili" "ill" "ils" "ilt" "ily" "ima" "imb" "ime"
        "imi" "imm" "imp" "ina" "inc" "ind" "ine" "inf" "ing" "ini" "ink" "inn" "ino" "ins" "int" "ion" "ipe" "ipp" "ips" "ira" "ird" "ire" "iri" "irs" "irt" "isa" "isc" "ise" "ish" "isi"
        "ism" "iso" "iss" "ist" "ita" "itc" "ite" "ith" "iti" "ito" "its" "itt" "ity" "ium" "ive" "ivi" "ize" "izz" "ked" "ken" "ker" "kes" "ket" "key" "kie" "kil" "kin" "kle" "lab" "lac"
        "lad" "lag" "lai" "lam" "lan" "lap" "lar" "las" "lat" "lay" "lde" "lea" "lec" "led" "lee" "leg" "len" "ler" "les" "let" "lia" "lic" "lid" "lie" "lif" "lig" "lik" "lim" "lin" "lip"
        "lis" "lit" "lla" "lle" "lli" "llo" "lls" "lly" "loa" "loc" "log" "lon" "loo" "lop" "lor" "los" "lot" "lou" "low" "lte" "lti" "lue" "lum" "lun" "lus" "lut" "lve" "mac" "mag" "mal"
        "man" "mar" "mas" "mat" "mba" "mbe" "mbl" "mbo" "mea" "med" "mel" "men" "mer" "mes" "met" "mia" "mic" "mid" "mie" "mil" "min" "mis" "mit" "mme" "mmi" "mol" "mon" "moo" "mor" "mos"
        "mot" "mou" "mpa" "mpe" "mpi" "mpl" "mul" "mus" "nag" "nal" "nan" "nap" "nar" "nas" "nat" "nce" "nch" "nci" "nco" "nda" "nde" "ndi" "ndl" "ndo" "nds" "nea" "ned" "nee" "nel" "ner"
        "nes" "net" "nga" "nge" "ngi" "ngl" "ngs" "nia" "nic" "nie" "nin" "nis" "nit" "nke" "nki" "nks" "nna" "nne" "nni" "non" "nor" "nos" "not" "nse" "nta" "nte" "nth" "nti" "nto" "ntr"
        "nts" "oad" "oar" "oat" "obb" "obe" "oca" "och" "ock" "ode" "odi" "ods" "oes" "off" "oge" "ogg" "oid" "oil" "oin" "ois" "oke" "ola" "old" "ole" "oli" "oll" "olo" "ols" "oma" "omb"
        "ome" "omi" "omp" "oms" "ona" "ond" "one" "ong" "oni" "ono" "ons" "ont" "ony" "ood" "oof" "ook" "ool" "oom" "oon" "oop" "oos" "oot" "ope" "opi" "opp" "ops" "ora" "ord" "ore" "org"
        "ori" "ork" "orm" "orn" "oro" "ors" "ort" "ose" "osi" "oss" "ost" "ota" "ote" "oth" "oti" "oto" "ots" "ott" "oug" "oul" "oun" "our" "ous" "out" "ove" "owe" "owl" "own" "ows" "oxi"
        "pac" "pal" "pan" "par" "pas" "pat" "pea" "pec" "ped" "pee" "pel" "pen" "per" "pes" "pet" "phe" "phi" "pho" "pic" "pie" "pil" "pin" "pir" "pis" "pit" "pla" "ple" "pli" "plo" "plu"
        "pod" "pol" "pon" "poo" "por" "pos" "pot" "pou" "ppe" "ppi" "pra" "pre" "pri" "pro" "pti" "pul" "pun" "pur" "put" "qua" "que" "qui" "rab" "rac" "rad" "rag" "rai" "ral" "ram" "ran"
        "rap" "ras" "rat" "rav" "raw" "ray" "rce" "rch" "rde" "rdi" "rds" "rea" "reb" "rec" "red" "ree" "ref" "reg" "rei" "rel" "rem" "ren" "rep" "rer" "res" "ret" "rev" "rew" "rge" "ria"
        "rib" "ric" "rid" "rie" "rif" "rig" "ril" "rim" "rin" "rio" "rip" "ris" "rit" "riv" "rke" "rle" "rli" "rme" "rmi" "rne" "rni" "rns" "roa" "rob" "roc" "roi" "rol" "rom" "ron" "roo"
        "rop" "ros" "rot" "rou" "row" "rra" "rre" "rri" "rro" "rse" "rte" "rti" "rts" "rum" "run" "rus" "rve" "sal" "san" "sar" "sca" "sch" "sco" "scr" "scu" "sea" "sec" "sed" "see" "sel"
        "sen" "ser" "ses" "set" "sha" "she" "shi" "sho" "sic" "sid" "sie" "sig" "sil" "sin" "sis" "sit" "ske" "ski" "sla" "sle" "sli" "sma" "sme" "sms" "sna" "sno" "sol" "som" "son" "sor"
        "sou" "spa" "spe" "spi" "spo" "spr" "squ" "ssa" "sse" "ssi" "sta" "ste" "sti" "stl" "sto" "str" "sts" "stu" "sty" "sub" "sul" "sun" "sur" "swa" "swi" "tab" "tac" "tag" "tai" "tal"
        "tam" "tan" "tar" "tas" "tat" "tch" "tea" "ted" "tee" "tel" "tem" "ten" "ter" "tes" "tha" "the" "thi" "tho" "thr" "ths" "tia" "tic" "tie" "til" "tim" "tin" "tio" "tis" "tit" "tiv"
        "tle" "tli" "tly" "tol" "tom" "ton" "too" "top" "tor" "tos" "tou" "tra" "tre" "tri" "tro" "tru" "try" "tta" "tte" "tti" "ttl" "tto" "tum" "tur" "tus" "twi" "ual" "ubb" "uce" "uch"
        "uck" "udd" "ude" "udg" "ues" "uff" "ugg" "ugh" "uin" "uit" "ula" "ule" "uli" "ull" "uls" "ult" "umb" "ume" "umm" "ump" "ums" "una" "unb" "unc" "und" "une" "ung" "uni" "unk" "unn"
        "uns" "unt" "upe" "ups" "ura" "ure" "urg" "uri" "url" "urn" "urr" "urs" "urt" "use" "ush" "usi" "uss" "ust" "ute" "uth" "uti" "uts" "utt" "val" "var" "ved" "vel" "ven" "ver" "ves"
        "vie" "vil" "vin" "vis" "wal" "wan" "war" "way" "wea" "wed" "wee" "wel" "wer" "whe" "whi" "win" "wis" "woo" "wor" "xes" "yed" "yer" "yin" "zed" "zer" "zes" "zin" ))


(define-struct bp [seq input status])
; A BombParty (BP) is a (make-bp String String Number[0:3])
; Interepreation: A structure containing the sequence that must be used, the
; user's input, and the status of the game, represented as a number.

(define BP-1 (make-bp "ati" "ratio" 0))
(define BP-2 (make-bp "all" "" 0))
(define BP-3 (make-bp "nce" "" 1))

(define (bp-temp bp)
  (...
   ... (bp-seq bp) ...
   ... (bp-input bp) ...
   ... (bp-status bp) ...))


; Some of the functions below were copied from my other game called "brain-starters"
; They came from that game originally


; random-letter : Number -> 1String
; Picks a random letter based on a numerical input, which
; is supposed to be random.

(check-expect (random-letter 1) "a")
(check-expect (random-letter 20) "t")

(define (random-letter n)
  (substring (substring "abcdefghijklmnopqrstuvwxyz" 0 n)
             (- (string-length (substring "abcdefghijklmnopqrstuvwxyz" 0 n)) 1)))



; good-seq? : String -> Boolean (function not called during the game)
; Determines if the given sequence is good for the game

(define (good-seq? str)
  (> (length
      (filter (λ (word) (string-contains? str word)) (read-lines DICT-FILE))) 73))

; get-seq : Number Number List -> List
; Generates a sequence that works for the game
; This was used to generate the list needed for the game
; and won't be called during the game due to how long it takes
; to run this

(define (get-seq n1 n2 list)
  (local [(define SEQ (string-append
                       (random-letter n1)
                       (random-letter n2)))
          (define NEW-LIST
            (if (good-seq? SEQ)
                (cons SEQ list) list))]
    (if (= n2 26)
        (if (= n1 26)
            NEW-LIST
            (get-seq (add1 n1) 1 NEW-LIST))
        (get-seq n1 (add1 n2) NEW-LIST))))


; make-new-game: Number -> BP
; Makes a new BP, with the number input being the status

(define (make-new-game n)
  (make-bp
   (list-ref SEQ (random 1137))
   ""
   n))


; status->text: Number -> Image
; Converts BP status to a text image

(check-expect (status->text 1) (text "Correct!" 50 "green"))
(check-expect (status->text (sub1 1)) (circle 0 "solid" "blue"))

(define (status->text n)
  (cond
    [(= n 0) empty-image]
    [(= n 1) (text "Correct!" 50 "green")]
    [(= n 2) (text "That doesn't work!" 35 "red")]
    [(= n 3) (text "Skipped" 50 "white")]))


; is-letter? : 1String -> Boolean
; Determines if the character is a letter

(check-expect (is-letter? "g") #true)
(check-expect (is-letter? "menu") #false)

(define (is-letter? 1str)
  (ormap (λ (s) (string=? s (string-downcase 1str))) (explode "qwertyuiopasdfghjklzxcvbnm")))


; enter-reqs? : BP -> Boolean
; Determines if the BP meets the game requirements

(check-expect (enter-reqs? BP-1) #true)
(check-expect (enter-reqs? BP-2) #false)

(define (enter-reqs? bp)
  (and (string-contains? (bp-seq bp) (string-downcase (bp-input bp)))
       (ormap (λ (word) (string=? (string-downcase (bp-input bp)) word)) (read-lines DICT-FILE))))


; bp->image : BP -> Image
; Converts info of the BP game into an image

(check-expect (bp->image BP-1)
              (place-image
               (text "ati" 50 "orange") 200 45
               (place-image
                (text "ratio" 50 "yellow") 200 100 BACKGROUND)))

(define (bp->image bp)
  (place-image
   (text (bp-seq bp) 50 "orange") 200 45
   (place-image
    (text (bp-input bp) 50 "yellow") 200 100
    (place-image (status->text (bp-status bp)) 200 155 BACKGROUND))))


; key-pressed : BP KeyEvent -> BP
; Updates the BP based on the key that is pressed

(check-expect (key-pressed BP-1 "r") (make-bp "ati" "ratior" 0))
(check-random (key-pressed BP-1 "\r") (make-new-game 1))
(check-expect (key-pressed BP-2 "\r") (make-bp "all" "" 2))

(define (key-pressed bp ke)
  (cond
    [(is-letter? ke)
     (make-bp
      (bp-seq bp)
      (string-append (bp-input bp) ke)
      0)]
    [(and (string=? ke "\b") (> (string-length (bp-input bp)) 0))
     (make-bp
      (bp-seq bp)
      (substring (bp-input bp) 0 (- (string-length (bp-input bp)) 1))
      0)]
    [(string=? ke "\t") (make-new-game 3)]
    [(string=? ke "\r")
     (if (enter-reqs? bp)
         (make-new-game 1)
         (make-bp (bp-seq bp) (bp-input bp) 2))]
    [else (make-bp (bp-seq bp) (bp-input bp) 0)]))


; play : BP -> World
; Plays the game in big bang

(define (play bp)
  (big-bang
      bp
    (to-draw bp->image)
    (on-key key-pressed)))


(play (make-new-game 0))