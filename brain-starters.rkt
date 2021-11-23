;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname brain-starters) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/universe)
(require 2htdp/image)
(require 2htdp/batch-io)


(define BACKGROUND (rectangle 400 200 "solid" "aqua"))
(define HEADER (text "Welcome to Brain Starters!" 30 "indigo"))
(define SUB-HEADER (text "Chose a game to play." 23 "indigo"))
(define ENGLISH (overlay (text "English (E)" 35 "red")
                         (rectangle 175 50 "solid" "black")))
(define MATH (overlay (text "Math (M)" 35 "blue")
                      (rectangle 175 50 "solid" "orange")))
(define BG-1 (place-image HEADER 200 30 BACKGROUND))
(define BG-2 (place-image SUB-HEADER 200 60 BG-1))
(define BG-3 (place-image ENGLISH 95 120 BG-2))
(define BG-FINAL (place-image MATH 305 120 BG-3))



(define-struct english [s1 s2 input status])
; An English is a (make-english 1String 1String String String)
; Interpretation: A structure containing the starting and ender letters
; of the word the user must input, and their input as a string alongside
; any additional status of the game.

(define ENGLISH-EX-1 (make-english "d" "e" "" ""))
(define ENGLISH-EX-2 (make-english "c" "f" "wefewgwengewj" "This doesn't work!"))

(define (english-temp english)
  (...
   ... (english-s1 english) ...
   ... (english-s2 english) ...
   ... (english-input english) ...
   ... (english-status english) ...))


(define-struct math [n1 n2 op input status])
; A Math is a (make-math Number Number [Number Number -> Number] String String)
; Interpretation: A structure containing the numbers and operation the user needs to use to compute
; a mathematical expression, and their input as a string alongside any additional status of the game

(define MATH-EX-1 (make-math 5 3 + "8" ""))
(define MATH-EX-2 (make-math 51 17 / "" "Wrong!"))

(define (math-temp math)
  (...
   ... (math-n1 math) ...
   ... (math-n2 math) ...
   (math-op math ...) ...
   ... (math-input math) ...))

; A Game is one of:
; -Math
; -English
; -Number
; Interpretation: The current status of the game the user is playing, with 0 being the menu

(define GAME-1 MATH-EX-1)
(define GAME-2 0)

(define (game-temp game)
  (...
   (cond
     [(math? game)
      ... (math-temp game) ...]
     [(english? game)
      ... (english-temp game) ...]
     [(number? game)
      ... game ...])))


; dictionary-word? : String -> Boolean
; Checks if the word is a dictionary word

; no check-expect in case the user does not have "words.txt"
; the code works though I promise ;)

(define (dictionary-word? s)
  (ormap (λ (word) (string=? s word)) (read-lines "words.txt")))

; valid-structure? : English -> Boolean
; Determines if the entry meets the criteria of having the first and last letter

(check-expect (valid-structure? ENGLISH-EX-1) #false)
(check-expect (valid-structure? (make-english "c" "t" "cat" "")) #true)

(define (valid-structure? e)
  (and
   (not (string=? "" (english-input e)))
   (string=? (english-s1 e) (first (explode (string-downcase (english-input e)))))
   (string=? (english-s2 e) (first (reverse (explode (string-downcase (english-input e))))))))


; does-it-work? 1String 1String -> Boolean
; Checks if the first or last characters requirement works with at least 20 words

#|
Tests are commented out in case the user does not have "words.txt"
(check-expect (does-it-work? "q" "b") #false)
(check-expect (does-it-work? "q" "e") #true)
|#

(define (does-it-work? s1 s2)
  (> (length (filter (λ (word) (and (string=? s1 (first (explode word)))
                                    (string=? s2 (first (reverse (explode word))))))
                     (read-lines "words.txt")))
     20))

; random-first-letter : Number -> 1String
; Picks a random letter based on a numerical input, which
; is supposed to be random.

(check-expect (random-letter 1) "a")
(check-expect (random-letter 20) "t")

(define (random-letter n)
  (substring (substring "abcdefghijklmnopqrstuvwxyz" 0 n)
             (- (string-length (substring "abcdefghijklmnopqrstuvwxyz" 0 n)) 1)))



; make-new-english : String -> English
; Makes a new English structure with the input being the status

; no tests due to random variables

(define (make-new-english status)
  (local [(define S1 (random-letter (add1 (random 26))))
          (define S2 (random-letter (add1 (random 26))))]
    (if (does-it-work? S1 S2)
        (make-english S1 S2 "" status)
        (make-new-english status))))



; english-key-pressed : English Key -> English
; Modifies the game based on the key that is pressed

; no tests due to file 'words.txt'

(define (english-key-pressed e ke)
  (if (file-exists? "words.txt")
      (cond
        [(string=? ke "\b")
         (if (> (string-length (english-input e)) 0)
             (make-english
              (english-s1 e)
              (english-s2 e)
              (substring (english-input e) 0 (- (string-length (english-input e)) 1))
              "") e)]
        [(string=? ke "\r")
         (if (and (dictionary-word? (string-downcase (english-input e))) (valid-structure? e))
             (make-new-english "Correct!")
             (make-english
              (english-s1 e)
              (english-s2 e)
              (english-input e)
              "This doesn't work!"))]
        [(string=? ke "\t") 0]
        [(or (string=? ke "\n") (string=? ke "shift") (string=? ke "rshift")) e]
        [else (make-english
               (english-s1 e)
               (english-s2 e)
               (string-append (english-input e) ke)
               "")])
      (if (string=? ke "\t") 0 e)))


; english->image : English -> Image
; Converts an English structure into an image

(define (english->image e)
  (local [(define PROBLEM (text (string-append "What word starts with\n'"
                                               (english-s1 e)
                                               "' and ends with '"
                                               (english-s2 e)
                                               "' ?")
                                30 "indigo"))]
    (if (file-exists? "words.txt")
        (place-image (text (english-status e) 30 (status->color (english-status e))) 200 160
                 (place-image
                  (text (english-input e) 30 "indigo") 200 110
                  (place-image PROBLEM 200 50 (rectangle 400 200 "solid" "aqua"))))
        (place-image (text "To play this game, please\ninstall the 'words.txt' file." 30 "red")
                     200 100
                     (rectangle 400 200 "solid" "aqua")))))


; random-operator : Number (between 0-3) -> [Number Number -> Number]
; Picks an numberical operator when given a random number

(check-expect ((random-operator 0) 1 2) 3)
(check-expect ((random-operator 1) 2 1) 1)
(check-expect ((random-operator 2) 3 3) 9)
(check-expect ((random-operator 3) 6 2) 3)

(define (random-operator num)
  (cond
    [(= num 0) +]
    [(= num 1) -]
    [(= num 2) *]
    [(= num 3) /]))

; math-correct? : Math -> Boolean
; Checks if the user's input matches the answer

(check-expect (math-correct? MATH-EX-1) #true)
(check-expect (math-correct? MATH-EX-2) #false)

(define (math-correct? math)
  (and
   (not (= (string-length (math-input math)) 0))
   (=
    ((math-op math) (math-n1 math) (math-n2 math))
    (string->number (math-input math)))))

; create-new-math: String -> Math
; Creates a new randomized math structure with the input as the status

(check-expect (math? (create-new-math "Correct!")) #true)

(define (create-new-math status)
  (local [(define OPERATOR (random-operator (random 4)))]
    (cond
      [(= (OPERATOR 3 2) 5)
       (make-math (random 150)
                  (random 150)
                  OPERATOR
                  ""
                  status)]
      [(= (OPERATOR 3 2) 1)
       (local [(define N1 (random 150))
               (define N2 (random 150))]
         (make-math (if (> N1 N2) N1 N2)
                    (if (> N1 N2) N2 N1)
                    OPERATOR
                    ""
                    status))]
      [(= (OPERATOR 3 2) 6)
       (make-math (random 75)
                  (random 10)
                  OPERATOR
                  ""
                  status)]
      [(= (OPERATOR 3 2) 1.5)
       (local [(define N2 (add1 (random 40)))]
         (make-math (* N2 (random 10))
                    N2
                    OPERATOR
                    ""
                    status))])))


; is-a-number? : String -> Boolean
; Determines if the given string (of length one) is a number

(check-expect (is-a-number? "5") #true)
(check-expect (is-a-number? "f") #false)

(define (is-a-number? s)
  (ormap (λ (ss) (string=? ss s)) (explode "0123456789")))

; math-key-pressed : Math KeyEvent -> Math
; Updates the math game based on the key that the user presses

(check-expect (math? (math-key-pressed MATH-EX-1 "\r")) #true)

(define (math-key-pressed m k)
  (cond
    [(and (string=? k "\b") (> (string-length (math-input m)) 0))
     (make-math (math-n1 m)
                (math-n2 m)
                (math-op m)
                (substring (math-input m) 0 (- (string-length (math-input m)) 1))
                "")]
    [(string=? k "\r")
     (if (math-correct? m)
         (create-new-math "Correct!")
         (make-math (math-n1 m)
                    (math-n2 m)
                    (math-op m)
                    (math-input m)
                    "Wrong!"))]
    [(string=? k "\t")
     0]
    [(is-a-number? k)
     (make-math (math-n1 m)
                (math-n2 m)
                (math-op m)
                (string-append (math-input m) k)
                "")]
    [else (make-math (math-n1 m)
                     (math-n2 m)
                     (math-op m)
                     (math-input m)
                     "")]))

; mathop->string : [Number Number -> Number] -> String
; Converts a mathematical operator to a string

(check-expect (mathop->string +) "+")
(check-expect (mathop->string -) "-")
(check-expect (mathop->string *) "x")
(check-expect (mathop->string /) "÷")

(define (mathop->string f)
  (cond
    [(= (f 3 2) 5) "+"]
    [(= (f 3 2) 1) "-"]
    [(= (f 3 2) 6) "x"]
    [(= (f 3 2) 1.5) "÷"]))

; status->color : String -> String
; Converts the status in a math game to a color for the text

(check-expect (status->color "Correct!") "green")
(check-expect (status->color "BAD") "indigo")

(define (status->color str)
  (cond
    [(string=? str "Correct!") "green"]
    [(string=? str "Wrong!") "red"]
    [(string=? str "This doesn't work!") "red"]
    [else "indigo"]))


; math->image : Math -> Image
; Creates an image when given a math structure

(check-expect (image? (math->image MATH-EX-1)) #true)

(define (math->image m)
  (local [(define PROBLEM (text (string-append "Solve:    "
                                               (number->string (math-n1 m))
                                               "    "
                                               (mathop->string (math-op m))
                                               "    "
                                               (number->string (math-n2 m)))
                                30 "indigo"))]
    (place-image (text (math-status m) 30 (status->color (math-status m))) 200 150
                 (place-image (text (math-input m) 30 "indigo") 200 100
                              (place-image PROBLEM 200 50 (rectangle 400 200 "solid" "aqua"))))))

; global-key-pressed : Game KeyEvent -> Game     
; Updates the game status based on the key pressed

(check-expect (math? (global-key-pressed 0 "m")) #true)
(check-expect (math? (global-key-pressed 0 "f")) #false)

(define (global-key-pressed g ke)
  (cond
    [(math? g) (math-key-pressed g ke)]
    [(english? g) (english-key-pressed g ke)]
    [(string=? (string-downcase ke) "m")
     (create-new-math "")]
    [(string=? (string-downcase ke) "e")
     (if (file-exists? "words.txt")
         (make-new-english "")
         (make-english "a" "a" "" ""))]
    [else g]))

; global-draw : Game -> Image

(check-expect (global-draw MATH-EX-1) (math->image MATH-EX-1))

(define (global-draw g)
  (cond
    [(math? g) (math->image g)]
    [(english? g) (english->image g)]
    [else BG-FINAL]))

    
; play : Game -> World
; Plays the game in big bang

(define (play game)
  (big-bang
      game
    (to-draw global-draw)
    (on-key global-key-pressed)))

(play 0)