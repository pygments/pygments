#!/usr/bin/env newlisp

;; @module Nlex
;; @author cormullion
;; @description  newLISP source code lexer/tokenizer/parser
;; @location somewhere on github
;; @version 0.1 of 2011-09-19 08:55:19
;;<h4>About this module</h4>
;;<p>The Nlex module is a lexer/tokenizer/parser for newLISP source code.
;; An expert from StackOverflow xplains:
;; A tokenizer breaks a stream of text into tokens.
;; A lexer is basically a tokenizer, but it usually attaches extra context to the tokens.
;; A parser takes the stream of tokens from the lexer and turns it into an abstract syntax tree representing the program represented by the original text.</p>
;;<p><b>Usage</b></p>
;;<p>To tokenize/parse source code stored in symbol 'original, use <b>parse-newlisp</b>, To convert the parsed source tree back to plain source, use <b>nlx-to-plaintext</b>:</p>
;;<pre>
;;(letn ((converted    (Nlex:parse-newlisp     original-source)) ; parses 
;;       (new-original (Nlex:nlx-to-plaintext  converted)))      ; converts back to plain text
;;</pre>
;;<p>After this round trip, original-source and new-original should be identical.</p>
;;<p></p>

(context 'Nlex)

; class variables

(define *cursor*)
(define *source-length*)
(define *source-list*)
(define *depth*)
(define *tree*)
(define *loc*)

(define (get-next-char)
 (let ((nch ""))
   (if (< *cursor* *source-length*)
       (begin
          (set 'nch (*source-list* *cursor*))
          (inc *cursor* (utf8len nch)))
       (set 'nch nil))
   nch))

(define (peek-char)
 (let ((pch ""))
   (if (< *cursor* *source-length*)
       (set 'pch (*source-list* *cursor*))
       (set 'pch nil))))

(define (char-identifier-first? c)
  (not (find (lower-case c) [text] #;"'(){}.0123456789[/text])))
  
(define (char-identifier? c)
  (not (find (lower-case c) { "':,()})))

(define (char-numeric-first? c)
   (find c {123456789+-.0}))

(define (char-numeric? c)
   (find c {0123456789+-.xXabcdefABracketedCommandDEF}))

(define (char-whitespace? c)
  (or (= c " ") (= c "\n") (= c "\t")))

(define (open-paren-token)
  (add-to-parse-tree '(LeftParen "(")))

(define (close-paren-token)
  (add-to-parse-tree '(RightParen ")")))

(define (read-comment c)
  (let ((res c) (ch ""))
     (while (and (!= (set 'ch (get-next-char)) "\n") ch)
        (push ch res -1))
    (add-to-parse-tree (list 'Comment (string res "\n")))))
    
(define (read-identifier c)
  (let ((res c) (ch ""))
   ; look for end of identifier
    (while (and (not (find (set 'ch (peek-char)) " \"',()\n\t\r")) (!= ch nil))
      (push (get-next-char) res -1))
    (add-to-parse-tree (list 'Symbol res))))

(define (read-number-scanner list-so-far)
    (let ((next-char (peek-char)))
      ;; if next-char is a digit then recurse
      (if (and (char-numeric? next-char) next-char)
          (read-number-scanner (cons (get-next-char) list-so-far))
          (reverse list-so-far))))

(define (precise-float str)
; more faithful to original format than newLISP's float?
  (let ((p "") (q ""))
    (map set '(p q) (parse str "."))
    (append p "." q)))
    
(define (scientific-float str)
  (let ((p "") (q ""))
    (map set '(p q) (parse str "e"))
    (append p "e" q)))

(define (read-number c)
  (let ((res '() number-as-string ""))
     (set 'number-as-string (join (read-number-scanner (list c))))
     (cond
       ; try hex first
       ((starts-with (lower-case number-as-string) "0x")
          (set 'res  (list 'Hex number-as-string)))
       ; scientific notation if there's an e
       ((find "e" (lower-case number-as-string))
          (set 'res  (list 'Scientific (scientific-float number-as-string))))
       ; float?
       ((find "." number-as-string)
          ; newLISP's float function isn't quite what we want here     
          (set 'res  (list 'Float (precise-float number-as-string))))
       ; octal, not hex or float? 017 is OK, 019 is read as 10
       ((and (starts-with (lower-case number-as-string) "0") 
             (> (length number-as-string) 1)
             (empty? (difference (explode number-as-string) (explode "01234567"))))
          (set 'res (list 'Octal number-as-string)))
       ; perhaps an integer?  019 is read as 19 ...
       ((integer? (int number-as-string 0 10))
         (set 'res  (list 'Integer (int number-as-string 0 10))))
       ; give up
       (true
         (set 'res (list 'NaN "NaN"))))
  (add-to-parse-tree res)))

(define (read-quote)
   (add-to-parse-tree '(Quote "'")))

(define (read-quoted-string)
  (let ((res {}) (ch {}))
     (while (and (!= (set 'ch (get-next-char)) {"}) ch)
        (push ch res -1)
        ; check for backslashed quotes
        (when (= ch {\}) 
              (set 'ch (get-next-char))
              (push ch res -1)))
    (add-to-parse-tree (list 'QuotedString res))))

(define (read-braced-string)
  (let ((res "") (ch {}) (level 1)) 
  ; we've already seen the first { so we're up to level 1
     (while (> level 0)
         (set 'ch (get-next-char))
         (if (= ch "{") (inc level))
         (if (= ch "}") (dec level))
         (if (or (< level 0) (= ch nil)) (throw-error (string "error in a braced string at character " *cursor*)))
         ; don't push final "}"
         (if (and (> level 0)) (push ch res -1)))
    (add-to-parse-tree (list 'BracedString res))))

(define (read-bracketed-string ch)
  (let ((res "") (ch {}))  
    (cond
     ; bracketed TEXT?
     ((= (lower-case (join (slice *source-list* (- *cursor* 1) 6))) "[text]")
         ; look for final [/text]
         (inc *cursor* 5)
         ; look for end
         (while (and  (< *cursor* (- *source-length* 7)) 
                      (!= (lower-case (join (*cursor* 7 *source-list*))) "[/text]")
                      ch)
                (push (get-next-char) res -1))
         (inc *cursor* 7)
         (add-to-parse-tree (list 'BracketedText res)))
     ; bracketed CMD?
     ((= (lower-case (join (slice *source-list* (- *cursor* 1) 5))) "[cmd]")
         ; look for final [/cmd]
         (inc *cursor* 4)
         (while (and  (< *cursor* (- *source-length* 6)) 
                      (!= (lower-case (join (*cursor* 6 *source-list*))) "[/cmd]")
                      ch)
                (push (get-next-char) res -1))
         (inc *cursor* 6)
         (add-to-parse-tree (list 'BracketedCommand res)))
     ; must be those weird bracketed identifiers    
     (true
         (while (and (!= (set 'ch (get-next-char)) {]}) ch)
            (push ch res -1)
            ; check for backslashed quotes
            (when (= ch {\}) 
                  (set 'ch (get-next-char))
                  (push ch res -1)))
            (add-to-parse-tree (list 'BracketedIdentifier res))))))

(define (read-whitespace ch)
  (let ((res ch))
     (while (find (set 'ch (peek-char)) " \n\t")
        (push (get-next-char) res -1))
    (add-to-parse-tree (list 'WhiteSpace (base64-enc res)))))

(define (get-token)
 (let ((first-char (get-next-char)))
    (if first-char
      (cond 
            ; a - or + could be the start of a symbol or a number, so look at the next char
            ((or (= first-char "-") (= first-char "+"))
                (if (find (peek-char) "1234567890")
                    (read-number first-char)
                    (read-identifier first-char)))
            ((char-whitespace? first-char)   
               (read-whitespace first-char))
            ((= first-char {(})
               (open-paren-token))
            ((= first-char {)})
               (close-paren-token))
            ((= first-char {#})
               (read-comment first-char))
            ((= first-char {;})
               (read-comment first-char))
            ((= first-char {"})
               (read-quoted-string))
            ((= first-char "{")
               (read-braced-string))
            ((= first-char "[")
               (read-bracketed-string first-char))
            ((= first-char {'})
               (read-quote))
            ((char-numeric-first? first-char)
               (read-number first-char))
            ((char-identifier-first? first-char)
               (read-identifier first-char))
            (true (throw-error (string "{" first-char "} is an unrecognized token")))))))

(define (add-to-parse-tree token-pair)
  (let (token (first token-pair))
  (cond 
    ((= token 'LeftParen)
        (inc *depth*)
        (push '((LeftParen "(")) *tree* *loc*)
        (push -1 *loc*))
    ((= token 'RightParen)
        (push '(RightParen ")") *tree* *loc*)
        (dec *depth*)
        (pop *loc*))
    (true
        (push token-pair *tree* *loc*)
        true))))

(define (parse-newlisp src)
  ; main function: tokenize/lex/parse the string in src
  (set '*depth* 0 
       '*tree* '() 
       '*loc* '(-1) 
       '*cursor* 0 
       '*source-list*   (explode src) 
       '*source-length* (utf8len src)
       '*source-length* (length *source-list*))
  (while (< *cursor* *source-length*)
         (get-token))
  *tree*)

(define (nlx-to-plaintext nlx (depth 0))
   (if (= depth 0) (set 'buff {})) ; if first pass, initialize a buffer
   (dolist (element nlx)
    (set 'token-type (first element) 'token-value (last element))
    (if (atom? token-type)
        (cond 
           ((= token-type 'LeftParen) ; left parenthesis
                (extend buff {(}))
           ((= token-type 'RightParen) ; right parenthesis
                (extend buff {)}))
           ((= token-type 'WhiteSpace) ; whitespace
                (dostring (s (base64-dec token-value)) 
                  (extend buff (string (char s)))))
           ((= token-type 'BracedString) ; braced string
                (extend buff (string  "{" token-value "}")))
           ((= token-type 'QuotedString) ; quoted string
                (extend buff (string  {"} token-value {"})))
           ((= token-type 'BracketedText) ; bracketed  text
                (extend buff (string  {[text]} token-value {[/text]})))        
           ((= token-type 'Quote); quote
                (extend buff (string  "'")))
           ((= token-type 'Comment) ; comment
                (extend buff (string (last element) "\n")))
           ((= token-type 'Integer) ; int
                (extend buff (string (int (last element)))))
           ((= token-type 'Float) ; float
                (extend buff (string (precise-float (last element)))))  
           ((= token-type 'Scientific) ; scientific notation
                (extend buff (scientific-float (last element))))  
           ((= token-type 'BracketedCommand) ; bracketed command
               (extend buff (string {[cmd]} (last element) {[/cmd]})))
           ((or 
                (= token-type 'Symbol) ; close parenthesis
                (= token-type 'Hex) ; hex
                (= token-type 'NaN) ; not a number
                (= token-type 'Octal) ; octal
                )
                (extend buff (string (last element))))
           ((= token-type 'BracketedIdentifier) ; bracketed identifier
                (extend buff (string {[} (last element) {]}))))
        ; not an atom, so recurse but don't initialize buffer
        (nlx-to-plaintext element 1)))
   buff)

;eof
