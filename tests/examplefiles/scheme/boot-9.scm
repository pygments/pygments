;;; installed-scm-file

;;;; Copyright (C) 1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004 Free Software Foundation, Inc.
;;;;
;;;; This program is free software; you can redistribute it and/or modify
;;;; it under the terms of the GNU General Public License as published by
;;;; the Free Software Foundation; either version 2, or (at your option)
;;;; any later version.
;;;;
;;;; This program is distributed in the hope that it will be useful,
;;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;;; GNU General Public License for more details.
;;;;
;;;; You should have received a copy of the GNU General Public License
;;;; along with this software; see the file COPYING.  If not, write to
;;;; the Free Software Foundation, Inc., 59 Temple Place, Suite 330,
;;;; Boston, MA 02111-1307 USA
;;;;
;;;; As a special exception, the Free Software Foundation gives permission
;;;; for additional uses of the text contained in its release of GUILE.
;;;;
;;;; The exception is that, if you link the GUILE library with other files
;;;; to produce an executable, this does not by itself cause the
;;;; resulting executable to be covered by the GNU General Public License.
;;;; Your use of that executable is in no way restricted on account of
;;;; linking the GUILE library code into it.
;;;;
;;;; This exception does not however invalidate any other reasons why
;;;; the executable file might be covered by the GNU General Public License.
;;;;
;;;; This exception applies only to the code released by the
;;;; Free Software Foundation under the name GUILE.  If you copy
;;;; code from other Free Software Foundation releases into a copy of
;;;; GUILE, as the General Public License permits, the exception does
;;;; not apply to the code that you add in this way.  To avoid misleading
;;;; anyone as to the status of such modified files, you must delete
;;;; this exception notice from them.
;;;;
;;;; If you write modifications of your own for GUILE, it is your choice
;;;; whether to permit this exception to apply to your modifications.
;;;; If you do not wish that, delete this exception notice.
;;;;


;;; Commentary:

;;; This file is the first thing loaded into Guile.  It adds many mundane
;;; definitions and a few that are interesting.
;;;
;;; The module system (hence the hierarchical namespace) are defined in this
;;; file.
;;;

;;; Code:


;;; {Deprecation}
;;;

;; We don't have macros here, but we do want to define
;; `begin-deprecated' early.

(define begin-deprecated
  (procedure->memoizing-macro
   (lambda (exp env)
     (if (include-deprecated-features)
	 `(begin ,@(cdr exp))
	 `#f))))


;;; {Features}
;;

(define (provide sym)
  (if (not (memq sym *features*))
      (set! *features* (cons sym *features*))))

;;; Return #t iff FEATURE is available to this Guile interpreter.
;;; In SLIB, provided? also checks to see if the module is available.
;;; We should do that too, but don't.
(define (provided? feature)
  (and (memq feature *features*) #t))

(begin-deprecated
 (define (feature? sym)
   (issue-deprecation-warning
    "`feature?' is deprecated.  Use `provided?' instead.")
   (provided? sym)))

;;; let format alias simple-format until the more complete version is loaded
(define format simple-format)


;;; {R4RS compliance}

(primitive-load-path "ice-9/r4rs.scm")


;;; {Simple Debugging Tools}
;;


;; peek takes any number of arguments, writes them to the
;; current ouput port, and returns the last argument.
;; It is handy to wrap around an expression to look at
;; a value each time is evaluated, e.g.:
;;
;;	(+ 10 (troublesome-fn))
;;	=> (+ 10 (pk 'troublesome-fn-returned (troublesome-fn)))
;;

(define (peek . stuff)
  (newline)
  (display ";;; ")
  (write stuff)
  (newline)
  (car (last-pair stuff)))

(define pk peek)

(define (warn . stuff)
  (with-output-to-port (current-error-port)
    (lambda ()
      (newline)
      (display ";;; WARNING ")
      (display stuff)
      (newline)
      (car (last-pair stuff)))))


;;; {Trivial Functions}
;;;

(define (identity x) x)
(define (1+ n) (+ n 1))
(define (1- n) (+ n -1))
(define (and=> value procedure) (and value (procedure value)))
(define (make-hash-table k) (make-vector k '()))

(begin-deprecated
 (define (id x)
   (issue-deprecation-warning "`id' is deprecated.  Use `identity' instead.")
   (identity x))
 (define (-1+ n)
   (issue-deprecation-warning "`-1+' is deprecated.  Use `1-' instead.")
   (1- n))
 (define (return-it . args)
   (issue-deprecation-warning "`return-it' is deprecated.  Use `noop' instead.")
   (apply noop args)))

;;; apply-to-args is functionally redundant with apply and, worse,
;;; is less general than apply since it only takes two arguments.
;;;
;;; On the other hand, apply-to-args is a syntacticly convenient way to
;;; perform binding in many circumstances when the "let" family of
;;; of forms don't cut it.  E.g.:
;;;
;;;	(apply-to-args (return-3d-mouse-coords)
;;;	  (lambda (x y z)
;;;		...))
;;;

(define (apply-to-args args fn) (apply fn args))



;;; {Integer Math}
;;;

(define (ipow-by-squaring x k acc proc)
  (cond ((zero? k) acc)
	((= 1 k) (proc acc x))
	(else (ipow-by-squaring (proc x x)
				(quotient k 2)
				(if (even? k) acc (proc acc x))
				proc))))

(begin-deprecated
 (define (string-character-length s)
   (issue-deprecation-warning "`string-character-length' is deprecated.  Use `string-length' instead.")
   (string-length s))
 (define (flags . args)
   (issue-deprecation-warning "`flags' is deprecated.  Use `logior' instead.")
   (apply logior args)))


;;; {Symbol Properties}
;;;

(define (symbol-property sym prop)
  (let ((pair (assoc prop (symbol-pref sym))))
    (and pair (cdr pair))))

(define (set-symbol-property! sym prop val)
  (let ((pair (assoc prop (symbol-pref sym))))
    (if pair
	(set-cdr! pair val)
	(symbol-pset! sym (acons prop val (symbol-pref sym))))))

(define (symbol-property-remove! sym prop)
  (let ((pair (assoc prop (symbol-pref sym))))
    (if pair
	(symbol-pset! sym (delq! pair (symbol-pref sym))))))

;;; {General Properties}
;;;

;; This is a more modern interface to properties.  It will replace all
;; other property-like things eventually.

(define (make-object-property)
  (let ((prop (primitive-make-property #f)))
    (make-procedure-with-setter
     (lambda (obj) (primitive-property-ref prop obj))
     (lambda (obj val) (primitive-property-set! prop obj val)))))



;;; {Arrays}
;;;

(if (provided? 'array)
    (primitive-load-path "ice-9/arrays.scm"))


;;; {Keywords}
;;;

(define (symbol->keyword symbol)
  (make-keyword-from-dash-symbol (symbol-append '- symbol)))

(define (keyword->symbol kw)
  (let ((sym (symbol->string (keyword-dash-symbol kw))))
    (string->symbol (substring sym 1 (string-length sym)))))

(define (kw-arg-ref args kw)
  (let ((rem (member kw args)))
    (and rem (pair? (cdr rem)) (cadr rem))))



;;; {Structs}

(define (struct-layout s)
  (struct-ref (struct-vtable s) vtable-index-layout))



;;; Environments

(define the-environment
  (procedure->syntax
   (lambda (x e)
     e)))

(define the-root-environment (the-environment))

(define (environment-module env)
  (let ((closure (and (pair? env) (car (last-pair env)))))
    (and closure (procedure-property closure 'module))))


;;; {Records}
;;;

;; Printing records: by default, records are printed as
;;
;;   #<type-name field1: val1 field2: val2 ...>
;;
;; You can change that by giving a custom printing function to
;; MAKE-RECORD-TYPE (after the list of field symbols).  This function
;; will be called like
;;
;;   (<printer> object port)
;;
;; It should print OBJECT to PORT.

(define (inherit-print-state old-port new-port)
  (if (get-print-state old-port)
      (port-with-print-state new-port (get-print-state old-port))
      new-port))

;; 0: type-name, 1: fields
(define record-type-vtable
  (make-vtable-vtable "prpr" 0
		      (lambda (s p)
			(cond ((eq? s record-type-vtable)
			       (display "#<record-type-vtable>" p))
			      (else
			       (display "#<record-type " p)
			       (display (record-type-name s) p)
			       (display ">" p))))))

(define (record-type? obj)
  (and (struct? obj) (eq? record-type-vtable (struct-vtable obj))))

(define (make-record-type type-name fields . opt)
  (let ((printer-fn (and (pair? opt) (car opt))))
    (let ((struct (make-struct record-type-vtable 0
			       (make-struct-layout
				(apply string-append
				       (map (lambda (f) "pw") fields)))
			       (or printer-fn
				   (lambda (s p)
				     (display "#<" p)
				     (display type-name p)
				     (let loop ((fields fields)
						(off 0))
				       (cond
					((not (null? fields))
					 (display " " p)
					 (display (car fields) p)
					 (display ": " p)
					 (display (struct-ref s off) p)
					 (loop (cdr fields) (+ 1 off)))))
				     (display ">" p)))
			       type-name
			       (copy-tree fields))))
      ;; Temporary solution: Associate a name to the record type descriptor
      ;; so that the object system can create a wrapper class for it.
      (set-struct-vtable-name! struct (if (symbol? type-name)
					  type-name
					  (string->symbol type-name)))
      struct)))

(define (record-type-name obj)
  (if (record-type? obj)
      (struct-ref obj vtable-offset-user)
      (error 'not-a-record-type obj)))

(define (record-type-fields obj)
  (if (record-type? obj)
      (struct-ref obj (+ 1 vtable-offset-user))
      (error 'not-a-record-type obj)))

(define (record-constructor rtd . opt)
  (let ((field-names (if (pair? opt) (car opt) (record-type-fields rtd))))
    (local-eval `(lambda ,field-names
		   (make-struct ',rtd 0 ,@(map (lambda (f)
						 (if (memq f field-names)
						     f
						     #f))
					       (record-type-fields rtd))))
		the-root-environment)))

(define (record-predicate rtd)
  (lambda (obj) (and (struct? obj) (eq? rtd (struct-vtable obj)))))

(define (record-accessor rtd field-name)
  (let* ((pos (list-index (record-type-fields rtd) field-name)))
    (if (not pos)
	(error 'no-such-field field-name))
    (local-eval `(lambda (obj)
		   (and (eq? ',rtd (record-type-descriptor obj))
			(struct-ref obj ,pos)))
		the-root-environment)))

(define (record-modifier rtd field-name)
  (let* ((pos (list-index (record-type-fields rtd) field-name)))
    (if (not pos)
	(error 'no-such-field field-name))
    (local-eval `(lambda (obj val)
		   (and (eq? ',rtd (record-type-descriptor obj))
			(struct-set! obj ,pos val)))
		the-root-environment)))


(define (record? obj)
  (and (struct? obj) (record-type? (struct-vtable obj))))

(define (record-type-descriptor obj)
  (if (struct? obj)
      (struct-vtable obj)
      (error 'not-a-record obj)))

(provide 'record)


;;; {Booleans}
;;;

(define (->bool x) (not (not x)))


;;; {Symbols}
;;;

(define (symbol-append . args)
  (string->symbol (apply string-append (map symbol->string args))))

(define (list->symbol . args)
  (string->symbol (apply list->string args)))

(define (symbol . args)
  (string->symbol (apply string args)))


;;; {Lists}
;;;

(define (list-index l k)
  (let loop ((n 0)
	     (l l))
    (and (not (null? l))
	 (if (eq? (car l) k)
	     n
	     (loop (+ n 1) (cdr l))))))

(define (make-list n . init)
  (if (pair? init) (set! init (car init)))
  (let loop ((answer '())
	     (n n))
    (if (<= n 0)
	answer
	(loop (cons init answer) (- n 1)))))


;;; {and-map and or-map}
;;;
;;; (and-map fn lst) is like (and (fn (car lst)) (fn (cadr lst)) (fn...) ...)
;;; (or-map fn lst) is like (or (fn (car lst)) (fn (cadr lst)) (fn...) ...)
;;;

;; and-map f l
;;
;; Apply f to successive elements of l until exhaustion or f returns #f.
;; If returning early, return #f.  Otherwise, return the last value returned
;; by f.  If f has never been called because l is empty, return #t.
;;
(define (and-map f lst)
  (let loop ((result #t)
	     (l lst))
    (and result
	 (or (and (null? l)
		  result)
	     (loop (f (car l)) (cdr l))))))

;; or-map f l
;;
;; Apply f to successive elements of l until exhaustion or while f returns #f.
;; If returning early, return the return value of f.
;;
(define (or-map f lst)
  (let loop ((result #f)
	     (l lst))
    (or result
	(and (not (null? l))
	     (loop (f (car l)) (cdr l))))))



(if (provided? 'posix)
    (primitive-load-path "ice-9/posix.scm"))

(if (provided? 'socket)
    (primitive-load-path "ice-9/networking.scm"))

(define file-exists?
  (if (provided? 'posix)
      (lambda (str)
	(->bool (false-if-exception (stat str))))
      (lambda (str)
	(let ((port (catch 'system-error (lambda () (open-file str OPEN_READ))
			   (lambda args #f))))
	  (if port (begin (close-port port) #t)
	      #f)))))

(define file-is-directory?
  (if (provided? 'posix)
      (lambda (str)
	(eq? (stat:type (stat str)) 'directory))
      (lambda (str)
	(let ((port (catch 'system-error
			   (lambda () (open-file (string-append str "/.")
						 OPEN_READ))
			   (lambda args #f))))
	  (if port (begin (close-port port) #t)
	      #f)))))

(define (has-suffix? str suffix)
  (let ((sufl (string-length suffix))
	(sl (string-length str)))
    (and (> sl sufl)
	 (string=? (substring str (- sl sufl) sl) suffix))))

(define (system-error-errno args)
  (if (eq? (car args) 'system-error)
      (car (list-ref args 4))
      #f))


;;; {Error Handling}
;;;

(define (error . args)
  (save-stack)
  (if (null? args)
      (scm-error 'misc-error #f "?" #f #f)
      (let loop ((msg "~A")
		 (rest (cdr args)))
	(if (not (null? rest))
	    (loop (string-append msg " ~S")
		  (cdr rest))
	    (scm-error 'misc-error #f msg args #f)))))

;; bad-throw is the hook that is called upon a throw to a an unhandled
;; key (unless the throw has four arguments, in which case
;; it's usually interpreted as an error throw.)
;; If the key has a default handler (a throw-handler-default property),
;; it is applied to the throw.
;;
(define (bad-throw key . args)
  (let ((default (symbol-property key 'throw-handler-default)))
    (or (and default (apply default key args))
	(apply error "unhandled-exception:" key args))))



(define (tm:sec obj) (vector-ref obj 0))
(define (tm:min obj) (vector-ref obj 1))
(define (tm:hour obj) (vector-ref obj 2))
(define (tm:mday obj) (vector-ref obj 3))
(define (tm:mon obj) (vector-ref obj 4))
(define (tm:year obj) (vector-ref obj 5))
(define (tm:wday obj) (vector-ref obj 6))
(define (tm:yday obj) (vector-ref obj 7))
(define (tm:isdst obj) (vector-ref obj 8))
(define (tm:gmtoff obj) (vector-ref obj 9))
(define (tm:zone obj) (vector-ref obj 10))

(define (set-tm:sec obj val) (vector-set! obj 0 val))
(define (set-tm:min obj val) (vector-set! obj 1 val))
(define (set-tm:hour obj val) (vector-set! obj 2 val))
(define (set-tm:mday obj val) (vector-set! obj 3 val))
(define (set-tm:mon obj val) (vector-set! obj 4 val))
(define (set-tm:year obj val) (vector-set! obj 5 val))
(define (set-tm:wday obj val) (vector-set! obj 6 val))
(define (set-tm:yday obj val) (vector-set! obj 7 val))
(define (set-tm:isdst obj val) (vector-set! obj 8 val))
(define (set-tm:gmtoff obj val) (vector-set! obj 9 val))
(define (set-tm:zone obj val) (vector-set! obj 10 val))

(define (tms:clock obj) (vector-ref obj 0))
(define (tms:utime obj) (vector-ref obj 1))
(define (tms:stime obj) (vector-ref obj 2))
(define (tms:cutime obj) (vector-ref obj 3))
(define (tms:cstime obj) (vector-ref obj 4))

(define file-position ftell)
(define (file-set-position port offset . whence)
  (let ((whence (if (eq? whence '()) SEEK_SET (car whence))))
    (seek port offset whence)))

(define (move->fdes fd/port fd)
  (cond ((integer? fd/port)
	 (dup->fdes fd/port fd)
	 (close fd/port)
	 fd)
	(else
	 (primitive-move->fdes fd/port fd)
	 (set-port-revealed! fd/port 1)
	 fd/port)))

(define (release-port-handle port)
  (let ((revealed (port-revealed port)))
    (if (> revealed 0)
	(set-port-revealed! port (- revealed 1)))))

(define (dup->port port/fd mode . maybe-fd)
  (let ((port (fdopen (apply dup->fdes port/fd maybe-fd)
		      mode)))
    (if (pair? maybe-fd)
	(set-port-revealed! port 1))
    port))

(define (dup->inport port/fd . maybe-fd)
  (apply dup->port port/fd "r" maybe-fd))

(define (dup->outport port/fd . maybe-fd)
  (apply dup->port port/fd "w" maybe-fd))

(define (dup port/fd . maybe-fd)
  (if (integer? port/fd)
      (apply dup->fdes port/fd maybe-fd)
      (apply dup->port port/fd (port-mode port/fd) maybe-fd)))

(define (duplicate-port port modes)
  (dup->port port modes))

(define (fdes->inport fdes)
  (let loop ((rest-ports (fdes->ports fdes)))
    (cond ((null? rest-ports)
	   (let ((result (fdopen fdes "r")))
	     (set-port-revealed! result 1)
	     result))
	  ((input-port? (car rest-ports))
	   (set-port-revealed! (car rest-ports)
			       (+ (port-revealed (car rest-ports)) 1))
	   (car rest-ports))
	  (else
	   (loop (cdr rest-ports))))))

(define (fdes->outport fdes)
  (let loop ((rest-ports (fdes->ports fdes)))
    (cond ((null? rest-ports)
	   (let ((result (fdopen fdes "w")))
	     (set-port-revealed! result 1)
	     result))
	  ((output-port? (car rest-ports))
	   (set-port-revealed! (car rest-ports)
			       (+ (port-revealed (car rest-ports)) 1))
	   (car rest-ports))
	  (else
	   (loop (cdr rest-ports))))))

(define (port->fdes port)
  (set-port-revealed! port (+ (port-revealed port) 1))
  (fileno port))

(define (setenv name value)
  (if value
      (putenv (string-append name "=" value))
      (putenv name)))


;;; {Load Paths}
;;;

;;; Here for backward compatability
;;
(define scheme-file-suffix (lambda () ".scm"))

(define (in-vicinity vicinity file)
  (let ((tail (let ((len (string-length vicinity)))
		(if (zero? len)
		    #f
		    (string-ref vicinity (- len 1))))))
    (string-append vicinity
		   (if (or (not tail)
			   (eq? tail #\/))
		       ""
		       "/")
		   file)))


;;; {Help for scm_shell}
;;; The argument-processing code used by Guile-based shells generates
;;; Scheme code based on the argument list.  This page contains help
;;; functions for the code it generates.

(define (command-line) (program-arguments))

;; This is mostly for the internal use of the code generated by
;; scm_compile_shell_switches.
(define (load-user-init)
  (let* ((home (or (getenv "HOME")
		   (false-if-exception (passwd:dir (getpwuid (getuid))))
		   "/"))  ;; fallback for cygwin etc.
	 (init-file (in-vicinity home ".guile")))
    (if (file-exists? init-file)
	(primitive-load init-file))))


;;; {Loading by paths}

;;; Load a Scheme source file named NAME, searching for it in the
;;; directories listed in %load-path, and applying each of the file
;;; name extensions listed in %load-extensions.
(define (load-from-path name)
  (start-stack 'load-stack
	       (primitive-load-path name)))



;;; {Transcendental Functions}
;;;
;;; Derived from "Transcen.scm", Complex trancendental functions for SCM.
;;; Written by Jerry D. Hedden, (C) FSF.
;;; See the file `COPYING' for terms applying to this program.
;;;

(define (exp z)
  (if (real? z) ($exp z)
      (make-polar ($exp (real-part z)) (imag-part z))))

(define (log z)
  (if (and (real? z) (>= z 0))
      ($log z)
      (make-rectangular ($log (magnitude z)) (angle z))))

(define (sqrt z)
  (if (real? z)
      (if (negative? z) (make-rectangular 0 ($sqrt (- z)))
	  ($sqrt z))
      (make-polar ($sqrt (magnitude z)) (/ (angle z) 2))))

(define expt
  (let ((integer-expt integer-expt))
    (lambda (z1 z2)
      (cond ((integer? z2)
             (if (negative? z2)
		 (/ 1 (integer-expt z1 (- z2)))
		 (integer-expt z1 z2)))
	    ((and (real? z2) (real? z1) (>= z1 0))
	     ($expt z1 z2))
	    (else
	     (exp (* z2 (log z1))))))))

(define (sinh z)
  (if (real? z) ($sinh z)
      (let ((x (real-part z)) (y (imag-part z)))
	(make-rectangular (* ($sinh x) ($cos y))
			  (* ($cosh x) ($sin y))))))
(define (cosh z)
  (if (real? z) ($cosh z)
      (let ((x (real-part z)) (y (imag-part z)))
	(make-rectangular (* ($cosh x) ($cos y))
			  (* ($sinh x) ($sin y))))))
(define (tanh z)
  (if (real? z) ($tanh z)
      (let* ((x (* 2 (real-part z)))
	     (y (* 2 (imag-part z)))
	     (w (+ ($cosh x) ($cos y))))
	(make-rectangular (/ ($sinh x) w) (/ ($sin y) w)))))

(define (asinh z)
  (if (real? z) ($asinh z)
      (log (+ z (sqrt (+ (* z z) 1))))))

(define (acosh z)
  (if (and (real? z) (>= z 1))
      ($acosh z)
      (log (+ z (sqrt (- (* z z) 1))))))

(define (atanh z)
  (if (and (real? z) (> z -1) (< z 1))
      ($atanh z)
      (/ (log (/ (+ 1 z) (- 1 z))) 2)))

(define (sin z)
  (if (real? z) ($sin z)
      (let ((x (real-part z)) (y (imag-part z)))
	(make-rectangular (* ($sin x) ($cosh y))
			  (* ($cos x) ($sinh y))))))
(define (cos z)
  (if (real? z) ($cos z)
      (let ((x (real-part z)) (y (imag-part z)))
	(make-rectangular (* ($cos x) ($cosh y))
			  (- (* ($sin x) ($sinh y)))))))
(define (tan z)
  (if (real? z) ($tan z)
      (let* ((x (* 2 (real-part z)))
	     (y (* 2 (imag-part z)))
	     (w (+ ($cos x) ($cosh y))))
	(make-rectangular (/ ($sin x) w) (/ ($sinh y) w)))))

(define (asin z)
  (if (and (real? z) (>= z -1) (<= z 1))
      ($asin z)
      (* -i (asinh (* +i z)))))

(define (acos z)
  (if (and (real? z) (>= z -1) (<= z 1))
      ($acos z)
      (+ (/ (angle -1) 2) (* +i (asinh (* +i z))))))

(define (atan z . y)
  (if (null? y)
      (if (real? z) ($atan z)
	  (/ (log (/ (- +i z) (+ +i z))) +2i))
      ($atan2 z (car y))))

(define (log10 arg)
  (/ (log arg) (log 10)))



;;; {Reader Extensions}
;;;

;;; Reader code for various "#c" forms.
;;;

(read-hash-extend #\' (lambda (c port)
			(read port)))

(define read-eval? (make-fluid))
(fluid-set! read-eval? #f)
(read-hash-extend #\.
                  (lambda (c port)
                    (if (fluid-ref read-eval?)
                        (eval (read port) (interaction-environment))
                        (error
                         "#. read expansion found and read-eval? is #f."))))


;;; {Command Line Options}
;;;

(define (get-option argv kw-opts kw-args return)
  (cond
   ((null? argv)
    (return #f #f argv))

   ((or (not (eq? #\- (string-ref (car argv) 0)))
	(eq? (string-length (car argv)) 1))
    (return 'normal-arg (car argv) (cdr argv)))

   ((eq? #\- (string-ref (car argv) 1))
    (let* ((kw-arg-pos (or (string-index (car argv) #\=)
			   (string-length (car argv))))
	   (kw (symbol->keyword (substring (car argv) 2 kw-arg-pos)))
	   (kw-opt? (member kw kw-opts))
	   (kw-arg? (member kw kw-args))
	   (arg (or (and (not (eq? kw-arg-pos (string-length (car argv))))
			 (substring (car argv)
				    (+ kw-arg-pos 1)
				    (string-length (car argv))))
		    (and kw-arg?
			 (begin (set! argv (cdr argv)) (car argv))))))
      (if (or kw-opt? kw-arg?)
	  (return kw arg (cdr argv))
	  (return 'usage-error kw (cdr argv)))))

   (else
    (let* ((char (substring (car argv) 1 2))
	   (kw (symbol->keyword char)))
      (cond

       ((member kw kw-opts)
	(let* ((rest-car (substring (car argv) 2 (string-length (car argv))))
	       (new-argv (if (= 0 (string-length rest-car))
			     (cdr argv)
			     (cons (string-append "-" rest-car) (cdr argv)))))
	  (return kw #f new-argv)))

       ((member kw kw-args)
	(let* ((rest-car (substring (car argv) 2 (string-length (car argv))))
	       (arg (if (= 0 (string-length rest-car))
			(cadr argv)
			rest-car))
	       (new-argv (if (= 0 (string-length rest-car))
			     (cddr argv)
			     (cdr argv))))
	  (return kw arg new-argv)))

       (else (return 'usage-error kw argv)))))))

(define (for-next-option proc argv kw-opts kw-args)
  (let loop ((argv argv))
    (get-option argv kw-opts kw-args
		(lambda (opt opt-arg argv)
		  (and opt (proc opt opt-arg argv loop))))))

(define (display-usage-report kw-desc)
  (for-each
   (lambda (kw)
     (or (eq? (car kw) #t)
	 (eq? (car kw) 'else)
	 (let* ((opt-desc kw)
		(help (cadr opt-desc))
		(opts (car opt-desc))
		(opts-proper (if (string? (car opts)) (cdr opts) opts))
		(arg-name (if (string? (car opts))
			      (string-append "<" (car opts) ">")
			      ""))
		(left-part (string-append
			    (with-output-to-string
			      (lambda ()
				(map (lambda (x) (display (keyword->symbol x)) (display " "))
				     opts-proper)))
			    arg-name))
		(middle-part (if (and (< (string-length left-part) 30)
				      (< (string-length help) 40))
				 (make-string (- 30 (string-length left-part)) #\ )
				 "\n\t")))
	   (display left-part)
	   (display middle-part)
	   (display help)
	   (newline))))
   kw-desc))



(define (transform-usage-lambda cases)
  (let* ((raw-usage (delq! 'else (map car cases)))
	 (usage-sans-specials (map (lambda (x)
				    (or (and (not (list? x)) x)
					(and (symbol? (car x)) #t)
					(and (boolean? (car x)) #t)
					x))
				  raw-usage))
	 (usage-desc (delq! #t usage-sans-specials))
	 (kw-desc (map car usage-desc))
	 (kw-opts (apply append (map (lambda (x) (and (not (string? (car x))) x)) kw-desc)))
	 (kw-args (apply append (map (lambda (x) (and (string? (car x)) (cdr x))) kw-desc)))
	 (transmogrified-cases (map (lambda (case)
				      (cons (let ((opts (car case)))
					      (if (or (boolean? opts) (eq? 'else opts))
						  opts
						  (cond
						   ((symbol? (car opts))  opts)
						   ((boolean? (car opts)) opts)
						   ((string? (caar opts)) (cdar opts))
						   (else (car opts)))))
					    (cdr case)))
				    cases)))
    `(let ((%display-usage (lambda () (display-usage-report ',usage-desc))))
       (lambda (%argv)
	 (let %next-arg ((%argv %argv))
	   (get-option %argv
		       ',kw-opts
		       ',kw-args
		       (lambda (%opt %arg %new-argv)
			 (case %opt
			   ,@ transmogrified-cases))))))))




;;; {Low Level Modules}
;;;
;;; These are the low level data structures for modules.
;;;
;;; !!! warning: The interface to lazy binder procedures is going
;;; to be changed in an incompatible way to permit all the basic
;;; module ops to be virtualized.
;;;
;;; (make-module size use-list lazy-binding-proc) => module
;;; module-{obarray,uses,binder}[|-set!]
;;; (module? obj) => [#t|#f]
;;; (module-locally-bound? module symbol) => [#t|#f]
;;; (module-bound? module symbol) => [#t|#f]
;;; (module-symbol-locally-interned? module symbol) => [#t|#f]
;;; (module-symbol-interned? module symbol) => [#t|#f]
;;; (module-local-variable module symbol) => [#<variable ...> | #f]
;;; (module-variable module symbol) => [#<variable ...> | #f]
;;; (module-symbol-binding module symbol opt-value)
;;;		=> [ <obj> | opt-value | an error occurs ]
;;; (module-make-local-var! module symbol) => #<variable...>
;;; (module-add! module symbol var) => unspecified
;;; (module-remove! module symbol) =>  unspecified
;;; (module-for-each proc module) => unspecified
;;; (make-scm-module) => module ; a lazy copy of the symhash module
;;; (set-current-module module) => unspecified
;;; (current-module) => #<module...>
;;;
;;;


;;; {Printing Modules}
;; This is how modules are printed.  You can re-define it.
;; (Redefining is actually more complicated than simply redefining
;; %print-module because that would only change the binding and not
;; the value stored in the vtable that determines how record are
;; printed. Sigh.)

(define (%print-module mod port)  ; unused args: depth length style table)
  (display "#<" port)
  (display (or (module-kind mod) "module") port)
  (let ((name (module-name mod)))
    (if name
	(begin
	  (display " " port)
	  (display name port))))
  (display " " port)
  (display (number->string (object-address mod) 16) port)
  (display ">" port))

;; module-type
;;
;; A module is characterized by an obarray in which local symbols
;; are interned, a list of modules, "uses", from which non-local
;; bindings can be inherited, and an optional lazy-binder which
;; is a (CLOSURE module symbol) which, as a last resort, can provide
;; bindings that would otherwise not be found locally in the module.
;;
;; NOTE: If you change here, you also need to change libguile/modules.h.
;;
(define module-type
  (make-record-type 'module
		    '(obarray uses binder eval-closure transformer name kind
			      observers weak-observers observer-id)
		    %print-module))

;; make-module &opt size uses binder
;;
;; Create a new module, perhaps with a particular size of obarray,
;; initial uses list, or binding procedure.
;;
(define make-module
    (lambda args

      (define (parse-arg index default)
	(if (> (length args) index)
	    (list-ref args index)
	    default))

      (if (> (length args) 3)
	  (error "Too many args to make-module." args))

      (let ((size (parse-arg 0 1021))
	    (uses (parse-arg 1 '()))
	    (binder (parse-arg 2 #f)))

	(if (not (integer? size))
	    (error "Illegal size to make-module." size))
	(if (not (and (list? uses)
		      (and-map module? uses)))
	    (error "Incorrect use list." uses))
	(if (and binder (not (procedure? binder)))
	    (error
	     "Lazy-binder expected to be a procedure or #f." binder))

	(let ((module (module-constructor (make-vector size '())
					  uses binder #f #f #f #f
					  '()
					  (make-weak-value-hash-table 31)
					  0)))

	  ;; We can't pass this as an argument to module-constructor,
	  ;; because we need it to close over a pointer to the module
	  ;; itself.
	  (set-module-eval-closure! module (standard-eval-closure module))

	  module))))

(define module-constructor (record-constructor module-type))
(define module-obarray  (record-accessor module-type 'obarray))
(define set-module-obarray! (record-modifier module-type 'obarray))
(define module-uses  (record-accessor module-type 'uses))
(define set-module-uses! (record-modifier module-type 'uses))
(define module-binder (record-accessor module-type 'binder))
(define set-module-binder! (record-modifier module-type 'binder))

;; NOTE: This binding is used in libguile/modules.c.
(define module-eval-closure (record-accessor module-type 'eval-closure))

(define module-transformer (record-accessor module-type 'transformer))
(define set-module-transformer! (record-modifier module-type 'transformer))
(define module-name (record-accessor module-type 'name))
(define set-module-name! (record-modifier module-type 'name))
(define module-kind (record-accessor module-type 'kind))
(define set-module-kind! (record-modifier module-type 'kind))
(define module-observers (record-accessor module-type 'observers))
(define set-module-observers! (record-modifier module-type 'observers))
(define module-weak-observers (record-accessor module-type 'weak-observers))
(define module-observer-id (record-accessor module-type 'observer-id))
(define set-module-observer-id! (record-modifier module-type 'observer-id))
(define module? (record-predicate module-type))

(define set-module-eval-closure!
  (let ((setter (record-modifier module-type 'eval-closure)))
    (lambda (module closure)
      (setter module closure)
      ;; Make it possible to lookup the module from the environment.
      ;; This implementation is correct since an eval closure can belong
      ;; to maximally one module.
      (set-procedure-property! closure 'module module))))

(begin-deprecated
 (define (eval-in-module exp mod)
   (issue-deprecation-warning
    "`eval-in-module' is deprecated.  Use `eval' instead.")
   (eval exp mod)))


;;; {Observer protocol}
;;;

(define (module-observe module proc)
  (set-module-observers! module (cons proc (module-observers module)))
  (cons module proc))

(define (module-observe-weak module proc)
  (let ((id (module-observer-id module)))
    (hash-set! (module-weak-observers module) id proc)
    (set-module-observer-id! module (+ 1 id))
    (cons module id)))

(define (module-unobserve token)
  (let ((module (car token))
	(id (cdr token)))
    (if (integer? id)
	(hash-remove! (module-weak-observers module) id)
	(set-module-observers! module (delq1! id (module-observers module)))))
  *unspecified*)

(define (module-modified m)
  (for-each (lambda (proc) (proc m)) (module-observers m))
  (hash-fold (lambda (id proc res) (proc m)) #f (module-weak-observers m)))


;;; {Module Searching in General}
;;;
;;; We sometimes want to look for properties of a symbol
;;; just within the obarray of one module.  If the property
;;; holds, then it is said to hold ``locally'' as in, ``The symbol
;;; DISPLAY is locally rebound in the module `safe-guile'.''
;;;
;;;
;;; Other times, we want to test for a symbol property in the obarray
;;; of M and, if it is not found there, try each of the modules in the
;;; uses list of M.  This is the normal way of testing for some
;;; property, so we state these properties without qualification as
;;; in: ``The symbol 'fnord is interned in module M because it is
;;; interned locally in module M2 which is a member of the uses list
;;; of M.''
;;;

;; module-search fn m
;;
;; return the first non-#f result of FN applied to M and then to
;; the modules in the uses of m, and so on recursively.  If all applications
;; return #f, then so does this function.
;;
(define (module-search fn m v)
  (define (loop pos)
    (and (pair? pos)
	 (or (module-search fn (car pos) v)
	     (loop (cdr pos)))))
  (or (fn m v)
      (loop (module-uses m))))


;;; {Is a symbol bound in a module?}
;;;
;;; Symbol S in Module M is bound if S is interned in M and if the binding
;;; of S in M has been set to some well-defined value.
;;;

;; module-locally-bound? module symbol
;;
;; Is a symbol bound (interned and defined) locally in a given module?
;;
(define (module-locally-bound? m v)
  (let ((var (module-local-variable m v)))
    (and var
	 (variable-bound? var))))

;; module-bound? module symbol
;;
;; Is a symbol bound (interned and defined) anywhere in a given module
;; or its uses?
;;
(define (module-bound? m v)
  (module-search module-locally-bound? m v))

;;; {Is a symbol interned in a module?}
;;;
;;; Symbol S in Module M is interned if S occurs in
;;; of S in M has been set to some well-defined value.
;;;
;;; It is possible to intern a symbol in a module without providing
;;; an initial binding for the corresponding variable.  This is done
;;; with:
;;;       (module-add! module symbol (make-undefined-variable))
;;;
;;; In that case, the symbol is interned in the module, but not
;;; bound there.  The unbound symbol shadows any binding for that
;;; symbol that might otherwise be inherited from a member of the uses list.
;;;

(define (module-obarray-get-handle ob key)
  ((if (symbol? key) hashq-get-handle hash-get-handle) ob key))

(define (module-obarray-ref ob key)
  ((if (symbol? key) hashq-ref hash-ref) ob key))

(define (module-obarray-set! ob key val)
  ((if (symbol? key) hashq-set! hash-set!) ob key val))

(define (module-obarray-remove! ob key)
  ((if (symbol? key) hashq-remove! hash-remove!) ob key))

;; module-symbol-locally-interned? module symbol
;;
;; is a symbol interned (not neccessarily defined) locally in a given module
;; or its uses?  Interned symbols shadow inherited bindings even if
;; they are not themselves bound to a defined value.
;;
(define (module-symbol-locally-interned? m v)
  (not (not (module-obarray-get-handle (module-obarray m) v))))

;; module-symbol-interned? module symbol
;;
;; is a symbol interned (not neccessarily defined) anywhere in a given module
;; or its uses?  Interned symbols shadow inherited bindings even if
;; they are not themselves bound to a defined value.
;;
(define (module-symbol-interned? m v)
  (module-search module-symbol-locally-interned? m v))


;;; {Mapping modules x symbols --> variables}
;;;

;; module-local-variable module symbol
;; return the local variable associated with a MODULE and SYMBOL.
;;
;;; This function is very important. It is the only function that can
;;; return a variable from a module other than the mutators that store
;;; new variables in modules.  Therefore, this function is the location
;;; of the "lazy binder" hack.
;;;
;;; If symbol is defined in MODULE, and if the definition binds symbol
;;; to a variable, return that variable object.
;;;
;;; If the symbols is not found at first, but the module has a lazy binder,
;;; then try the binder.
;;;
;;; If the symbol is not found at all, return #f.
;;;
(define (module-local-variable m v)
;  (caddr
;   (list m v
	 (let ((b (module-obarray-ref (module-obarray m) v)))
	   (or (and (variable? b) b)
	       (and (module-binder m)
		    ((module-binder m) m v #f)))))
;))

;; module-variable module symbol
;;
;; like module-local-variable, except search the uses in the
;; case V is not found in M.
;;
;; NOTE: This function is superseded with C code (see modules.c)
;;;      when using the standard eval closure.
;;
(define (module-variable m v)
  (module-search module-local-variable m v))


;;; {Mapping modules x symbols --> bindings}
;;;
;;; These are similar to the mapping to variables, except that the
;;; variable is dereferenced.
;;;

;; module-symbol-binding module symbol opt-value
;;
;; return the binding of a variable specified by name within
;; a given module, signalling an error if the variable is unbound.
;; If the OPT-VALUE is passed, then instead of signalling an error,
;; return OPT-VALUE.
;;
(define (module-symbol-local-binding m v . opt-val)
  (let ((var (module-local-variable m v)))
    (if var
	(variable-ref var)
	(if (not (null? opt-val))
	    (car opt-val)
	    (error "Locally unbound variable." v)))))

;; module-symbol-binding module symbol opt-value
;;
;; return the binding of a variable specified by name within
;; a given module, signalling an error if the variable is unbound.
;; If the OPT-VALUE is passed, then instead of signalling an error,
;; return OPT-VALUE.
;;
(define (module-symbol-binding m v . opt-val)
  (let ((var (module-variable m v)))
    (if var
	(variable-ref var)
	(if (not (null? opt-val))
	    (car opt-val)
	    (error "Unbound variable." v)))))



;;; {Adding Variables to Modules}
;;;
;;;


;; module-make-local-var! module symbol
;;
;; ensure a variable for V in the local namespace of M.
;; If no variable was already there, then create a new and uninitialzied
;; variable.
;;
(define (module-make-local-var! m v)
  (or (let ((b (module-obarray-ref (module-obarray m) v)))
	(and (variable? b)
	     (begin
	       (module-modified m)
	       b)))
      (and (module-binder m)
	   ((module-binder m) m v #t))
      (begin
	(let ((answer (make-undefined-variable)))
	  (variable-set-name-hint! answer v)
	  (module-obarray-set! (module-obarray m) v answer)
	  (module-modified m)
	  answer))))

;; module-ensure-local-variable! module symbol
;;
;; Ensure that there is a local variable in MODULE for SYMBOL.  If
;; there is no binding for SYMBOL, create a new uninitialized
;; variable.  Return the local variable.
;;
(define (module-ensure-local-variable! module symbol)
  (or (module-local-variable module symbol)
      (let ((var (make-undefined-variable)))
	(variable-set-name-hint! var symbol)
	(module-add! module symbol var)
	var)))

;; module-add! module symbol var
;;
;; ensure a particular variable for V in the local namespace of M.
;;
(define (module-add! m v var)
  (if (not (variable? var))
      (error "Bad variable to module-add!" var))
  (module-obarray-set! (module-obarray m) v var)
  (module-modified m))

;; module-remove!
;;
;; make sure that a symbol is undefined in the local namespace of M.
;;
(define (module-remove! m v)
  (module-obarray-remove!  (module-obarray m) v)
  (module-modified m))

(define (module-clear! m)
  (vector-fill! (module-obarray m) '())
  (module-modified m))

;; MODULE-FOR-EACH -- exported
;;
;; Call PROC on each symbol in MODULE, with arguments of (SYMBOL VARIABLE).
;;
(define (module-for-each proc module)
  (let ((obarray (module-obarray module)))
    (do ((index 0 (+ index 1))
	 (end (vector-length obarray)))
	((= index end))
      (for-each
       (lambda (bucket)
	 (proc (car bucket) (cdr bucket)))
       (vector-ref obarray index)))))


(define (module-map proc module)
  (let* ((obarray (module-obarray module))
	 (end (vector-length obarray)))

    (let loop ((i 0)
	       (answer '()))
      (if (= i end)
	  answer
	  (loop (+ 1 i)
		(append!
		 (map (lambda (bucket)
			(proc (car bucket) (cdr bucket)))
		      (vector-ref obarray i))
		 answer))))))


;;; {Low Level Bootstrapping}
;;;

;; make-root-module

;; A root module uses the pre-modules-obarray as its obarray.  This
;; special obarray accumulates all bindings that have been established
;; before the module system is fully booted.
;;
;; (The obarray continues to be used by code that has been closed over
;;  before the module system has been booted.)

(define (make-root-module)
  (let ((m (make-module 0)))
    (set-module-obarray! m (%get-pre-modules-obarray))
    m))

;; make-scm-module

;; The root interface is a module that uses the same obarray as the
;; root module.  It does not allow new definitions, tho.

(define (make-scm-module)
  (let ((m (make-module 0)))
    (set-module-obarray! m (%get-pre-modules-obarray))
    (set-module-eval-closure! m (standard-interface-eval-closure m))
    m))



;;; {Module-based Loading}
;;;

(define (save-module-excursion thunk)
  (let ((inner-module (current-module))
	(outer-module #f))
    (dynamic-wind (lambda ()
		    (set! outer-module (current-module))
		    (set-current-module inner-module)
		    (set! inner-module #f))
		  thunk
		  (lambda ()
		    (set! inner-module (current-module))
		    (set-current-module outer-module)
		    (set! outer-module #f)))))

(define basic-load load)

(define (load-module filename)
  (save-module-excursion
   (lambda ()
     (let ((oldname (and (current-load-port)
			 (port-filename (current-load-port)))))
       (basic-load (if (and oldname
			    (> (string-length filename) 0)
			    (not (char=? (string-ref filename 0) #\/))
			    (not (string=? (dirname oldname) ".")))
		       (string-append (dirname oldname) "/" filename)
		       filename))))))



;;; {MODULE-REF -- exported}
;;
;; Returns the value of a variable called NAME in MODULE or any of its
;; used modules.  If there is no such variable, then if the optional third
;; argument DEFAULT is present, it is returned; otherwise an error is signaled.
;;
(define (module-ref module name . rest)
  (let ((variable (module-variable module name)))
    (if (and variable (variable-bound? variable))
	(variable-ref variable)
	(if (null? rest)
	    (error "No variable named" name 'in module)
	    (car rest)			; default value
	    ))))

;; MODULE-SET! -- exported
;;
;; Sets the variable called NAME in MODULE (or in a module that MODULE uses)
;; to VALUE; if there is no such variable, an error is signaled.
;;
(define (module-set! module name value)
  (let ((variable (module-variable module name)))
    (if variable
	(variable-set! variable value)
	(error "No variable named" name 'in module))))

;; MODULE-DEFINE! -- exported
;;
;; Sets the variable called NAME in MODULE to VALUE; if there is no such
;; variable, it is added first.
;;
(define (module-define! module name value)
  (let ((variable (module-local-variable module name)))
    (if variable
	(begin
	  (variable-set! variable value)
	  (module-modified module))
	(let ((variable (make-variable value)))
	  (variable-set-name-hint! variable name)
	  (module-add! module name variable)))))

;; MODULE-DEFINED? -- exported
;;
;; Return #t iff NAME is defined in MODULE (or in a module that MODULE
;; uses)
;;
(define (module-defined? module name)
  (let ((variable (module-variable module name)))
    (and variable (variable-bound? variable))))

;; MODULE-USE! module interface
;;
;; Add INTERFACE to the list of interfaces used by MODULE.
;;
(define (module-use! module interface)
  (set-module-uses! module
		    (cons interface (delq! interface (module-uses module))))
  (module-modified module))


;;; {Recursive Namespaces}
;;;
;;;
;;; A hierarchical namespace emerges if we consider some module to be
;;; root, and variables bound to modules as nested namespaces.
;;;
;;; The routines in this file manage variable names in hierarchical namespace.
;;; Each variable name is a list of elements, looked up in successively nested
;;; modules.
;;;
;;;		(nested-ref some-root-module '(foo bar baz))
;;;		=> <value of a variable named baz in the module bound to bar in
;;;		    the module bound to foo in some-root-module>
;;;
;;;
;;; There are:
;;;
;;;	;; a-root is a module
;;;	;; name is a list of symbols
;;;
;;;	nested-ref a-root name
;;;	nested-set! a-root name val
;;;	nested-define! a-root name val
;;;	nested-remove! a-root name
;;;
;;;
;;; (current-module) is a natural choice for a-root so for convenience there are
;;; also:
;;;
;;;	local-ref name		==	nested-ref (current-module) name
;;;	local-set! name val	==	nested-set! (current-module) name val
;;;	local-define! name val	==	nested-define! (current-module) name val
;;;	local-remove! name	==	nested-remove! (current-module) name
;;;


(define (nested-ref root names)
  (let loop ((cur root)
	     (elts names))
    (cond
     ((null? elts)		cur)
     ((not (module? cur))	#f)
     (else (loop (module-ref cur (car elts) #f) (cdr elts))))))

(define (nested-set! root names val)
  (let loop ((cur root)
	     (elts names))
    (if (null? (cdr elts))
	(module-set! cur (car elts) val)
	(loop (module-ref cur (car elts)) (cdr elts)))))

(define (nested-define! root names val)
  (let loop ((cur root)
	     (elts names))
    (if (null? (cdr elts))
	(module-define! cur (car elts) val)
	(loop (module-ref cur (car elts)) (cdr elts)))))

(define (nested-remove! root names)
  (let loop ((cur root)
	     (elts names))
    (if (null? (cdr elts))
	(module-remove! cur (car elts))
	(loop (module-ref cur (car elts)) (cdr elts)))))

(define (local-ref names) (nested-ref (current-module) names))
(define (local-set! names val) (nested-set! (current-module) names val))
(define (local-define names val) (nested-define! (current-module) names val))
(define (local-remove names) (nested-remove! (current-module) names))
;;; boot-9.scm ends here
