
(progn
  (ql:quickload "alexandria")
  (ql:quickload "cl-ppcre")
  (defpackage :cl-csharp-generator
	 (:use :cl
	       :alexandria
	       :cl-ppcre)
	 (:export
	  #:write-source)))

(in-package :cl-csharp-generator)

(setf (readtable-case *readtable*) :invert)

(defparameter *file-hashes* (make-hash-table))


(defun write-source (name code &key
				 (dir (user-homedir-pathname))
				 ignore-hash
				 (format t))
  (let* ((fn (merge-pathnames (format nil "~a" name)
			      dir))
	 (code-str (emit-cs :code code ;:header-only nil
			    ))
	 (fn-hash (sxhash fn))
	 (code-hash (sxhash code-str)))
   ; (format t "write code into file: '~a'~%" fn)
    (multiple-value-bind (old-code-hash exists) (gethash fn-hash *file-hashes*)
      (when (or (not exists) ignore-hash (/= code-hash old-code-hash)
		(not (probe-file fn)))
	;; store the sxhash of the c source in the hash table
	;; *file-hashes* with the key formed by the sxhash of the full
	;; pathname
	(setf (gethash fn-hash *file-hashes*) code-hash)
	(with-open-file (s fn
			   :direction :output
			   :if-exists :supersede
			   :if-does-not-exist :create)
	  (write-sequence code-str s))
	#+nil (when format
	 (sb-ext:run-program "/usr/bin/clang-format"
			     (list "-i"  (namestring fn)
	)))))))


(defclass method-declare-state ()
  ((type-env :initarg :type-env :accessor type-env :initform (make-hash-table))
   (public-p :initarg :public-p :accessor public-p :type boolean :initform nil)
   (static-p :initarg :static-p :accessor static-p :type boolean :initform nil)
   (protected-p :initarg :protected-p :accessor protected-p :type boolean :initform nil)
   (virtual-p :initarg :virtual-p :accessor virtual-p :type boolean :initform nil)
   (private-p :initarg :private-p :accessor private-p :type boolean :initform nil)
   (return-type :initarg :return-type :accessor return-type :initform nil)))

(defclass let-declare-state ()
  ((type-env :initarg :type-env :accessor type-env :initform (make-hash-table))
   ))


(defclass class-declare-state ()
  (
   (public-p :initarg :public-p :accessor public-p)
   ;(private-p :initarg :private-p :accessor private-p)
   ))

(defun method-consume-declare (body)
  "take a list of instructions from body, parse type declarations,
return the body without them and a state object that contains a hash
table with an environment, the return type and some boolean
switches. Return body and state."
  (let ((state (make-instance 'method-declare-state))
	(looking-p t) 
	(new-body nil))
    (loop for e in body do
	 (if looking-p
	     (if (listp e)
		 (if (eq (car e) 'declare)
		     (loop for declaration in (cdr e) do
			  (when (eq (first declaration) 'type)
			    (destructuring-bind (symb type &rest vars) declaration
			      (declare (ignorable symb))
			      (loop for var in vars do
				   (setf (gethash var (type-env state)) type))))
			  (when (eq (first declaration) 'static)
			    (setf (static-p state) t))
			  (when (eq (first declaration) 'public)
			    (setf (public-p state) t))
			  (when (eq (first declaration) 'private)
			    (setf (private-p state) t))
			  (when (eq (first declaration) 'protected)
			    (setf (protected-p state) t))
			  (when (eq (first declaration) 'virtual)
			    (setf (virtual-p state) t))
			  (when (eq (first declaration) 'values)
			    (destructuring-bind (symb &rest types-opt) declaration
			      (declare (ignorable symb))
			      ;; if no values specified parse-defun will emit void
			      ;; if (values :constructor) then nothing will be emitted
			      (let ((types nil))
				;; only collect types until occurrance of &optional
				(loop for type in types-opt do
				  (unless (eq #\& (aref (format nil "~a" type) 0))
				    (push type types)))
				(setf (return-type state) (reverse types))))))
		     (progn
		       (push e new-body)
		       (setf looking-p nil)))
		 (progn
		   (setf looking-p nil)
		   (push e new-body)))
	     (push e new-body)))
    (values (reverse new-body) state)))

(defun let-consume-declare (body)
  "take a list of instructions from body, parse type declarations,
return the body without them and a state object that contains a hash
table with an environment, the return type and some boolean
switches. Return body and state."
  (let ((state (make-instance 'let-declare-state))
	(looking-p t) 
	(new-body nil))
    (loop for e in body do
	 (if looking-p
	     (if (listp e)
		 (if (eq (car e) 'declare)
		     (loop for declaration in (cdr e) do
			  (when (eq (first declaration) 'type)
			    (destructuring-bind (symb type &rest vars) declaration
			      (declare (ignorable symb))
			      (loop for var in vars do
				   (setf (gethash var (type-env state)) type))))
			  )
		     (progn
		       (push e new-body)
		       (setf looking-p nil)))
		 (progn
		   (setf looking-p nil)
		   (push e new-body)))
	     (push e new-body)))
    (values (reverse new-body) state)))

(defun class-consume-declare (body)
  "take a list of instructions from body, parse type declarations,
return the body without them and a state object with some boolean
switches Return body and state."
  (let ((state (make-instance 'method-declare-state))
	(looking-p t) 
	(new-body nil))
    (loop for e in body do
	 (if looking-p
	     (if (listp e)
		 (if (eq (car e) 'declare)
		     (loop for declaration in (cdr e) do
		       (when (eq (first declaration) 'public)
			    (setf (public-p state) t))
		       )
		     (progn
		       (push e new-body)
		       (setf looking-p nil)))
		 (progn
		   (setf looking-p nil)
		   (push e new-body)))
	     (push e new-body)))
    (values (reverse new-body) state)))

(defun lookup-type (name &key state)
  "get the type of a variable from an environment in state"
  (gethash name (type-env state)))

(defun variable-declaration (&key name state emit)
  (let* ((type (lookup-type name :state state)))
    (cond ((null type)
	   (format nil "var ~a"
		   (funcall emit name)))
	  ((and (listp type)
		(eq 'array (first type)))
	   (progn
	      ;; array
	      (destructuring-bind (array_ element-type &rest dims) type
		(assert (eq array_ 'array))
		(format nil "~a[] ~a~{[~a]~}"
			(funcall emit element-type)
			(funcall emit name)
			(mapcar emit dims)))))
	  (t (format nil "~a ~a"
		(if type
		    (funcall emit type)
		    "var"
		    )
		(funcall emit name))))))

(defun parse-let (code emit)
  "let ({var | (var [init-form])}*) declaration* form*"
  (destructuring-bind (decls &rest body) (cdr code)
    (multiple-value-bind (body state) (let-consume-declare body)
      (with-output-to-string (s)
	(format
	 s "~a"
	 (funcall
	  emit
	  `(do0
	    ,@(loop for decl in decls
		    collect
		    (if (listp decl) ;; split into name and initform
			(destructuring-bind (name &optional value) decl
			  (format nil "~a ~@[ = ~a~];"
				  (variable-declaration :name name :state state :emit emit)
				  (when value
				    (funcall emit value))))
			(format nil "~a;"
				(variable-declaration :name decl :state state :emit emit))))
	    ,@body)))))))

;; positional:
;; static void PrintOrderDetails(string sellerName, int orderNum, string productName)

;; optional:
;; public void ExampleMethod(int required, string optionalstr = "default string",
;;     int optionalint = 10)



(defun parse-defmethod (code emit ;&key (class nil)
			)
  ;; defun function-name lambda-list [declaration*] form*
  (destructuring-bind (name lambda-list &rest body) (cdr code)
    (multiple-value-bind (body state) (method-consume-declare body)
      (multiple-value-bind (req-param opt-param res-param
				      key-param other-key-p
				      aux-param key-exist-p)
	  (parse-ordinary-lambda-list lambda-list)
	(declare (ignorable req-param opt-param res-param
			    key-param other-key-p aux-param key-exist-p))
	(with-output-to-string (s)
	  ;; if no visibility string is given, C# defaults to private
	  ;; here, i make visibility public by default,
	  
	  (let ((visibility (cond
			      ((and (return-type state)
				    (<= 1 (length (return-type state)))
				    (eq (first (return-type state))
					:constructor))
			       ;; (values :constructor) will not print anything
			       "")
			      (t "public"))))
	    (when (public-p state)
	      (setf visibility "public"))
	    (when (private-p state)
	      (setf visibility "private"))
	    (when (protected-p state)
	      (setf visibility "protected"))
	    
	    ;;         visibility                 name
	    ;;                 stat    virtu   ret   params
	    ;;         1       2       3       4  5  6
	    (format s "~@[~a ~]~@[~a ~]~@[~a ~]~a ~a ~a"
		    ;; 1 visibilty
		    visibility
		    ;; 2 static
		    (when (static-p state)
		      "static")
		    ;; 3 virtual
		    (when (virtual-p state)
		      "virtual")
		    ;; 4 return value
		    (let ((r (return-type state)))
		      (if (< 1 (length r))
			  (break "multiple return values unsupported: ~a"
				 r)
			  (if (car r)
			      (case (car r)
				(:constructor "") ;; (values :constructor) will not print anything
				(t (car r)))
			      "void")))
		    ;; 5 name
		    name
		    ;; 6 positional parameters, followed by key parameters
		    (funcall emit `(paren
				    ;; positional
				    ,@(loop for p in req-param
					    collect
					    (format nil "~a ~a"
						    (let ((type (gethash p (type-env state))))
						      (if type
							  (funcall emit type)
							  (break "can't find type for positional parameter ~a in defun"
								 p)))
						    p))
				    ;; optional parameters
				    ,@(loop for ((keyword-name name) init supplied-p) in key-param
					    collect
					    (progn
					      (format nil "~a ~a~@[ = ~a~]"
						      (let ((type (gethash name (type-env state))))
							(if type
							    (funcall emit type)
							    (break "can't find type for keyword parameter ~a in defun"
								   name)))
						      name
						      (when init
							(funcall emit init)))))))))
	  (format s "~a" (funcall emit `(progn ,@body))))))))

;; //[access modifier] - [class] - [identifier]
;; public class Customer
;; {
;;    // Fields, properties, methods and events go here...
;; }

;; inheritance:
;; Unlike C++, a class in C# can only directly inherit from one base class
;; public class Manager : Employee
;; {
;;     // Employee fields, properties, methods and events are inherited
;;     // New Manager fields, properties, methods and events go here...
;; }

(defun parse-defclass (code emit)
  ;; declass <name> ([<parent>]) [declaration*] form*
  ;; if no declaration is given, access modifier is public by default
  (destructuring-bind (name parent &rest body) (cdr code)
    (multiple-value-bind (body state) (class-consume-declare body)
      (with-output-to-string (s)
	(let ((visibility "public"))
	  (when (public-p state)
	    (setf visibility "public"))
	  (when (private-p state)
	    (setf visibility "private"))
	  (format s "~a class ~a ~@[ : ~a ~]"
		  visibility
		  name
		  (when parent
		    (car parent))
		  
		  )
	  (format s "~a" (funcall emit `(progn ,@body)))
	  )
	))))


(defun parse-lambda (code emit)
  ;;  lambda lambda-list [declaration*] form*
  ;; no return value:
  ;;  [] (int a, float b) { body }
  ;; with (declaration (values float)):
  ;;  [] (int a, float b) -> float { body }
  ;; support for captures (placed into the first set of brackets)
  ;; (declare (capture &app bla)) will result in [&app, bla]
  (destructuring-bind (lambda-list &rest body) (cdr code)
    (multiple-value-bind (body state ; env captures constructs const-p
			  ) (method-consume-declare body)
      (multiple-value-bind (req-param opt-param res-param
				      key-param other-key-p
				      aux-param key-exist-p)
	  (parse-ordinary-lambda-list lambda-list)
	(declare (ignorable req-param opt-param res-param
			    key-param other-key-p aux-param key-exist-p))
	(with-output-to-string (s)
	  (format s "~a"
		  ;(mapcar emit captures)
		  (funcall emit `(paren
				  ,@(loop for p in req-param collect
					 (format nil "~a ~a"
						 (let ((type (lookup-type p   :state state
									  )))
						   (if type
						       (funcall emit type)
						       ""))
						 p
						 ))))
		  #+nil (let ((r (gethash 'return-values env)))
		    (if (< 1 (length r))
			(funcall emit `(paren ,@r))
			(car r))))
	  (format s "=> ~a" (funcall emit `(progn ,@body))))))))

(defun print-sufficient-digits-f32 (f)
  "print a single floating point number as a string with a given nr. of                                                                                                                                             
  digits. parse it again and increase nr. of digits until the same bit                                                                                                                                              
  pattern."
    (let* ((a f)
           (digits 1)
           (b (- a 1)))
      (unless (= a 0)
	(loop while (and (< 1e-6 (/ (abs (- a b))
				    (abs a)))
			 (< digits 30))
           do
             (setf b (read-from-string (format nil "~,v,,,,,'eG"
					;"~,vG"
					       digits a
					       )))
             (incf digits)))
					;(format nil "~,vG" digits a)
      ;(format nil "~,v,,,,,'eGf" digits a)
      (let ((str
	     (format nil "~,v,,,,,'eG" digits a)))
	(format nil "~af" (string-trim '(#\Space) str))
	#+nil
	(if (find #\e str)
	    str
	    (format nil "~af" (string-trim '(#\Space) str))))))


(defun print-sufficient-digits-f64 (f)
  "print a double floating point number as a string with a given nr. of                                                                                                                                             
  digits. parse it again and increase nr. of digits until the same bit                                                                                                                                              
  pattern."

  (let* ((a f)
         (digits 1)
         (b (- a 1)))
    (unless (= a 0)
      (loop while (and (< 1d-12
			  (/ (abs (- a b))
			     (abs a))
			  )
		       (< digits 30)) do
           (setf b (read-from-string (format nil "~,vG" digits a)))
	   (incf digits)))
    ;(format t "~,v,,,,,'eG~%" digits a)
    (format nil "~,v,,,,,'eG" digits a)
    ;(substitute #\e #\d (format nil "~,vG" digits a))
    ))

			  
(progn
  (defun emit-cs (&key code (str nil)  (level 0))
    "evaluate s-expressions in code, emit a string. if hook-defun is not nil, hook-defun will be called with every function definition. this functionality is intended to collect function declarations."
					;(format t "~a~%" code)
    ;(format t "header-only=~a~%" header-only)
    (flet ((emit (code &key (dl 0))
	     "change the indentation level. this is used in do"
	     (emit-cs :code code
		     :level (+ dl level))))
      (if code
	  (if (listp code)
	      (progn
		(case (car code)
		  (comma
		   ;; comma {args}*
		   (let ((args (cdr code)))
		     (format nil "~{~a~^, ~}" (mapcar #'emit args))))
		  (semicolon
		   ;; semicolon {args}*
		   (let ((args (cdr code)))
		     (format nil "~{~a~^; ~}" (mapcar #'emit args))))
		  (space
		   ;; space {args}*
		   (let ((args (cdr code)))
		     (format nil "~{~a~^ ~}" (mapcar #'emit args))))
		  (space-n
		   ;; space-n {args}*
		   ;; like space but no semicolon at the end
		   (let ((args (cdr code)))
		     (format nil "~{~a~^ ~}" (mapcar #'emit args))))
		  (namespace
		   ;; namespace <name> {body}*
		   (destructuring-bind (ns name &rest body) code
		     (format nil "~a" (emit `(space-n namespace ,name
						      (progn ,@body))))))
		  (comments (let ((args (cdr code)))
			      (with-output-to-string (s)
			       (loop for arg in args
				     do
					(format s "~{// ~a~%~}"
						(cl-ppcre:split "\\n+" arg))))
                              ))
		  (paren
		   ;; paren {args}*
		   (let ((args (cdr code)))
		     (format nil "(~{~a~^, ~})" (mapcar #'emit args))))
		  (angle
		   (let ((args (cdr code)))
		     (format nil "<~{~a~^, ~}>" (mapcar #'emit args))))
		  (bracket
		   ;; bracket {args}*
		   (let ((args (cdr code)))
		     (format nil "[~{~a~^, ~}]" (mapcar #'emit args))))
		  (bracket-n
		   ;; bracket {args}*
		   ;; like bracket but no semicolon at the end
		   (let ((args (cdr code)))
		     (format nil "[~{~a~^, ~}]" (mapcar #'emit args))))
		  (curly
		   ;; curly {args}*
		   (let ((args (cdr code)))
		     (format nil "{~{~a~^, ~}}" (mapcar #'emit args))))
		  (new
		   ;; new arg
		   (let ((arg (cadr code)))
		     (format nil "new ~a" (emit arg))))
		  (using
		   ;; using <sys0> <sys1> ..
		   (let ((args (cdr code)))
		     (format nil "~{using ~a~^;~%~}" (mapcar #'emit args)))
		   )
		  (indent
		   ;; indent form
		   (format nil "~{~a~}~a"
			   ;; print indentation characters
			   (loop for i below level collect "    ")
			   (emit (cadr code))))
		  #+nil (split-header-and-code
		   (let ((args (cdr code)))
		     (destructuring-bind (arg0 arg1) args
		       (progn ;if hook-defclass
			   #+nil (funcall hook-defclass (format nil "~a" (emit `(do0 ,arg0))))
			   (format nil "~a" (emit `(do0 ,arg1))))
		       
		       )))
		  (do0 (with-output-to-string (s)
			 ;; do0 {form}*
			 ;; write each form into a newline, keep current indentation level
			 (format s "~{~&~a~}"
				 (mapcar
				  #'(lambda (x)
				      (let ((b (emit `(indent ,x) :dl 0)))
					(format nil "~a~a"
						b
						;; don't add semicolon if there is already one
						;; or if x contains a string
						;; or if x is an s-expression with a c thing that doesn't end with semicolon
						(if (or (eq #\; (aref b (- (length b) 1)))
							(and (typep x 'string))
							(and (typep x '(array character (*))))
							(and (listp x)
							     (member (car x) `(defun do do0 progn
										for for-range dotimes
										while
										include case
										when if unless
										let
										split-header-and-code
										defun defmethod defclass
										space-n namespace bracket-n
										comments 
										handler-case progn foreach))))
						    ""
						    ";"))))
				  (cdr code)))
			 #+nil
			 (let ((a (emit (cadr code))))
			   (format s "~&~a~a~{~&~a~}"
				   a
				   (if (eq #\; (aref a (- (length a) 1)))
				       ""
				       ";")
				   (mapcar
				    #'(lambda (x)
					(let ((b (emit `(indent ,x) 0)))
					  (format nil "~a~a"
						  b
						  (if (eq #\; (aref b (- (length b) 1)))
						      ""
						      ";"))))
				    (cddr code))))))
		  (include (let ((args (cdr code)))
			     ;; include {name}*
			     ;; (include <stdio.h>)   => #include <stdio.h>
			     ;; (include interface.h) => #include "interface.h"
			     (let ((str (with-output-to-string (s)
					  (loop for e in args do
					    ;; emit string if first character is not <
					    (format s "~&#include ~a"
						    (emit (if (eq #\< (aref (format nil "~a" e) 0))
							      e
							      `(string ,e))))))))
			       #+nil (when hook-defclass
				 (funcall hook-defclass (format nil "~a~%" str)))
			       str)))
		  (progn (with-output-to-string (s)
			   ;; progn {form}*
			   ;; like do but surrounds forms with braces.
			   (format s "{~{~&~a~}~&}" (mapcar #'(lambda (x) (emit `(indent (do0 ,x)) :dl 1)) (cdr code)))))
		  (do (with-output-to-string (s)
			;; do {form}*
			;; print each form on a new line with one more indentation.
			(format s "~{~&~a~}" (mapcar #'(lambda (x) (emit `(indent (do0 ,x)) :dl 1)) (cdr code)))))
		  #+nil(defclass+
		   ;; for writing directly into header with all code in class
		   (destructuring-bind (name parents &rest body) (cdr code)
		     (let ((class-name (if (listp name)
					   (car name)
					   name))
			   (class-template nil)
			   (class-template-instance nil))
		       (when (listp name)
			 (destructuring-bind (name &key (template nil) (template-instance nil)) name
			   (setf class-name name
				 class-template template
				 class-template-instance template-instance)))
		       (format nil "~@[template<~a> ~]class ~a~@[<~a>~] ~@[: ~a~] ~a"
			       
			       class-template
			       (emit class-name)

			       class-template-instance
			       
			       (when parents
				 (emit `(comma ,parents)))
			       (emit `(progn ,@body)
				     :class nil ;(emit name)
				     :hook-fun nil
				     :hook-class hook-defclass
				     :header-only-p nil
				     :in-class-p 'defclass+))))
		   )
		  
		  (protected (format nil "protected ~a" (emit (cadr code))))
		  (public (format nil "public ~a" (emit (cadr code))))
		  (defmethod
		      (parse-defmethod code #'emit ))
		  (defclass
		      (parse-defclass code #'emit ))
		  #+nil (defmethod*
			 (if hook-defclass
			     (parse-defmethod code #'emit :class current-class :header-only t)
			     (parse-defmethod code #'emit :class current-class :header-only nil)))
		  #+nil (defun
		      (prog1
			  (parse-defun code #'emit )
					;(format t "defun ~a~%" (subseq code 0 (min 4 (length code))))
			#+nil (when hook-defun ;(and hook-defun (not current-class))
			  ;; only emit function headers when we are not currently in defclass
			  (funcall hook-defun (parse-defun code #'emit :header-only t :class current-class)))))
		#+nil  (defun* (parse-defun code #'emit :header-only t :class current-class))
		  #+nil (defun+ (parse-defun code #'emit :header-only nil :class current-class))
		  (return (format nil "return ~a" (emit (car (cdr code)))))
		  
		  
		  (throw (format nil "throw ~a" (emit (car (cdr code)))))
		  (cast (destructuring-bind (type value) (cdr code)
			  (format nil "(~a) ~a"
				  (emit type)
				  (emit value))))
		  
		  (let (parse-let code #'emit))
		  (setf 
		   (let ((args (cdr code)))
		     ;; "setf {pair}*"
		     (format nil "~a"
			     (emit
			      `(do0 
				,@(loop for i below (length args) by 2 collect
								       (let ((a (elt args i))
									     (b (elt args (+ 1 i))))
									 `(= ,a ,b))))))))
		  (setf?? 
		   (let ((args (cdr code)))
		     ;; "setf?? {pair}*" like setf but only if lhs isn't null
		     (format nil "~a"
			     (emit
			      `(do0 
				,@(loop for i below (length args) by 2 collect
								       (let ((a (elt args i))
									     (b (elt args (+ 1 i))))
									 `(??= ,a ,b))))))))
		  (not (format nil "!(~a)" (emit (car (cdr code)))))
		  (deref (format nil "*(~a)" (emit (car (cdr code)))))
		  (ref (format nil "&(~a)" (emit (car (cdr code)))))
		  (+ (let ((args (cdr code)))
		       ;; + {summands}*
		       (format nil "(~{(~a)~^+~})" (mapcar #'emit args))))
		  (- (let ((args (cdr code)))
		       (if (eq 1 (length args))
			   (format nil "(-(~a))" (emit (car args))) ;; py
			   (format nil "(~{(~a)~^-~})" (mapcar #'emit args)))))
		  (* (let ((args (cdr code)))
		       (format nil "(~{(~a)~^*~})" (mapcar #'emit args))))
		  (^ (let ((args (cdr code)))
		       (format nil "(~{(~a)~^^~})" (mapcar #'emit args))))
		  (& (let ((args (cdr code)))
		       (format nil "(~{(~a)~^&~})" (mapcar #'emit args))))
		  (/ (let ((args (cdr code)))
		       (if (eq 1 (length args))
			   (format nil "(1.0/(~a))" (emit (car args))) ;; py
			   (format nil "(~{(~a)~^/~})" (mapcar #'emit args)))))
		  
		  (logior (let ((args (cdr code))) ;; py
			    (format nil "(~{(~a)~^ || ~})" (mapcar #'emit args))))
		  (logand (let ((args (cdr code))) ;; py
			    (format nil "(~{(~a)~^ && ~})" (mapcar #'emit args))))
		  (xor (let ((args (cdr code))) ;; py
			    (format nil "(~{(~a)~^ ^ ~})" (mapcar #'emit args))))
		  (or (let ((args (cdr code)))
			(format nil "(~{(~a)~^|~})" (mapcar #'emit args))))
		  (and (let ((args (cdr code)))
			 (format nil "(~{(~a)~^&~})" (mapcar #'emit args))))
		  (= (destructuring-bind (a b) (cdr code)
		       ;; = pair
		       (format nil "~a=~a" (emit a) (emit b))))
		  (??= (destructuring-bind (a b) (cdr code)
		       ;; = pair
		       (format nil "~a??=~a" (emit a) (emit b))))
		  (/= (destructuring-bind (a b) (cdr code)
			(format nil "~a/=(~a)" (emit a) (emit b))))
		  (*= (destructuring-bind (a b) (cdr code)
			(format nil "~a*=(~a)" (emit a) (emit b))))
		  (^= (destructuring-bind (a b) (cdr code)
			(format nil "(~a)^=(~a)" (emit a) (emit b))))
		  (<= (destructuring-bind (a b &optional c) (cdr code)
			(if c
			    (format nil "(((~a)<=(~a)) && ((~a)<=(~a)))" (emit a) (emit b)
				    (emit b) (emit c))
			    (format nil "(~a)<=(~a)" (emit a) (emit b)))))
		  (< (destructuring-bind (a b &optional c) (cdr code)
		       (if c
			   (format nil "(((~a)<(~a)) && ((~a)<(~a)))" (emit a) (emit b)
				   (emit b) (emit c))
			   (format nil "(~a)<(~a)" (emit a) (emit b)))))
		  (!= (destructuring-bind (a b) (cdr code)
			(format nil "(~a)!=(~a)" (emit a) (emit b))))
		  (== (destructuring-bind (a b) (cdr code)
			(format nil "(~a)==(~a)" (emit a) (emit b))))
		  
		  (% (destructuring-bind (a b) (cdr code)
		       (format nil "~a%~a" (emit a) (emit b))))
		  (<< (destructuring-bind (a &rest rest) (cdr code)
			(format nil "(~a)~{<<(~a)~}" (emit a) (mapcar #'emit rest))))
		  (>> (destructuring-bind (a &rest rest) (cdr code)
			(format nil "(~a)~{>>(~a)~}" (emit a) (mapcar #'emit rest))))
		  #+nil (>> (destructuring-bind (a b) (cdr code)
			      (format nil "(~a)>>~a" (emit a) (emit b))))
		  (incf (destructuring-bind (a &optional b) (cdr code) ;; py
			  (if b
			      (format nil "(~a)+=(~a)" (emit a) (emit b))
			      (format nil "(~a)++" (emit a)))))
		  (decf (destructuring-bind (a &optional b) (cdr code)
			  (if b
			      (format nil "(~a)-=(~a)" (emit a) (emit b))
			      (format nil "(~a)--" (emit a)))))
		  (string (format nil "\"~a\"" (cadr code)))
		  (string$ (format nil "$\"~a\"" (cadr code)))
		  (string@ (format nil "@\"~a\"" (cl-ppcre:regex-replace-all
						  "\""
						  (cadr code)
						  "\"\"")))
		  (string$@ (format nil "$@\"~a\"" (cl-ppcre:regex-replace-all
						  "\""
						  (cadr code)
						  "\"\"")))
		  (string-u8 (format nil "u8\"(~a)\"" (cadr code)))
		  (char (format nil "'~a'" (cadr code)))
		  (hex (destructuring-bind (number) (cdr code)
			 (format nil "0x~x" number)))
		  (? (destructuring-bind (a b &optional c) (cdr code)
		       (if c
			   (format nil "(~a) ? (~a) : (~a)" (emit a) (emit b) (emit c))
			   (format nil "(~a) ? (~a)" (emit a) (emit b)))))
		  (if (destructuring-bind (condition true-statement &optional false-statement) (cdr code)
			(with-output-to-string (s)
			  (format s "if ( ~a ) ~a"
				  (emit condition)
				  (emit `(progn ,true-statement)))
			  (when false-statement
			    (format s " else ~a"
				    (emit `(progn ,false-statement)))))))
		  (when (destructuring-bind (condition &rest forms) (cdr code)
			  (emit `(if ,condition
				     (do0
				      ,@forms)))))
		  (unless (destructuring-bind (condition &rest forms) (cdr code)
			    (emit `(if (not ,condition)
				       (do0
					,@forms)))))
		  
		  (dot (let ((args (cdr code)))
			 (format nil "~{~a~^.~}" (mapcar #'emit args))))
		  

		  (aref (destructuring-bind (name &rest indices) (cdr code)
					;(format t "aref: ~a ~a~%" (emit name) (mapcar #'emit indices))
			  (format nil "~a~{[~a]~}" (emit name) (mapcar #'emit indices))))
		  
		  (-> (let ((args (cdr code)))
			(format nil "~{~a~^->~}" (mapcar #'emit args))))
		  
		  (lambda (parse-lambda code #'emit))
		  
		  (case
		      ;; case keyform {normal-clause}* [otherwise-clause]
		      ;; normal-clause::= (keys form*) 
		      ;; otherwise-clause::= (t form*) 
		      
		      (destructuring-bind (keyform &rest clauses)
			  (cdr code)
			(format
			 nil "switch(~a) ~a"
			 (emit keyform)
			 (emit
			  `(progn
			     ,@(loop for c in clauses collect
						      (destructuring-bind (key &rest forms) c
							(if (eq key t)
							    (format nil "default: ~a"
								    (emit
								     `(progn
								       ,@forms #+nil (mapcar #'emit
										 forms)
								       break)))
							    (format nil "case ~a: ~a"
								    (emit key)
								    (emit
								     `(progn
								       ,@forms #+nil (mapcar #'emit
										 forms)
								       break)))))))))))
		  (for (destructuring-bind ((start end iter) &rest body) (cdr code)
			 (format nil "for (~@[~a~];~@[~a~];~@[~a~]) ~a"
				 (emit start)
				 (emit end)
				 (emit iter)
				 (emit `(progn ,@body)))))
		  (for-range (destructuring-bind ((var-decl range) &rest statement-list)
				 (cdr code)
			       (format str "for(~a : ~a) ~a"
				       (if (atom var-decl)
					   (format nil "auto ~a" var-decl)
					   (destructuring-bind (name &key (type 'auto)) var-decl
					     (format nil "~a ~a" type name)))
				       (emit range)
				       (emit `(progn ,@statement-list)))))
		  (dotimes (destructuring-bind ((i n &optional (step 1)) &rest body) (cdr code)
			     (emit `(for (,(format nil "var ~a = 0"
						   (emit i)) ;; int
					  (< ,(emit i) ,(emit n))
					  (incf ,(emit i) ,(emit step)))
					 ,@body))))
		  #-generic-c
		  (foreach (destructuring-bind ((item collection) &rest body) (cdr code)
			    (multiple-value-bind (body state) (let-consume-declare body)
			      (format nil "foreach (~a ~a in ~a) ~a"
				      (or (lookup-type item :state state)
					  "var")
				      (emit item)
				      (emit collection)
				      (emit `(progn ,@body))))))
		  #+generic-c
		  (foreach
		   (destructuring-bind ((item collection) &rest body) (cdr code)
		     (let ((itemidx (format nil "~a_idx" (emit item))))
		       (format nil
			       "~a"
			       (emit
				`(dotimes (,itemidx (/ (sizeof ,collection)
						       (sizeof (deref ,collection))))
				   (let ((,item (aref ,collection ,itemidx)))
				     (progn ,@body))))))))
		  (while ;; while condition {forms}*
		   (destructuring-bind (condition &rest body) (cdr code)
		     (format nil "while (~a) ~a"
			     (emit condition)
			     (emit `(progn ,@body)))))
		  (deftype
		      ;; deftype name lambda-list {form}*
		      ;; only the first form of the body is used, lambda list is ignored
		      (destructuring-bind (name lambda-list &rest body) (cdr code)
			(declare (ignore lambda-list))
			(format nil "typedef ~a ~a" (emit (car body)) name)))
		  (struct (format nil "struct ~a" (emit (car (cdr code)))))
		  (defstruct0
		   ;; defstruct without init-form
		   ;; defstruct name {slot-description}*
		   ;; slot-description::= slot-name | (slot-name [slot-type])
		   
		   ;; a slot-name without type can be used to create a
		   ;; composed type with a struct embedding
		   
		   ;; i think i should use this pattern that works in C
		   ;; and in C++. Typedef isn't strictly necessary in
		   ;; C++, execept if you overload the struct name with
		   ;; a function:
		   
		   ;; struct 
		   ;; { 
		   ;;    char name[50]; 
		   ;;    char street[100]; 
		   ;;    char city[50]; 
		   ;;    char state[20]; 
		   ;;    int pin; 
		   ;; } Address;
		   ;; typedef struct Address Address;
		   ;; int Address(int b){ ...}
		   
		   ;; https://stackoverflow.com/questions/1675351/typedef-struct-vs-struct-definitions
		   (destructuring-bind (name &rest slot-descriptions) (cdr code)
		     (format nil "~a"
			     (emit `(do0
				     ,(format nil "struct ~a ~a;"
					      name
					      (emit
					       `(progn
						  ,@(loop for desc in slot-descriptions collect
											(destructuring-bind (slot-name &optional type value) desc
											  (declare (ignorable value))
											  (format nil "~a ~a;" (emit type) (emit slot-name)))))))
				     (deftype ,name () (struct ,name)))))))
		  (handler-case
		      ;; handler-case expression [[{error-clause}*]]
;;; error-clause::= (typespec ([var]) declaration* form*) ;; note: declarations are currently unsupported
		      ;; error-clause::= (typespec ([var]) form*)
		      ;; if typespec is t, catch any kind of exception

		      ;; (handler-case (progn forma formb)
		      ;;   (typespec1 (var1) form1)
		      ;;   (typespec2 (var2) form2))

		      ;; a clause such as:
		      ;; (typespec (var) (declare (ignore var)) form)
		      ;; can be written as (typespec () form)
		      

		      
		      ;; try {
		      ;;   // code here
		      ;; }
		      ;; catch (int param) { cout << "int exception"; }
		      ;; catch (char param) { cout << "char exception"; }
		      ;; catch (...) { cout << "default exception"; }
		      
		      (destructuring-bind (expr &rest clauses) (cdr code)
			(with-output-to-string (s)
			  (format s "try ~a"
				  (if (eq 'progn (car expr))
				      (emit expr)
				      (emit `(progn ,expr))))
			  (loop for clause in clauses do
			    (destructuring-bind (typespec (var) &rest forms) clause
			      (format s "catch (~a) ~a"
				      (if (and (eq 't typespec)
					       (null var))
					  (format nil "...")
					  (format nil "~a ~a" typespec var))
				      (emit `(progn ,@forms))))))))
		  (t (destructuring-bind (name &rest args) code

		       (if (listp name)
			   ;; lambda call and similar complex constructs
			   (format nil "(~a)~a"
				   (emit name)
				   (emit `(paren ,@args))
				   )
			   ;; function call
			   
			   
			   (progn	;if
			     
			     #+nil(and
				   (= 1 (length args))
				   (eq (aref (format nil "~a" (car args)) 0) #\.))
			     #+nil (format nil "~a~a" name
					   (emit args))
			     (format nil "~a~a"
				     (emit name)
				     (emit `(paren ,@args)))))))))
	      (cond
		((symbolp code)
					;(cl-ppcre::regex-replace-all "--" "bla--fub" "::")
		 (cl-ppcre::regex-replace-all "--" (format nil "~a" code) "::")
					;(substitute #\: #\- (format nil "~a" code))
		 )
		((stringp code) ;; print variable
		 (format nil "~a" code))
		((numberp code) ;; print constants
		 (cond ((integerp code) (format str "~a" code))
		       ((floatp code)
			(typecase code
			  (single-float (format str "(~a)" (print-sufficient-digits-f32 code)))
			  (double-float (format str "(~a)" (print-sufficient-digits-f64 code))))
			#+nil (format str "(~a)" (print-sufficient-digits-f64 code)))))))
	  "")))
  )


 

