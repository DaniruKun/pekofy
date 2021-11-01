(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload '(:with-user-abort :adopt) :silent t))

(defpackage :pekofy
  (:use :cl)
  (:export :toplevel *ui*))

(in-package :pekofy)

;;;; Config -------------------------------------------------------------------
(defparameter *default-name* "World")

(defun pekofy-word (word)
  "Pekofy a word with a punctuator in the end. E.g. \"dog.\" -> \"dog peko.\" "
  (let* ((word-len (length word))
		 (bare-word (subseq word 0 (- word-len 1)))
		 (punct (subseq word (- word-len 1) word-len)))
	(concatenate 'string bare-word " peko" punct)))

(defun terminator-word-p (word)
  "Predicate for a word being a block terminator."
  (let* ((last-char (char word (- (length word) 1))))
	(or (eq #\. last-char)
		(eq #\! last-char)
		(eq #\? last-char))))

(defun tokenize (input)
  "Produces a sequence of tokens from the given input."
  (uiop:split-string input :separator " "))

(defun pekofy (input)
  "Pekofies the given input by adding a peko to the end of each sentence."
  (let* ((tokens (tokenize input))
		 (processed-words (mapcar (lambda (word)
									(if (terminator-word-p word)
										(pekofy-word word)
										word)) tokens)))
	(format t "~{~A~^ ~}" processed-words)))

;;;; CLI ----------------------------------------------------------------------
(defparameter *help*
  (adopt:make-option 'help
    :help "display help and exit"
    :long "help"
    :short #\h
    :reduce (constantly t)))

(defparameter *name*
  (adopt:make-option 'name
    :help (format nil "say hello to NAME (default ~A)" *default-name*)
    :long "name"
    :short #\n
    :parameter "NAME"
    :initial-value *default-name*
    :reduce #'adopt:last))


;;;; User Interface ----------------------------------------------
(defmacro exit-on-ctrl-c (&body body)
  `(handler-case (with-user-abort:with-user-abort (progn ,@body))
     (with-user-abort:user-abort () (sb-ext:exit :code 130))))

(defparameter *ui*
  (adopt:make-interface
   :name "pekofy"
   :summary "Example"
   :usage "Usage"
   :help "An example program to show how to make well-behaved CLI tools in Common Lisp."
    :examples '(("Say hello:" . "example")
                ("Say hello to Alice:" . "example --name Alice"))
    :contents (list *help* *name*)))

(defun toplevel ()
  (sb-ext:disable-debugger)
  (exit-on-ctrl-c
    (multiple-value-bind (arguments options) (adopt:parse-options-or-exit *ui*)
       ; Handle options.
      (handler-case (pekofy (first arguments))
        (user-error (e) (adopt:print-error-and-exit e))))))
