(eval-when (:compile-toplevel :load-toplevel :execute))

(defpackage :pekofy
  (:use :cl)
  (:export :toplevel :pekofy))

(in-package :pekofy)

;;;; Config -------------------------------------------------------------------

(defun pekofy-word (word)
  "Pekofy a word with a punctuator in the end. E.g. \"dog.\" -> \"dog peko.\" "
  (let* ((word-len (length word))
         (bare-word (subseq word 0 (- word-len 1)))
         (punct (subseq word (- word-len 1) word-len)))
    (concatenate 'string bare-word " peko" punct)))

(defun terminator-word-p (word)
  "Predicate for a word being a block terminator."
  (let* ((last-char (char word (- (length word) 1))))
    (find last-char ".?!")))

(defun tokenize (input)
  "Produces a sequence of tokens from the given input."
  (uiop:split-string input :separator " "))

(defun sanitize (input)
  "Sanitize the input from CLI."
  (string-trim '(#\Space #\Newline #\Backspace #\Tab 
                 #\Linefeed #\Page #\Return #\Rubout)
               input))

(defun pekofy (input)
  "Pekofies the given input by adding a peko to the end of each sentence."
  (let* ((tokens (tokenize (sanitize input)))
         (processed-words (mapcar (lambda (word)
                                    (if (terminator-word-p word)
                                        (pekofy-word word)
                                        word))
                                  tokens)))
    (format t "~{~A~^ ~}" processed-words)))

;;;; CLI ----------------------------------------------------------------------

(defun toplevel ()
  (sb-ext:disable-debugger)
  (pekofy (second sb-ext:*posix-argv*)))
