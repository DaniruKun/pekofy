(eval-when (:compile-toplevel :load-toplevel :execute))

(defpackage :pekofy
  (:use :cl)
  (:export :toplevel :pekofy))

(in-package :pekofy)

;;;; Config -------------------------------------------------------------------

(defun pekofy-word-terminator (word)
  "Pekofy a WORD with a punctuator in the end. E.g. \"dog.\" -> \"dog peko.\" "
  (let* ((word-len (length word))
         (bare-word (subseq word 0 (1- word-len)))
         (punct (subseq word (1- word-len) word-len)))
    (concatenate 'string bare-word " peko" punct)))

(defun pekofy-word-co (word)
  "Pekofy a WORD that starts with co."
  (let ((word-tail (subseq word 2)))
    (uiop:strcat "peko" word-tail)))

(defun terminator-word-p (word)
  "Predicate for a WORD being a block terminator."
  (let* ((last-char (char word (1- (length word)))))
    (find last-char ".?!")))

(defun pekofy-word (word)
  "Pekofy a WORD."
  (apply (if (terminator-word-p word)
             #'pekofy-word-terminator
             #'pekofy-word-co)
         (list word)))

(defun pekofiable-p (word)
  "Predicate for if a WORD is pekofiable"
  (or (terminator-word-p word)
      (and (>= (length word) 2)
           (string= (string-downcase (subseq word 0 2))
                    "co"))))

(defun tokenize (input)
  "Produces a sequence of tokens from the given INPUT string."
  (uiop:split-string input :separator " "))

(defun sanitize (input)
  "Sanitize the INPUT string from CLI."
  (string-trim '(#\Space #\Newline #\Backspace #\Tab 
                 #\Linefeed #\Page #\Return #\Rubout)
               input))

(defun pekofy (input)
  "Pekofies the given INPUT string by adding a peko to the end of each sentence."
  (let* ((tokens (tokenize (sanitize input)))
         (processed-words (mapcar (lambda (word)
                                    (if (pekofiable-p word)
                                        (pekofy-word word)
                                        word))
                                  tokens)))
    (format t "~{~A~^ ~}" processed-words)))

;;;; CLI ----------------------------------------------------------------------

(defun toplevel ()
  (sb-ext:disable-debugger)
  (pekofy (second sb-ext:*posix-argv*)))
