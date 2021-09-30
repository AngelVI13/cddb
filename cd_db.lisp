(defvar *db* nil)

(defun add-record (cd) (push cd *db*))

(defun print-db ()
  (dolist (cd *db*)
    (format t "~{~a:~10t~a~%~}~%" cd)))

(defun make-cd (title artist rating ripped)
  (list :title title :artist artist :rating rating :ripped ripped))

;; (add-record (make-cd "Paris in the rain" "Lauv" 8 t))
;; (add-record (make-cd "You need me, I don't need you" "Ed Sheeran" 10 t))
;; (add-record (make-cd "Lover" "Taylor Swift" 7 t))

;; (format t "DB ~a ~%" *db*)

(print-db)

(defun prompt-read (prompt)
  (format *query-io* "~a: " prompt) ; *query-io* is a global var - contains input stream connected to the terminal
  (force-output *query-io*) ; Necessary so lisp doesnt wait for newline before it displays the prompt
  (read-line *query-io*))

;; (format t "Hello, ~a ~%" (prompt-read "Whats your name"))

(defun prompt-for-cd ()
  (make-cd
    (prompt-read "Title")
    (prompt-read "Artist")
    ;Tries to convert input to INT, on failure just use 0
    (or (parse-integer (prompt-read "Rating") :junk-allowed t) 0)
    (y-or-n-p "Ripped [y/n]: ")))

(defun add-cds ()
  (loop (add-record (prompt-for-cd))
      (if (not (y-or-n-p "Another? [y/n]: ")) (return))))

(add-cds)

