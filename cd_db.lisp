(defvar *db* nil)

(defun add-record (cd) (push cd *db*))

(defun print-db ()
  (dolist (cd *db*)
    (format t "~{~a:~10t~a~%~}~%" cd)))

(defun save-db (filename)
  (with-open-file (out filename
                   :direction :output ; Direction -> output - means the file is open for writing
                   :if-exists :supersede) ;If-exists -> supersede - means that file will be overwritten if it exists
    (with-standard-io-syntax
      (print *db* out))))

(defun load-db (filename)
  (with-open-file (in filename)
    (with-standard-io-syntax
      (setf *db* (read in)))))

(defun make-cd (title artist rating ripped)
  (list :title title :artist artist :rating rating :ripped ripped))

;; (add-record (make-cd "Paris in the rain" "Lauv" 8 t))
;; (print-db)

(defun prompt-read (prompt)
  (format *query-io* "~a: " prompt) ; *query-io* is a global var - contains input stream connected to the terminal
  (force-output *query-io*) ; Necessary so lisp doesnt wait for newline before it displays the prompt
  (read-line *query-io*))

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

;; (add-cds)
;; (save-db (prompt-read "Filename to store DB: "))
;; (save-db "~/Documents/lisp/db.txt")
(load-db "~/Documents/lisp/db.txt")
(format t "~a~%" *db*)

