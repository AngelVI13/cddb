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

(defun select-by-artist (artist)
  (remove-if-not
   #'(lambda (cd) (equal (getf cd :artist) artist))
   *db*))

(defun artist-selector (artist)
  #'(lambda (cd) (equal (getf cd :artist) artist)))

(defun where (&key title artist rating (ripped nil ripped-p))
  #'(lambda (cd)
      (and
        (if title    (equal (getf cd :title) title) t)
        (if artist   (equal (getf cd :artist) artist) t)
        (if rating   (equal (getf cd :rating) rating) t)
        (if ripped-p (equal (getf cd :ripped) ripped) t))))

;; (select (where :artist "Ed Sheeran"))
(defun select (selector-fn)
  (remove-if-not selector-fn *db*))

;; (update (where :artist "Ed Sheeran") :rating 11)
(defun update (selector-fn &key title artist rating (ripped nil ripped-p))
  (setf *db*
        (mapcar
         #'(lambda (row)
             (when (funcall selector-fn row)
               (if title    (setf (getf row :title) title))
               (if artist   (setf (getf row :artist) artist))
               (if rating   (setf (getf row :rating) rating))
               (if ripped-p (setf (getf row :ripped) ripped)))
             row) *db*)))

;; (delete-rows (where :title "Hello"))
(defun delete-rows (selector-fn)
  (setf *db* (remove-if selector-fn *db*)))

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

