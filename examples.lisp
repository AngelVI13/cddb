(defun first-product-bigger (n)
  (dotimes (i 10)
    (dotimes (j 10)
      (when (> (* i j) n)
        (return-from first-product-bigger (list i j))))))

(defun plot (fn min max step)
  (loop for i from min to max by step do
        (loop repeat (funcall fn i) do (format t "*"))
        (format t "~%")))

        
;; (if (> 3 2) 
;;     (format t "Hello ~%") 
;;     (format t "Bye ~%"))
;; 
;; (defmacro when (condition &rest body)
;;   `(if ,condition (progn ,@body)))
;; 
;; (when (> 3 2)
;;   (format t "Hello ~%")
;;   (format t "Bye ~%"))

;; (let ((x (list "a" "b" "c")))
;;      (dolist (var x)
;;         (format t "~a~%" var)))

;; (dolist (x '(1 2 3)) 
;;     (print x)
;;     (if (evenp x)
;;        (return)))

;; (do ((end-time (+ (get-universal-time) 10)))
;;     ((<= end-time (get-universal-time)))
;;    (format t "waiting ~%")
;;    (sleep 1))

;; (format t "~a~%" (loop for x across "the quick brown fox jumps over the lazy dog"
;;                        counting (find x "aeiou")))

(defun primep (number)
  (when (> number 1)
    (loop for fac from 2 to (isqrt number) never (zerop (mod number fac)))))

(defun next-prime (number)
  (loop for n from number when (primep n) return n))


;; (do ((p (next-prime 0) (next-prime (1+ p))))
;;     ((> p 19))
;;   (format t "~d " p))

(defmacro with-gensyms ((&rest names) &body body)
  `(let ,(loop for n in names collect `(,n (gensym)))
     ,@body))

(defmacro do-primes ((var start end) &body body)
  (with-gensyms (ending-value-name)
    `(do ((,var (next-prime ,start) (next-prime (1+ ,var)))
          (,ending-value-name ,end))
         ((> ,var ,end))
       ,@body)))

;; Alternative example of do-primes with helper macro "once-only"
(defmacro do-primes1 ((var start end) &body body)
  (once-only (start end) ; Evaluates start and end parameters only once
    `(do ((,var (next-prime ,start) (next-prime (1+ ,var))))
       ((> ,var ,end))
       ,@body)))

(do-primes (p 0 19)
  (format t "~d " p))

(format t "~%~a~%" (macroexpand-1 '(do-primes (p 0 19) (format t "~d " p))))

(do-primes1 (p 0 19)
  (format t "~d " p))

(format t "~%~a~%" (macroexpand-1 '(do-primes1 (p 0 19) (format t "~d " p))))

;; (do ((p (next-prime 0) (next-prime (1+ p))))
;;   ((> p 19))
;;   (format t "~d " p))
