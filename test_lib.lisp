(defvar *test-name* nil)

(defun report-result (result form)
  "Report the results of a single test case. Called by 'check'."
  (format t "~:[FAIL~;pass~] ... ~a: ~a~%" result *test-name* form)
  result)

(defmacro check (&body forms)
  "Run each expression in 'forms' as a test case."
  `(combine-results
     ,@(loop for form in forms collect `(report-result ,form ',form))))

(defmacro with-gensyms ((&rest names) &body body)
  `(let ,(loop for n in names collect `(,n (gensym)))
     ,@body))

(defmacro combine-results (&body forms)
  "Combine the results (as booleans) of evaluating 'forms' in order."
  (with-gensyms (result)
    `(let ((,result t))
       ,@(loop for f in forms collect `(unless ,f (setf ,result nil)))
       ,result)))

(defmacro deftest (name parameters &body body)
  "Define a test function. Within a test function we can call
   other test functions or use 'check' to run idividual test cases."
  `(defun ,name ,parameters
     (let ((*test-name* (append *test-name* (list ',name))))
       ,@body)))

(deftest test-+ ()
  (check (= (+ 1 2) 3)
           (= (+ 1 2 3) 6)))

(deftest test-* ()
  (check (= (* 2 2) 4)
           (= (* 3 5) 15)))


(deftest test-arithmetic ()
  (combine-results
    (test-+)
    (test-*)))

(deftest test-math ()
  (test-arithmetic))
