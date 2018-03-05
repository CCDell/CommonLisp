(in-package :todo)
(defvar *list* ())

(defun main ()
  (read-list-from-text)
  (cond ((and (= (length sb-ext:*posix-argv*) 2) (equal (elt sb-ext:*posix-argv* 1) "done"))
	 (clear-*list*))
	
	((and (= (length sb-ext:*posix-argv*) 3) (equal (elt sb-ext:*posix-argv* 1) "done"))
	 (clear-*list* (elt sb-ext:*posix-argv* 2))
	 (dump-*list*))

	((and (= (length sb-ext:*posix-argv*) 2) (equal (elt sb-ext:*posix-argv* 1) "todo"))
	 (dump-*list*))

	((and (> (length sb-ext:*posix-argv*) 2)
	      (equal (elt sb-ext:*posix-argv* 1) "acti"))
	 (add-*list* (concat-string-list (nth-value 1 (stndrd-funcs:split sb-ext:*posix-argv* 2))))
	 (dump-*list*))
	
	(t (format t "Valid arguments are done, done [index], and acti [activity].~%")))
  (write-list-to-text)
  (exit))

(defun dump-*list* ()
  (format t "todo:~%")
  (if (> (length *list*) 0)
      (loop for items in *list* do (format t "v^=> ~a~%" items))
      (format t "You have nothing to do!~%")))

(defun clear-*list*(&optional (indi nil indi-p))
  (if indi-p
      (setf *list* (concatenate 'list
				(subseq *list* 0 (parse-integer indi))
				(subseq *list* (1+ (parse-integer indi)))))
      (setf *list* ())))

;; Push the sent object to the front of the list.
(defun add-*list* (obj) 
  (push obj *list*))

;; Read list to text file and then reopen when called to read.
(defun write-list-to-text()
  (with-open-file (out-str "todo.txt" :direction :output :if-exists :supersede :if-does-not-exist :create)
    (dolist (str (reverse *list*))
      (write-line str out-str))))
(defun read-list-from-text()
  (when (probe-file "todo.txt")
    (with-open-file (in-str "todo.txt" :direction :input)
      (do ((line (read-line in-str nil :eof) (read-line in-str nil :eof))) ((equal line :eof))
	(push line *list*)))))
;; Add entrance and end dates to the print

