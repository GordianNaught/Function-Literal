(defun flatten (x)
  (if (consp x)
      (mapcan #'flatten x)
      (list x)))

(defun hash-keys (hash-table)
  (loop for key being the hash-keys of hash-table collect key))

(defun lambda-macro-var-p (thing)
  (and (symbolp thing)
    (let ((name (symbol-name thing)))
      (and (char= #\% (aref name 0))
           (every #'digit-char-p (coerce (subseq name 1) 'list))))))

(defun name-number-value (sym)
  (let ((num-str (subseq (symbol-name sym) 1)))
    (read-from-string
      (if (string= num-str "")
          "1"
          num-str))))

(defun lambda-macro-reader (stream junk); junk
  (let ((body (read stream t nil t))
        (vars (make-hash-table :test #'eq)))
    (mapcar (lambda (x)
              (when (and (lambda-macro-var-p x)
                         (not (gethash x vars)))
                      (setf (gethash x vars)
                            (gensym
                              (concatenate 'string
                                "[" (symbol-name x) "]")))))
            (flatten body))
    (let* ((names (mapcar (lambda (x) (gethash x vars))
                          (sort (hash-keys vars)
                                (lambda (a b)
                                  (< (name-number-value a)
                                     (name-number-value b))))))
           (code `(lambda ,names ,body)))
      (dolist (name (hash-keys vars) code)
        (nsubst (gethash name vars) name code)))))
                               
;this may be desirable to avoid reader macro conflict
;(set-dispatch-macro-character
  ;#\# #\%
  ;#'lambda-macro-reader)

;this DOES conflict with a built in Common Lisp reader macro
(set-macro-character #\# #'lambda-macro-reader)
