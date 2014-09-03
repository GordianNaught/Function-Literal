(defun flatten (x)
  (if (consp x)
      (mapcan #'flatten x)
      (list x)))

(defun lambda-macro-var-p (x)
  (and (symbolp x)
    (let ((name (symbol-name x)))
      (and (char= #\% (aref name 0))
           (every #'identity (mapcar #'digit-char-p (coerce (subseq name 1) 'list)))))))

(defun lambda-macro-reader (stream junk); junk)
  (let* ((body (read stream t nil t))
         (flat-code (flatten body))
         (vars '()))
    (dolist (x flat-code)
      (if (and (lambda-macro-var-p x)
               (not (member x vars)))
        (push x vars)))
     (let* ((names
       (mapcar #'cadr
         (sort
           (mapcar (lambda (x)
                     (list
                       (read-from-string
                         (let ((num-str
                                (subseq (symbol-name x) 1)))
                           (if (string= num-str "")
                               "1"
                               num-str)))
                       x))
                   vars)
           (lambda (a b) (< (car a) (car b))))))
           (gensyms
             (mapcar
               (lambda (x)
                 (gensym
                   (concatenate 'string
                                "["
                                (symbol-name x)
                                "]")))
               names))
           (mapping (mapcar
                      (lambda (a b) `(,a ,b))
                      names
                      gensyms))
           (code `(lambda ,names ,body)))
       (dolist (association mapping code)
          (nsubst (cadr association) (car association) code)))))

;this may be desirable to avoid reader macro conflict
;(set-dispatch-macro-character
  ;#\# #\%
  ;#'lambda-macro-reader)

;this DOES conflict with a built in Common Lisp reader macro
(set-macro-character #\# #'lambda-macro-reader)
