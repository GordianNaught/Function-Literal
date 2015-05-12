(defun unique (seq &key (test #'equalp))
  ;; unique elements from a sequence as a list
  (let ((uniques nil)
        (seen (make-hash-table :test test)))
    (map 'nil
         (lambda (element)
           (multiple-value-bind (val success) (gethash element seen)
             (declare (ignore val))
             (when (not success)
               (setf (gethash element seen) t)
               (push element uniques))))
         seq)
    (nreverse uniques)))

(defun unquoted-symbols (tree)
  ;; retrieve all unquoted symbols from code
  (let ((syms nil))
    (labels
      ((inner (tree quote-level)
         (if (consp tree)
           ;(or
             #+CLISP
               (case (car tree)
                 (quote)
                 ;(#.(car (read-from-string "`(a b ,c)"))
                 (system::backquote
                   (map nil (lambda (code) (inner code (+ 1 quote-level)))
                            (cdr tree)))
                 (system::unquote
                   (map nil (lambda (code) (inner code (- 1 quote-level)))
                            (cdr tree)))
                 (otherwise
                   (map nil (lambda (code) (inner code quote-level))
                            tree)))
             #+SBCL
               (case (car tree)
                 (quote)
                 (otherwise (map nil (lambda (code) (inner code quote-level))
                                     tree)))
             ;(error "unsuported Lisp implementation for #f macro"))
           (when (and (symbolp tree)
                 (= 0 quote-level))
             (push tree syms)))))
      (inner tree 0)
      syms)))

(defun subst-unquoted-symbol (replacement sym tree)
  ;; return a tree that is the given tree with all
  ;; unquoted instances of `sym' replaced with `replacement'
  (labels
    ((inner (tree quote-level)
       (if (consp tree)
         ;(or
           #+CLISP
             (case (car tree)
               (quote tree)
               ;(#.(car (read-from-string "`(a b ,c)"))
               (system::backquote
                 (cons 'system::backquote
                   (mapcar (lambda (code) (inner code (+ 1 quote-level)))
                           (cdr tree))))
               (system::unquote
                 (cons 'system::unquote
                   (mapcar (lambda (code) (inner code (- 1 quote-level)))
                           (cdr tree))))
               (otherwise
                 (mapcar (lambda (code) (inner code quote-level))
                         tree)))
           #+SBCL
             (case (car tree)
               (quote tree)
               (otherwise (mapcar (lambda (code) (inner code quote-level))
                                  tree)))
           ;(error "unsuported Lisp implementation for #f macro"))
         (if (and (eq tree sym)
                  (= 0 quote-level))
           replacement
           tree))))
    (inner tree 0)))

(defun lambda-macro-var-p (x)
  ;; determine if something could be a variable
  ;; for the `lambda-macro-reader'
  (and (symbolp x)
       (let ((str (symbol-name x)))
         (and (char= #\% (aref str 0))
              (every #'digit-char-p (subseq str 1))))))

;(defun lambda-macro-reader (stream junk) ;junk)
(defun lambda-macro-reader (stream junk1 junk2)
  ;; allows for Clojure-like function-literals
  ;; for example #F(* 2 %) is equivalent to
  ;; (lambda (x) (* 2 x))
  (declare (ignore junk1) (ignore junk2))
  (let* ((body (read stream t nil t))
         (vars
           (unique
             (remove-if-not #'lambda-macro-var-p
                            (unquoted-symbols body))))
         (names
           (sort vars #'<
             :key
             (lambda (x)
               (let ((num-str (subseq (symbol-name x) 1)))
                 (if (string= num-str "")
                   0
                   (read-from-string num-str))))))
         (gensyms
           (mapcar (lambda (x)
                     (gensym (concatenate 'string
                               "[" (symbol-name x) "]")))
                   names)))
    (let ((tree body))
      (loop for name in names
            for gensymbol in gensyms
        do (setf tree (subst-unquoted-symbol gensymbol name tree))
        finally (return `(lambda ,gensyms ,tree))))))

(set-dispatch-macro-character
  #\# #\f
  #'lambda-macro-reader)
