
; I would like to thank pjb for an insightful cleanup
; his changes also make this macro compatible outside
; of SBCL and CLISP.
;
; In the future I would like to remove the runtime overhead.

(defun lambda-macro-reader (stream a b)
  (declare (ignore a b))
  (let ((vargs (gensym)))
    `(lambda (&rest ,vargs)
       (progv (loop :for i :from 1 :to  (length ,vargs)
                    :collect (intern (format nil "%~A" i)))
           ,vargs
         (let ((% (first ,vargs)))
           ,(read stream nil nil t))))))

(set-dispatch-macro-character
  #\# #\f
  #'lambda-macro-reader)
