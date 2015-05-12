Function-Literal
================

Clojure inspired Common Lisp function literal read macro

This file contains a read macro allowing for function
literals in Common Lisp similar to those in Clojure,
for example:

    (mapcar #f(- % %2) '(1 2 3 4) '(5 6 7 8))

is equivalent to:

    (mapcar (lambda (a b) (- a b)) '(1 2 3 4) '(5 6 7 8))

Note however that unlike function literals seen in Clojure,
these can be nested freely, as in:

    ;double the squared value
    #f(* 2 (#f(* % %) %))

These also produce more legible output. The function above
will show up on a REPL like:

    #<FUNCTION :LAMBDA (#:[%]3235)
      (* 2 ((LAMBDA (#:[%]3234) (* #:[%]3234 #:[%]3234)) #:[%]3235))>

Note also that Let over Lambda's sharp backquote is a natural
consequence of this read macro as can be seen below. The only difference is the `f` after the `#` to prevent a conflict with Common Lisp's built in vector read-macro.

    (mapcar #f`(,% ,%) '(A B C))

yielding:

    ((A A) (B B) (C C))
#Important Compatibility Note

This macro is only compatible with SBCL and CLISP at the moment. If someone finds a way to make it more general, I would be eager to hear about it.

#License

This software falls under the WTFPL.
