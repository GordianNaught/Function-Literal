Function-Literal
================

Clojure inspired Common Lisp function literal read macro

This file contains a read macro allowing for function
literals in Common Lisp similar to those in Clojure,
for example:

    (mapcar #(- % %2) '(1 2 3 4) '(5 6 7 8))

is equivalent to:

    (mapcar (lambda (a b) (- a b)) '(1 2 3 4) '(5 6 7 8))

Note however that unlike function literals seen in Clojure,
these can be nested freely, as in:

    ;double the squared value
    #(* 2 (#(* % %) %))

These also produce more legible output. The function above
will show up on a REPL like:

    #<FUNCTION :LAMBDA (#:[%]3235)
      (* 2 ((LAMBDA (#:[%]3234) (* #:[%]3234 #:[%]3234)) #:[%]3235))>

Note also that Let over Lambda's sharp backquote is a natural
consequence of this read macro as can be seen here:

    (mapcar #`(,% ,%) '(A B C))

yielding:

    ((A A) (B B) (C C))

#WARNING

The macro-character choice made in this macro DOES conflict with
a read macro in Common Lisp already using `#` as the macro character.

#License


This software falls under the Creative Commons Attribution 4.0 International License.
