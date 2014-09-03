Function-Literal
================

Clojure inspired Common Lisp function literal read macro

This file contains a read macro allowing for function
literals in Common Lisp similar to those in Clojure,
for example: `(mapcar #(* % 2) '(1 2 3 4))`

Note however that unlike function literals seen in Clojure,
these can be nested freely, as in:

`;double the squared value`

`#(* 2 (#(* % %) %))`

These also produce more legible output. The function above
will show up on a REPL like:

`#<FUNCTION :LAMBDA (#:[%]3235)`

`  (* 2 ((LAMBDA (#:[%]3234) (* #:[%]3234 #:[%]3234)) #:[%]3235))>`

This software falls under the Creative Commons Attribution 4.0 International License.
