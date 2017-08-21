Text.Megaparsec.Expr is beautiful: It is only 60 lines of code (excluding comments). It allows you to create a nested expression parser with prefix, postfix and infix operators of varying associativity and precedence, in 12 lines (which you'll find at the bottom of the [page for it on Hackage](http://hackage.haskell.org/package/megaparsec-5.1.1/docs/Text-Megaparsec-Expr.html).)

The code in how-it-works/ contains simplifications that clarify exactly how it accomplishes certain tasks.

The code in what-it-does/ contains experiments I used to see how it treats various situations, such as when a flat expression contains operators of different precedence.
