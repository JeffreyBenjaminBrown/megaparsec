Text.Megaparsec.Expr is beautiful: It is only 60 lines of code (excluding comments). In 12 lines of code, it lets you write something that can evaluate expressions like "(3 + 4/(-2)) * 7" -- a nested expression parser, with prefix, postfix and infix operators of varying associativity and precedence, in 12 lines (which you'll find at the bottom of the [page for it on Hackage](http://hackage.haskell.org/package/megaparsec-5.1.1/docs/Text-Megaparsec-Expr.html).)

[how-it-works](how-it-works) contains simplifications that clarify exactly how it accomplishes certain tasks.

[what-it-does](what-it-does) contains experiments I used to see how it treats various situations, such as when a flat expression contains operators of different precedence.

[outside-lessons](outside-lessons) contains lessons I learned that are not specific to Text.Megaparsec.Expr.
