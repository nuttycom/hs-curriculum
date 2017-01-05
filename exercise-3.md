3. Printing Output & Repeating
==============================

For the third exercise, we're going to start learning a little
bit about how to include printing output into our function. Up
until now, we've been relying upon the ghci interpreter to 
directly print the output of our function as soon as it has
been evaluated, but sometimes we want to control that printing
more directly. Type the following into your interpreter.

~~~{haskell}

Prelude> let printPotato = putStrLn "potato"

~~~

If you run `printPotato` it will print the word "potato" to the screen.

~~~{haskell}

Prelude> printPotato
potato

~~~

For practice, we're going write out a little program to perform a countdown - a
program that starts starting at a number you specify, and then counts down to
zero. Type the following in exactly as you see it:

The ':{' and ':}' digraphs allow you to write a multi-line expression.

~~~{haskell}

Prelude> :{
Prelude| let countdown i = do
Prelude|     putStrLn (show i)
Prelude|     if (i > 0) then countdown (i - 1)
Prelude|                else pure () 
Prelude| :}

~~~

Try it out: 

~~~{haskell}

Prelude> countdown 10

~~~

There's a lot that's happening here, so we'll take it a bit at a time.

First, `putStrLn`. This prints a string (what we call a word or series of
words) to the screen. `putStrLn` is a function that takes a String value as its
argument, and returns a value of type `IO ()` which is a command to the
computer to do the printing.

Second, `(show i)`. `show` is a function that can turn a number into the string
that `putStrLn` is going to print.

Finally, the `if`-`then`-`else` expression will check whether the expression
`(i > 0)` is true. If `i` is a number greater than zero, we're not done
counting down, so we return the command to count down from the next lowest
number. We do this by calling the `countdown` function that we're in the
process of defining!  Since we want each step of the program to print a number
that's one less than the previous number, we write `countdown (i - 1)`.  If, on
the other hand, we want to be done counting down, because `i` has reached zero,
we need to return a command that does nothing.  `pure ()` will give that
command.



