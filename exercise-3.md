3. Printing Output & Repeating
==============================

For the third exercise, we're going to start learning a little
bit about how to include printing output into our function. Up
until now, we've been relying upon the ghci interpreter to 
directly print the output of our function as soon as it has
been evaluated, but sometimes we want to control that printing
more directly. Type the following into your interpreter.

~~~{haskell}

Prelude> let printIt i = putStrLn (show i)

~~~

If you run 'printIt 3' it will print the number 3 to the screen.
This time, rather than letting the interpreter print for you,
you're controlling the printing yourself.

~~~{haskell}

Prelude> printIt 3
3

~~~

At this point, it's kind of difficult to tell that there's anything
different going on, so let's change things a little bit. Instead of
just printing out one number, we're going to print out a countdown,
starting at the number you specify, and counting down to zero. Type
the following in exactly as you see it:

The ':{' and ':}' digraphs allow you to write a multi-line expression.

~~~{haskell}

Prelude> :{
Prelude| let countdown i = do
Prelude|     putStrLn $ show i
Prelude|     if (i == 0) then pure () 
Prelude|                 else countdown (i - 1)
Prelude| :}

~~~

Try it out: 

~~~{haskell}

Prelude> countdown 10

~~~

There's a lot that's happening here, so we'll take it a bit at a time.
