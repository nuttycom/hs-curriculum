1. Defining A Simple Function
=============================

This project will introduce you to the Haskell interpreter.

At your terminal prompt, type the following:

~~~{bash}

stack ghci

~~~

This will give you a prompt that looks like this:

~~~{sh}

Configuring GHCi with the following packages:
GHCi, version 8.0.1: http://www.haskell.org/ghc/  :? for help
Loaded GHCi configuration from /tmp/ghci28029/ghci-script
Prelude> 

~~~

You're going to define your very first function. It's going to be named 'f',
and it's going to take a single argument, a number. That argument is going
to be named 'x'. In the interpreter, the keyword 'let' is used to define
this function

~~~{haskell}

Prelude> let f x = x + 1

~~~

You can now try out this function! When you type the name 'f', followed
by a number, then it will print out the result of the addition.

~~~{haskell}

Prelude> f 3
4
Prelude> f 4
5
Prelude> f (f 3)
5

~~~

Try using the function you've defined as part of a larger function named
'g'. This function will multiply the result of 'f' by two:

~~~{haskell}

Prelude> let g x = (f x) * 2

~~~

Play around with f and g a little bit, giving them different arguments.
See if you can predict what will happen!
