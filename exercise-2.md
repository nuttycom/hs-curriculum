2. Higher-Order Functions
=========================

For the second exercise, we're going to learn a little more about function
arguments. We'll write functions that take multiple arguments, and learn that
we can pass functions *as* arguments.

~~~{haskell}

Prelude> let foo a b = a * (2 + b)

~~~

To invoke this function, simply provide two arguments to it,
separated by spaces.


~~~{haskell}

Prelude> foo 3 1
9

~~~

The fact that you can pass more than one argument Sometimes, you want to change
the behavior of a function so that it does different things depending upon what
has been passed to it. Try the following:

~~~{haskell}

Prelude> let hof f x = x + (f x)

~~~

Let's create a few functions that we can use to try it out:

~~~{haskell}

Prelude> let a x = 1 + x
Prelude> let m x = 2 * x

~~~

Now, for the fun part. Try this:

~~~{haskell}

Prelude> hof a 1
3
Prelude> hof m 2
6

~~~

Let's take a look at the type of our function 'hof'

~~~{haskell}

Prelude> :t hof
hof :: Num t => (t -> t) -> t -> t

~~~

You can always ask ghci to give you the type of an expression by using ':t' and
giving it the resulting expression. When you read these types, '->' means
'function. So the above says, given that 't' is numeric (the Num t part) hof is
a function that takes a a function from 't' to 't', then a 't', and finally
returns a 't'.

Something else you can do is to "partially apply" a function. This means that
you don't have to supply all the arguments to a function at once.  For example,
try taking the type of 'hof 2'

~~~{haskell}

Prelude> :t hof a
hof a :: Num t => t -> t

~~~

See how by providing one argument, it 'took away' the first 't -> t'? This is
because you filled in that argument, so all that's left is one more argument,
another 't'. You could give this a name if you wanted:

~~~{haskell}

Prelude> let ha = hof a

~~~

Then, you can use that function just as you did before.

~~~{haskell}

Prelude> ha 1
3

~~~




