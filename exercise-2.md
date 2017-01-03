2. Higher-Order Functions
=========================

For the second exercise, we're going to learn a little more
about function arguments. We'll write functions that take
multiple arguments, and learn that we can pass functions *as*
arguments.

~~~{haskell}

Prelude> let foo a b = a * (2 + b)

~~~

To invoke this function, simply provide two arguments to it,
separated by spaces.


~~~{haskell}

Prelude> foo 3 1
9

~~~

The fact that you can pass more than one argument Sometimes, you want
to change the behavior of a function so that it does different
things depending upon what has been passed to it. Try the following:

~~~{haskell}

Prelude> let hof x f = x + (f x)

~~~

Let's create a few functions that we can use to try it out:

~~~{haskell}

Prelude> let addone x = 1 + x
Prelude> let multwo x = 2 * x
Prelude> let div3 x = x / 3
Prelude> let sub4 x = x - 4

~~~

Now, for the fun part. Try this:

~~~{haskell}

Prelude> hof 2 addone
5
Prelude> hof 2 multwo
6

~~~

Let's take a look at the type of our function 'hof'

~~~{haskell}

Prelude> :t hof
hof :: Num t => t -> (t -> t) -> t

~~~

You can always ask ghci to give you the type of an expression
by using ':t' and giving it the resulting expression. When you read
these types, '->' means 'function. So the above says, given that
't' is numeric (the Num t part) hof is a function that takes a 't',
then a function from 't' to 't', and finally returns a 't'.

Something else you can do is to "partially apply" a function. This means
that you don't have to supply all the arguments to a function at once.
For example, try taking the type of 'hof 2'

~~~{haskell}

Prelude> :t hof 2
hof 2 :: Num t => (t -> t) -> t

~~~

See how by providing one argument, it 'took away' the first 't'? This
is because you filled in that argument, so all that's left is one more
argument, a function from 't' to 't'. You could give this a name if you wanted:

~~~{haskell}

Prelude> let hoftwo = hof 2

~~~

Then, you can use that function just as you did before.

~~~{haskell}

Prelude> hoftwo addone
5
Prelude> hoftwo multwo
6

~~~




