
# Why is there &#x201C;data&#x201D; and &#x201C;newtype&#x201D; in Haskell? [duplicate]

## Question
        
This question already has an answer here:

*   [Difference between \`data\` and \`newtype\` in Haskell](/questions/5889696/difference-between-data-and-newtype-in-haskell) 1 answer

It seems that a `newtype` definition is just a `data` definition that obeys some restrictions (e.g., only one constructor), and that due to these restrictions the runtime system can handle `newtype`s more efficiently. And the handling of pattern matching for undefined values is slightly different.

But suppose Haskell would only knew `data` definitions, no `newtype`s: couldn't the compiler find out for itself whether a given data definition obeys these restrictions, and automatically treat it more efficiently?

I'm sure I'm missing out on something, there must be some deeper reason for this.

## Answer
        
Both `newtype` and the single-constructor `data` introduce a single value constructor, but the value constructor introduced by `newtype` is strict and the value constructor introduced by `data` is lazy. So if you have

    data D = D Int
    newtype N = N Int
    

Then `N undefined` is equivalent to `undefined` and causes an error when evaluated. But `D undefined` is _not_ equivalent to `undefined`, and it can be evaluated as long as you don't try to peek inside.

> Couldn't the compiler handle this for itself.

No, not really—this is a case where as the programmer you get to decide whether the constructor is strict or lazy. To understand when and how to make constructors strict or lazy, you have to have a much better understanding of lazy evaluation than I do. I stick to the idea in the Report, namely that `newtype` is there for you to rename an existing type, like having several different incompatible kinds of measurements:

    newtype Feet = Feet Double
    newtype Cm   = Cm   Double
    

both behave exactly like `Double` at run time, but the compiler promises not to let you confuse them.
