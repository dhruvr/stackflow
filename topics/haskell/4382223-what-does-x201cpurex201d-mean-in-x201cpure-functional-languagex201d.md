
# What does &#x201C;pure&#x201D; mean in &#x201C;pure functional language&#x201D;?

## Question
        
Haskell has been called a "pure functional language."

What does "pure" mean in this context? What consequences does this have for a programmer?

## Answer
        
In a _pure_ functional language, you can't do anything that has a side effect.

A side effect would mean that evaluating an expression changes some internal state that would later cause evaluating the same expression to have a different result. In a pure functional language you can evaluate the same expression as often as you want with the same arguments, and it would always return the same value, because there is no state to change.

For example, a pure functional language cannot have an assignment operator or do input/output, although for practical purposes, even pure functional languages often call impure libraries to do I/O.
