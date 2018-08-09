
# Using Maybe type in Haskell

## Question
        
I'm trying to utilize the Maybe type in Haskell. I have a lookup for key, value tuples that returns a Maybe. How do I access the data that was wrapped by Maybe? For example I want to add the integer contained by Maybe with another integer.

## Answer
        
Alternatively you can pattern match:

    case maybeValue of
      Just value -> ...
      Nothing    -> ...
