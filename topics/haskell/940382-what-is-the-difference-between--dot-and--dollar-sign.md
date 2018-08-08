
# What is the difference between . (dot) and $ (dollar sign)?

## Question
      
What is the difference between the dot `(.)` and the dollar sign `($)`?. As I understand it, they are both syntactic sugar for not needing to use parentheses.
## Answer
      
The `$` operator is for avoiding parentheses. Anything appearing after it will take precedence over anything that comes before.

For example, let's say you've got a line that reads:

    putStrLn (show (1 + 1))
    

If you want to get rid of those parentheses, any of the following lines would also do the same thing:

    putStrLn (show $ 1 + 1)
    putStrLn $ show (1 + 1)
    putStrLn $ show $ 1 + 1
    

The primary purpose of the `.` operator is not to avoid parentheses, but to chain functions. It lets you tie the output of whatever appears on the right to the input of whatever appears on the left. This usually also results in fewer parentheses, but works differently.

Going back to the same example:

    putStrLn (show (1 + 1))
    

1.  `(1 + 1)` doesn't have an input, and therefore cannot be used with the `.` operator.
2.  `show` can take an `Int` and return a `String`.
3.  `putStrLn` can take a `String` and return an `IO ()`.

You can chain `show` to `putStrLn` like this:

    (putStrLn . show) (1 + 1)
    

If that's too many parentheses for your liking, get rid of them with the `$` operator:

    putStrLn . show $ 1 + 1
    