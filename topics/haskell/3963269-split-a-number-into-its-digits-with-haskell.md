
# Split a number into its digits with Haskell

## Question
        
Given an arbitrary number, how can I process each digit of the number individually?

**Edit** I've added a basic example of the kind of thing `Foo` might do.

For example, in C# I might do something like this:

    static void Main(string[] args)
    {
        int number = 1234567890;
        string numberAsString = number.ToString();
    
        foreach(char x in numberAsString)
        {
            string y = x.ToString();
            int z = int.Parse(y);
            Foo(z);
        }
    }
    
    void Foo(int n)
    {
        Console.WriteLine(n*n);
    }

## Answer
        
Have you heard of [div and mod](http://www.haskell.org/tutorial/numbers.html)?

You'll probably want to reverse the list of numbers if you want to treat the most significant digit first. Converting the number into a string is an impaired way of doing things.

    135 `div` 10 = 13
    135 `mod` 10 = 5
    

Generalize into a function:

    digs :: Integral x => x -> [x]
    digs 0 = []
    digs x = digs (x `div` 10) ++ [x `mod` 10]
    

Or in reverse:

    digs :: Integral x => x -> [x]
    digs 0 = []
    digs x = x `mod` 10 : digs (x `div` 10)
    

This treats `0` as having no digits. A simple wrapper function can deal with that special case if you want to.

Note that this solution does not work for negative numbers (the input `x` must be integral, i.e. a whole number).
