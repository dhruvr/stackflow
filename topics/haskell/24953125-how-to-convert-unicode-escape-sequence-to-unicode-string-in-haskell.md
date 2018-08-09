
# How to convert Unicode Escape Sequence to Unicode String in Haskell

## Question
        
I have a string like `"\3619\3657\3634\3609\3648\3592\3657\3648\3621\3657\3591"` which I want to decode it. I tried search the unicode library without success.

## Answer
        
> Prelude> putStrLn "\\3619\\3657\\3634\\3609\\3648\\3592\\3657\\3648\\3621\\3657\\3591"  
> ร้านเจ้เล้ง

Note that you don't actually have the string `"\3619\3657\3634\3609\3648\3592\3657\3648\3621\3657\3591"` – rather, you have the UTF-32 string `ร้านเจ้เล้ง`, for which `"\3619\3657..."` happens to be a ASCII-compliant literal. By default, GHCi uses the `Show` instance to display results, which doesn't so much show things as _spit out literals that can be used as Haskell code for the thing_. It's conservative in terms of unicode. That's why

> Prelude> "ร้านเจ้เล้ง"  
> "\\3619\\3657\\3634\\3609\\3648\\3592\\3657\\3648\\3621\\3657\\3591"

On the other hand, the `putStrLn`, `putChar`, `hPutStr` etc. functions will just dump the string itself in UTF-8 rather than an ASCII-safe representation thereof.

If you're actually reading the escaped string from a file or something, you can simply _read_ it:

> Prelude> s <‌- getLine  
> "\\3619\\3657\\3634\\3609\\3648\\3592\\3657\\3648\\3621\\3657\\3591"  
> Prelude> s  
> "\\"\\\3619\\\3657\\\3634\\\3609\\\3648\\\3592\\\3657\\\3648\\\3621\\\3657\\\3591\\""  
> \-\- Note double escaping, because I'm showing a string that contains a string literal.  
> Prelude> putStrLn $ read s  
> ร้านเจ้เล้ง
