
# Why is using the JavaScript eval function a bad idea?

## Question
        
The eval function is a powerful and easy way to dynamically generate code, so what are the caveats?

## Answer
        
1.  Improper use of **eval** opens up your code for injection attacks
    
2.  **Debugging** can be more challenging (no line numbers, etc.)
    
3.  eval'd code executes slower (no opportunity to compile/cache eval'd code)
    

Edit: As @Jeff Walden points out in comments, #3 is less true today than it was in 2008. However, while some caching of compiled scripts may happen this will only be limited to scripts that are eval'd repeated with no modification. A more likely scenario is that you are eval'ing scripts that have undergone slight modification each time and as such could not be cached. Let's just say that SOME eval'd code executes more slowly.
