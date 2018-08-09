
# What are the rules for JavaScript&apos;s automatic semicolon insertion (ASI)?

## Question
        
Well, first I should probably ask if this is browser dependent.

I've read that if an invalid token is found, but the section of code is valid until that invalid token, a semicolon is inserted before the token if it is preceded by a line break.

**However, the common example cited for bugs caused by semicolon insertion is:**

    return
      _a+b;
    

..which doesn't seem to follow this rule, since _a would be a valid token.

**On the other hand, breaking up call chains works as expected:**

    $('#myButton')
      .click(function(){alert("Hello!")});
    

Does anyone have a more in-depth description of the rules?

## Answer
        
First of all you should know which statements are affected by the automatic semicolon insertion (also known as ASI for brevity):

*   empty statement
*   `var` statement
*   expression statement
*   `do-while` statement
*   `continue` statement
*   `break` statement
*   `return` statement
*   `throw` statement

The concrete rules of ASI, are described in the specification [§11.9.1 Rules of Automatic Semicolon Insertion](http://www.ecma-international.org/ecma-262/7.0/index.html#sec-rules-of-automatic-semicolon-insertion)

Three cases are described:

1.  When a token (`LineTerminator` or `}`) is encountered that is not allowed by the grammar, a semicolon is inserted before it if:
    
    *   The token is separated from the previous token by at least one `LineTerminator`.
    *   The token is `}`
    
    _e.g._:
    
        { 1
        2 } 3
        
    
    is transformed to
    
        { 1
        ;2 ;} 3;
        
    
    The `NumericLiteral` `1` meets the first condition, the following token is a line terminator.  
    The `2` meets the second condition, the following token is `}`.
    
2.  When the end of the input stream of tokens is encountered and the parser is unable to parse the input token stream as a single complete Program, then a semicolon is automatically inserted at the end of the input stream.
    
    _e.g._:
    
        a = b
        ++c
        
    
    is transformed to:
    
        a = b;
        ++c;
        
    
3.  This case occurs when a token is allowed by some production of the grammar, but the production is a _restricted production_, a semicolon is automatically inserted before the restricted token.
    
    Restricted productions:
    
        UpdateExpression :
            LeftHandSideExpression [no LineTerminator here] ++
            LeftHandSideExpression [no LineTerminator here] --
        
        ContinueStatement :
            continue ;
            continue [no LineTerminator here] LabelIdentifier ;
        
        BreakStatement :
            break ;
            break [no LineTerminator here] LabelIdentifier ;
        
        ReturnStatement :
            return ;
            return [no LineTerminator here] Expression ;
        
        ThrowStatement :
            throw [no LineTerminator here] Expression ; 
        
        ArrowFunction :
            ArrowParameters [no LineTerminator here] => ConciseBody
        
        YieldExpression :
            yield [no LineTerminator here] * AssignmentExpression
            yield [no LineTerminator here] AssignmentExpression
        
    
    The classic example, with the `ReturnStatement`:
    
        return 
          "something";
        
    
    is transformed to
    
        return;
          "something";
