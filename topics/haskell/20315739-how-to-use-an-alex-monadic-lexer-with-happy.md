
# How to use an Alex monadic lexer with Happy?

## Question
        
I'm trying to learn using Alex + Happy to build parser, in particular I'm interested in learning to use the `monad` wrapper of Alex. I have already looked at the documentation of Alex and [Happy](http://www.haskell.org/happy/doc/html/sec-monads.html#sec-lexers) but I they are both, for me, _really_ lacking any useful information on using them together. I managed to make them work together with the `basic` and `posn` wrappers, but I'm at a loss with `monad`.

I have already looked at different question on SO about Alex, Happy and monadic lexers (including: [Are there any tutorials on building a simple interpreter using Alex + Happy?](https://stackoverflow.com/questions/3113197/are-there-any-tutorials-on-building-a-simple-interpreter-using-alex-happy) but none is able to provide a simple example where `monad` is used.

Most of the code online uses Happy with a custom lexer function, or uses the `basic` or `posn` Alex wrappers.

Here's a simple lexer for an ini-like syntax:

    {
    module IniLexer where
    }
    
    %wrapper "monad"
    
    
    
    $spaces = [\ \t]
    $alpha = [a-zA-Z]
    $digits = [0-9]
    $alnum = [$alpha$digits]
    
    
    @identifier = $alpha $alnum*
    
    @comment = \#.*
    
    @integer = $digits+
    
    @boolean = (true) | (false)
    
    @string = \"[^\"]*\"
    
    
    :-
    
    @integer    { mkL LInteger }
    @boolean    { mkL LBoolean }
    @string     { mkL LString }
    
    @identifier  { mkL LIdentifier }
    
    \[@identifier\] { mkL LSection }
    
    =           { mkL LAssign }
    
    \;          { mkL LEndAssign }
    @comment    ;
    [\ \t \n]+  ;
    
    
    {
    
    data LexemeClass = LInteger | LBoolean | LString | LIdentifier | LSection | LAssign | LEndAssign | LEOF
        deriving (Eq, Show)
    
    
    mkL :: LexemeClass -> AlexInput -> Int -> Alex Token
    mkL c (p, _, _, str) len = let t = take len str
                               in case c of
                                    LInteger -> return (IntegerNum ((read t) :: Integer) p)
                                    LBoolean -> return (BooleanVal (if t == "true"
                                                                       then True
                                                                       else False
                                                                   ) p)
                                    LString -> return (StringTxt (take (length t - 2) (drop 1 t)) p)
                                    LIdentifier -> return (Identifier t p)
                                    LSection -> return (SectionHeader (take (length t - 2) (drop 1 t)) p)
                                    LAssign -> return (Assignment p)
                                    LEndAssign -> return (EndAssignment p)
    
    
    -- No idea why I have to write this myself. Documentation doesn't mention it.
    alexEOF :: Alex Token
    alexEOF = return Eof
    
    
    
    data Token = SectionHeader {identifier :: String, position :: AlexPosn} |
                 Identifier {name :: String, position :: AlexPosn}          |
                 Assignment {position :: AlexPosn}                          |
                 EndAssignment {position :: AlexPosn}                       |
                 IntegerNum {value :: Integer, position :: AlexPosn}        |
                 BooleanVal {istrue :: Bool, position :: AlexPosn}          |
                 StringTxt  {text :: String, position :: AlexPosn}          |
                 Eof
        deriving (Eq, Show)
    
    
    }
    

And here's the relative Happy parser:

    {
    module Main where
    
    import IniLexer
    
    }
    
    
    
    %name parseIniFile
    %error {parseError}
    %lexer  {alexMonadScan} {AlexEOF}
    %monad {Alex}
    %tokentype {Token}
    %token
        SECTION     {SectionHeader name _ }
        IDENT       {Identifier name _ }
        '='         {Assignment _ }
        INT         {IntegerNum value _ }
        BOOL        {BooleanVal istrue _ }
        STRING      {StringTxt text _ }
        ';'         {EndAssignment _ }
    
    
    %%
    
    
    ConfigFile : SequenceOfSections                    {reverse $1}
    
    SequenceOfSections : {- empty -}                   {   []  }
                       | SequenceOfSections Section    {$2 : $1}
    
    
    Section : SECTION SectionBody                      {Section (identifier $1) (reverse $2)}
    
    
    SectionBody : {- empty -}        {[]}
                | SectionBody AssignmentLine ';' {$2 : $1}
    
    
    AssignmentLine : IDENT '=' Value      {(name $1, $3)}
    
    Value : INT         {IntV (value $1)}
          | BOOL        {BoolV (istrue $1)}
          | STRING      {StringV (text $1)}
    
    
    {
    
    data Value = IntV Integer | BoolV Bool | StringV String
        deriving (Eq, Show)
    
    data Section = Section String [(String, Value)]
        deriving (Eq, Show)
    
    data IniFile = IniFile [Section]
        deriving (Eq, Show)
    
    
    parseError :: [Token] -> Alex a
    parseError t = fail "a"
    
    main = do
        s <- getContents
        print $ parseIniFile $ runAlex s alexMonadScan
    
    }
    

Which raises a lot of compiler errors:

    [...]
    Couldn't match expected type `(AlexReturn t1 -> Alex a0) -> t0'
                    with actual type `Alex Token'
        The function `alexMonadScan' is applied to one argument,
        but its type `Alex Token' has none
    [...]
    

How should I modify the parser to use `alexMonadScan`? The [Happy](http://www.haskell.org/happy/doc/html/sec-monads.html#sec-lexers) documentation isn't clear at all and tries hard _not_ to use any clarifying example (or the examples provided fail in clarying from my point of view).

If needed I could post my `posn` version of this same lexer+parser.

## Answer
        
Your lexer's definition is completely fine as far as I can tell. Assuming there are no bugs there, the only problems you need to fix are in your parser's configuration. The first thing is that the lexer you are using is the wrong one. While that function is the interface to the Alex lexer, it has the type

    alexMonadScan :: Alex result
    

But the lexer Happy wants is of type

    lexer :: (Token -> P a) -> P a
    

Where `P` is the monad we are using. What this is saying is that the lexer should provide us an `Alex a` when given a continuation. A simple wrapper is what we need here:

    lexwrap :: (Token -> Alex a) -> Alex a
    lexwrap cont = do
        token <- alexMonadScan
        cont token
    

or equivalently

    lexwrap = (alexMonadScan >>=)
    

Second, using `alexEOF` in the `%lexer` directive will cause your parser to fail on every input. The name you supply there is inserted in to a branch of a case statement in generated code, so you must use the name of a data constructor rather than a value --- in particular, you need to use the data constructor that Alex will emit to signal EOF.

This makes our lexer line in the parser a little different.

    %lexer {lexwrap} {Eof}
    

(As a side note, _this_ is the reason that you need to write `alexEOF = return Eof` yourself. The data constructor you return inside `alexEOF` needs to pattern-match against the data constructor you identify to Happy as the one that ends the file. Alex has no way of knowing what you want to emit, and Happy has no way of knowing what you chose to emit via Alex.)

Now the next problem is that your parseError's type is incorrect. When using just a monad, that is indeed the type you need, but when you add a lexer into the mix, your parseError must have a different type. Also, using fail is probably not advised, so here is a slightly better definition:

    parseError :: Token -> Alex a
    parseError _ = alexError "Why is using happy and alex so hard"
    

Finally, the main function is definied a little strange here. what we want to do to call the parser is to invoke it with runAlex. So here is a quick wrapper for it. The string passed in is the string that you wish to parse.

    parse :: String -> Either String [Section]
    parse s = runAlex s parseIniFile
    

The type of the function parse is determined by the parseIniFile's definition. Here, it is an `Alex [Section]` so an `Either String [Section]` is returned.

I think that's everything.
