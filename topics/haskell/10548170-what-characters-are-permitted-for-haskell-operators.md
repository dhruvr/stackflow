
# What characters are permitted for haskell operators?

## Question
        
Is there a complete list of allowed characters somewhere, or a rule that determines what can be used in an identifier vs an operator?

## Answer
        
What I was looking for was the complete list of characters. Based on the other answers, the full list is;

Unicode Punctuation:

*   [http://www.fileformat.info/info/unicode/category/Pc/list.htm](http://www.fileformat.info/info/unicode/category/Pc/list.htm)
*   [http://www.fileformat.info/info/unicode/category/Pd/list.htm](http://www.fileformat.info/info/unicode/category/Pd/list.htm)
*   [http://www.fileformat.info/info/unicode/category/Pe/list.htm](http://www.fileformat.info/info/unicode/category/Pe/list.htm)
*   [http://www.fileformat.info/info/unicode/category/Pf/list.htm](http://www.fileformat.info/info/unicode/category/Pf/list.htm)
*   [http://www.fileformat.info/info/unicode/category/Pi/list.htm](http://www.fileformat.info/info/unicode/category/Pi/list.htm)
*   [http://www.fileformat.info/info/unicode/category/Po/list.htm](http://www.fileformat.info/info/unicode/category/Po/list.htm)
*   [http://www.fileformat.info/info/unicode/category/Ps/list.htm](http://www.fileformat.info/info/unicode/category/Ps/list.htm)

Unicode Symbols:

*   [http://www.fileformat.info/info/unicode/category/Sc/list.htm](http://www.fileformat.info/info/unicode/category/Sc/list.htm)
*   [http://www.fileformat.info/info/unicode/category/Sk/list.htm](http://www.fileformat.info/info/unicode/category/Sk/list.htm)
*   [http://www.fileformat.info/info/unicode/category/Sm/list.htm](http://www.fileformat.info/info/unicode/category/Sm/list.htm)
*   [http://www.fileformat.info/info/unicode/category/So/list.htm](http://www.fileformat.info/info/unicode/category/So/list.htm)

But _excluding_ the following characters with special meaning in Haskell:

    (),;[]`{}_:"'
