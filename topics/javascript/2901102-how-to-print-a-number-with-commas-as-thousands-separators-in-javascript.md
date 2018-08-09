
# How to print a number with commas as thousands separators in JavaScript

## Question
        
I am trying to print an integer in [JavaScript](http://en.wikipedia.org/wiki/JavaScript) with commas as thousands separators. For example, I want to show the number 1234567 as "1,234,567". How would I go about doing this?

Here is how I am doing it:

    function numberWithCommas(x) {
        x = x.toString();
        var pattern = /(-?\d+)(\d{3})/;
        while (pattern.test(x))
            x = x.replace(pattern, "$1,$2");
        return x;
    }
    

Is there a simpler or more elegant way to do it? It would be nice if it works with floats also, but that is not necessary. It does not need to be locale-specific to decide between periods and commas.

## Answer
        
I used the idea from Kerry's answer, but simplified it since I was just looking for something simple for my specific purpose. Here is what I did:

    const numberWithCommas = (x) => {
      return x.toString().replace(/\B(?=(\d{3})+(?!\d))/g, ",");
    }
    

This is all you really need to know.

@Neils Bom asked how the regex works. My explanation is sort of long. It won't fit in the comments and I don't know where else to put it so I am doing it here. If anyone has any other suggestions for where to put it, please let me know.

The regex uses 2 lookahead assertions: a positive one to look for any point in the string that has a multiple of 3 digits in a row after it, and a negative assertion to make sure that point only has exactly a multiple of 3 digits. The replacement expression puts a comma there.

For example, if you pass it "123456789.01", the positive assertion will match every spot to the left of the 7 (since "789" is a multiple of 3 digits, "678" is a multiple of 3 digits, "567", etc.). The negative assertion checks that the multiple of 3 digits does not have any digits after it. "789" has a period after it so it is exactly a multiple of 3 digits, so a comma goes there. "678" is a multiple of 3 digits but it has a "9" after it, so those 3 digits are part of a group of 4, and a comma does not go there. Similarly for "567". "456789" is 6 digits, which is a multiple of 3, so a comma goes before that. "345678" is a multiple of 3, but it has a "9" after it, so no comma goes there. And so on. The "\\B" keeps the regex from putting a comma at the beginning of the string.

@neu-rah mentioned that this function adds commas in undesirable places if there are more than 3 digits after the decimal point. If this is a problem, you can use this function:

    const numberWithCommas = (x) => {
      var parts = x.toString().split(".");
      parts[0] = parts[0].replace(/\B(?=(\d{3})+(?!\d))/g, ",");
      return parts.join(".");
    }
