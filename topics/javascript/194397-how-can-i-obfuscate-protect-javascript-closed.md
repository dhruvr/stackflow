
# How can I obfuscate (protect) JavaScript? [closed]

## Question
        
I want to make a JavaScript application that's not open source, and thus I wish to learn how to can obfuscate my JS code? Is this possible?

## Answer
        
**Obfuscation:**

Try [**YUI Compressor**](http://yuilibrary.com/). It's a very popular tool, built, enhanced and maintained by the Yahoo UI team.

You may also use:

*   [Google Closure Compiler](http://closure-compiler.appspot.com/home)
*   [UglifyJS](http://marijnhaverbeke.nl/uglifyjs)

**Private String Data:**

Keeping string values private is a different concern, and obfuscation won't really be of much benefit. Of course, by packaging up your source into a garbled, minified mess, you have a light version of **security** through **obscurity**. Most of the time, it's your user who is viewing the source, and the string values on the client are intended for their use, so that sort of private string value isn't often necessary.

If you really had a value that you never wanted a user to see, you would have a couple of options. First, you could do some kind of encryption, which is decrypted at page load. That would probably be one of the most secure options, but also a lot of work which may be unnecessary. You could probably base64 encode some string values, and that would be easier.. but someone who really wanted those string values could **easily decode them**. Encryption is the only way to truly prevent anyone from accessing your data, and most people find that to be more security than they need.

**Sidenote:**

Obfuscation in Javascript has been known to cause some bugs. The obfuscators are getting a little better about it, but many outfits decide that they see enough benefit from **minifying** and **gzipping**, and the added savings of obfuscation **isn't always worth the trouble**. If you're trying to protect your source, maybe you'll decide that it's worth your while, just to make your code harder to read. [**JSMin**](http://www.crockford.com/javascript/jsmin.html) is a good alternative.
