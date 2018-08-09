
# Create GUID / UUID in JavaScript?

## Question
        
I'm trying to create globally-unique identifiers in JavaScript. I'm not sure what routines are available on all browsers, how "random" and seeded the built-in random number generator is, etc..

The GUID / UUID should be at least 32 characters and should stay in the ASCII range to avoid trouble when passing them around.

## Answer
        
There have been a couple attempts at this. The question is: do you want actual GUIDs, or just random numbers that _look_ like GUIDs? It's easy enough to generate random numbers.

    function guid() {
      function s4() {
        return Math.floor((1 + Math.random()) * 0x10000)
          .toString(16)
          .substring(1);
      }
      return s4() + s4() + '-' + s4() + '-' + s4() + '-' + s4() + '-' + s4() + s4() + s4();
    }
    

However, note that such values **are not genuine GUIDs**.

There's no way to generate real GUIDs in Javascript, because they depend on properties of the local computer that browsers do not expose. You'll need to use OS-specific services like ActiveX: [http://p2p.wrox.com/topicindex/20339.htm](http://p2p.wrox.com/topicindex/20339.htm)

Edit: not correct - RFC4122 allows random ("version 4") GUIDs. See other answers for specifics.

**Note**: the provided code snippet does not follow RFC4122 which requires that the version (`4`) has to be integrated into the generated output string. **Do not use this answer** if you need compliant GUIDs.

Use:

    var uuid = guid();
    

Demo:
=====

    function guid() {
      return s4() + s4() + '-' + s4() + '-' + s4() + '-' +
        s4() + '-' + s4() + s4() + s4();
    }
    
    function s4() {
      return Math.floor((1 + Math.random()) * 0x10000)
        .toString(16)
        .substring(1);
    }
    
    document.getElementById('jsGenId').addEventListener('click', function() {
      document.getElementById('jsIdResult').value = guid();
    })

    input { font-family: monospace; }

    <button id="jsGenId" type="button">Generate GUID</button>
    <br>
    <input id="jsIdResult" type="text" placeholder="Results will be placed here..." readonly size="40"/>
