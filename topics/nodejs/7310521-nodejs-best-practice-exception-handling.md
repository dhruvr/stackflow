
# Node.js Best Practice Exception Handling

## Question
      
I just started trying out node.js a few days ago. I've realized that the Node is terminated whenever I have an unhandled exception in my program. This is different than the normal server container that I have been exposed to where only the Worker Thread dies when unhandled exceptions occur and the container would still be able to receive the request. This raises a few questions:

*   Is `process.on('uncaughtException')` the only effective way to guard against it?
*   Will `process.on('uncaughtException')` catch the unhandled exception during execution of asynchronous processes as well?
*   Is there a module that is already built (such as sending email or writing to a file) that I could leverage in the case of uncaught exceptions?

I would appreciate any pointer/article that would show me the common best practices for handling uncaught exceptions in node.js
## Answer
      
Update: Joyent now has [their own guide](https://www.joyent.com/node-js/production/design/errors) mentioned in [this answer](https://stackoverflow.com/a/23368579/130638). The following information is more of a summary:

Safely "throwing" errors
------------------------

Ideally we'd like to avoid uncaught errors as much as possible, as such, instead of literally throwing the error, we can instead safely "throw" the error using one of the following methods depending on our code architecture:

*   For synchronous code, if an error happens, return the error:
    
        // Define divider as a syncrhonous function
        var divideSync = function(x,y) {
            // if error condition?
            if ( y === 0 ) {
                // "throw" the error safely by returning it
                return new Error("Can't divide by zero")
            }
            else {
                // no error occured, continue on
                return x/y
            }
        }
        
        // Divide 4/2
        var result = divideSync(4,2)
        // did an error occur?
        if ( result instanceof Error ) {
            // handle the error safely
            console.log('4/2=err', result)
        }
        else {
            // no error occured, continue on
            console.log('4/2='+result)
        }
        
        // Divide 4/0
        result = divideSync(4,0)
        // did an error occur?
        if ( result instanceof Error ) {
            // handle the error safely
            console.log('4/0=err', result)
        }
        else {
            // no error occured, continue on
            console.log('4/0='+result)
        }
        
    
*   For callback-based (ie. asynchronous) code, the first argument of the callback is `err`, if an error happens `err` is the error, if an error doesn't happen then `err` is `null`. Any other arguments follow the `err` argument:
    
        var divide = function(x,y,next) {
            // if error condition?
            if ( y === 0 ) {
                // "throw" the error safely by calling the completion callback
                // with the first argument being the error
                next(new Error("Can't divide by zero"))
            }
            else {
                // no error occured, continue on
                next(null, x/y)
            }
        }
        
        divide(4,2,function(err,result){
            // did an error occur?
            if ( err ) {
                // handle the error safely
                console.log('4/2=err', err)
            }
            else {
                // no error occured, continue on
                console.log('4/2='+result)
            }
        })
        
        divide(4,0,function(err,result){
            // did an error occur?
            if ( err ) {
                // handle the error safely
                console.log('4/0=err', err)
            }
            else {
                // no error occured, continue on
                console.log('4/0='+result)
            }
        })
        
    
*   For [eventful](http://nodejs.org/api/events.html) code, where the error may happen anywhere, instead of throwing the error, fire the [`error` event instead](http://nodejs.org/api/events.html#events_class_events_eventemitter):
    
        // Definite our Divider Event Emitter
        var events = require('events')
        var Divider = function(){
            events.EventEmitter.call(this)
        }
        require('util').inherits(Divider, events.EventEmitter)
        
        // Add the divide function
        Divider.prototype.divide = function(x,y){
            // if error condition?
            if ( y === 0 ) {
                // "throw" the error safely by emitting it
                var err = new Error("Can't divide by zero")
                this.emit('error', err)
            }
            else {
                // no error occured, continue on
                this.emit('divided', x, y, x/y)
            }
        
            // Chain
            return this;
        }
        
        // Create our divider and listen for errors
        var divider = new Divider()
        divider.on('error', function(err){
            // handle the error safely
            console.log(err)
        })
        divider.on('divided', function(x,y,result){
            console.log(x+'/'+y+'='+result)
        })
        
        // Divide
        divider.divide(4,2).divide(4,0)
        
    

Safely "catching" errors
------------------------

Sometimes though, there may still be code that throws an error somewhere which can lead to an uncaught exception and a potential crash of our application if we don't catch it safely. Depending on our code architecture we can use one of the following methods to catch it:

*   When we know where the error is occurring, we can wrap that section in a [node.js domain](http://nodejs.org/api/domain.html)
    
        var d = require('domain').create()
        d.on('error', function(err){
            // handle the error safely
            console.log(err)
        })
        
        // catch the uncaught errors in this asynchronous or synchronous code block
        d.run(function(){
            // the asynchronous or synchronous code that we want to catch thrown errors on
            var err = new Error('example')
            throw err
        })
        
    
*   If we know where the error is occurring is synchronous code, and for whatever reason can't use domains (perhaps old version of node), we can use the try catch statement:
    
        // catch the uncaught errors in this synchronous code block
        // try catch statements only work on synchronous code
        try {
            // the synchronous code that we want to catch thrown errors on
            var err = new Error('example')
            throw err
        } catch (err) {
            // handle the error safely
            console.log(err)
        }
        
    
    However, be careful not to use `try...catch` in asynchronous code, as an asynchronously thrown error will not be caught:
    
        try {
            setTimeout(function(){
                var err = new Error('example')
                throw err
            }, 1000)
        }
        catch (err) {
            // Example error won't be caught here... crashing our app
            // hence the need for domains
        }
        
    
    Another thing to be careful about with `try...catch` is the risk of wrapping your completion callback inside the `try` statement like so:
    
        var divide = function(x,y,next) {
            // if error condition?
            if ( y === 0 ) {
                // "throw" the error safely by calling the completion callback
                // with the first argument being the error
                next(new Error("Can't divide by zero"))
            }
            else {
                // no error occured, continue on
                next(null, x/y)
            }
        }
        
        var continueElsewhere = function(err, result){
                throw new Error('elsewhere has failed')
        }
        
        try {
                divide(4, 2, continueElsewhere)
                // ^ the execution of divide, and the execution of 
                //   continueElsewhere will be inside the try statement
        }
        catch (err) {
                console.log(err.stack)
                // ^ will output the "unexpected" result of: elsewhere has failed
        }
        
    
    This gotcha is very easy to do as your code becomes more complex. As such, it is best to either use domains or to return errors to avoid (1) uncaught exceptions in asynchronous code (2) the try catch catching execution that you don't want it to. In languages that allow for proper threading instead of JavaScript's asynchronous event-machine style, this is less of an issue.
    
*   Finally, in the case where an uncaught error happens in a place that wasn't wrapped in a domain or a try catch statement, we can make our application not crash by using the `uncaughtException` listener (however doing so can put the application in an [unknown state](http://nodejs.org/api/process.html#process_event_uncaughtexception)):
    
        // catch the uncaught errors that weren't wrapped in a domain or try catch statement
        // do not use this in modules, but only in applications, as otherwise we could have multiple of these bound
        process.on('uncaughtException', function(err) {
            // handle the error safely
            console.log(err)
        })
        
        // the asynchronous or synchronous code that emits the otherwise uncaught error
        var err = new Error('example')
        throw err
    