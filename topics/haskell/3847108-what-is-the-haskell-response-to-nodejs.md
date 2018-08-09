
# What is the Haskell response to Node.js?

## Question
        
I believe the Erlang community is not envious of Node.js as it does non-blocking I/O natively and has ways to scale deployments easily to more than one processor (something not even built-in in Node.js). More details at [http://journal.dedasys.com/2010/04/29/erlang-vs-node-js](http://journal.dedasys.com/2010/04/29/erlang-vs-node-js) and [Node.js or Erlang](https://stackoverflow.com/questions/3011317/node-js-or-erlang)

What about Haskell? Can Haskell provide some of the benefits of Node.js, namely a clean solution to avoid blocking I/O without having recourse to multi-thread programming?

* * *

There are many things that are attractive with Node.js

1.  Events: No thread manipulation, the programmer only provides callbacks (as in Snap framework)
2.  Callbacks are guaranteed to be run in a single thread: no race condition possible.
3.  Nice and simple UNIX-friendly API. Bonus: Excellent HTTP support. DNS also available.
4.  Every I/O is by default asynchronous. This makes it easier to avoid locks. However, too much CPU processing in a callback will impact other connections (in this case, the task should split into smaller sub-tasks and re-scheduled).
5.  Same language for client-side and server-side. (I don't see too much value in this one, however. jQuery and Node.js share the event programming model but the rest is very different. I just can't see how sharing code between server-side and client-side could be useful in practice.)
6.  All this packaged in a single product.

## Answer
        
Ok, so having watched a little of the [node.js presentation](http://www.youtube.com/watch?v=F6k8lTrAE2g) that @gawi pointed me at, I can say a bit more about how Haskell compares to node.js. In the presentation, Ryan describes some of the benefits of Green Threads, but then goes on to say that he doesn't find the lack of a thread abstraction to be a disadvantage. I'd disagree with his position, particularly in the context of Haskell: I think the abstractions that threads provide are essential for making server code easier to get right, and more robust. In particular:

*   using one thread per connection lets you write code that expresses the communication with a single client, rather that writing code that deals with _all_ the clients at the same time. Think of it like this: a server that handles multiple clients with threads looks almost the same as one that handles a single client; the main difference is there's a `fork` somewhere in the former. If the protocol you're implementing is at all complex, managing the state machine for multiple clients simultaneously gets quite tricky, whereas threads let you just script the communication with a single client. The code is easier to get right, and easier to understand and maintain.
    
*   callbacks on a single OS thread is cooperative multitasking, as opposed to preemptive multitasking, which is what you get with threads. The main disadvantage with cooperative multitasking is that the programmer is responsible for making sure that there's no starvation. It loses modularity: make a mistake in one place, and it can screw up the whole system. This is really something you don't want to have to worry about, and preemption is the simple solution. Moreover, communication between callbacks isn't possible (it would deadlock).
    
*   concurrency isn't hard in Haskell, because most code is pure and so is thread-safe by construction. There are simple communication primitives. It's much harder to shoot yourself in the foot with concurrency in Haskell than in a language with unrestricted side effects.
