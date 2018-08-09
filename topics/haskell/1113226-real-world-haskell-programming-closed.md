
# Real world Haskell programming [closed]

## Question
        
Having been an imperative developer for some years now, I had never had the urge to learn functional programming.

A couple months ago at last I decided to learn Haskell. It's quite a cool language, but I'm puzzled about how an event driven real app would be programmed in such a language. Do you know of a good tutorial about it?

**Note:** When I say "real app" I'm not talking of a real world, production ready app. I just mean a little sample app, just to get the grasp of it. I think something like a simplified version of the windows caculator would be great, and then perhaps something a bit more complex.

## Answer
        
When you say "real world" examples you are presumably thinking about problems that are inherently sequential or stateful or do lots of I/O, right?

So, how about games?

*   [Frag](http://Haskell.Org/haskellwiki/Frag) is a Quake clone, implemented for an undergraduate thesis ([Functional Programming and 3D Games, Mun Hon Cheong, 2005](http://WWW.CSE.UNSW.Edu.Au/~pls/thesis/munc-thesis.pdf)). [Here's a video of it in action](http://www.youtube.com/watch?v=0jYdu2u8gAU).
*   [Super Monao Bros.](https://GitHub.Com/Mokehehe/Monao/) (formerly known as _Super Nario Bros._) is, well, you can probably figure out which game it is a clone of. ([This is the author's English language weblog.](http://Mokehehe.BlogSpot.Com/))
*   [Purely Functional Retrogames](http://Prog21.Dadgum.Com/23.html) is a 4-part series of blog articles about how to write games in a purely functional language, explained using Pacman as the example. ([Part 2](http://Prog21.Dadgum.Com/24.html), [Part 3](http://Prog21.Dadgum.Com/25.html), [Part 4](http://Prog21.Dadgum.Com/26.html).)

Or, what about an [X Window Manager](http://XMonad.Org/), an extensible [Emacs clone text editor](http://Haskell.Org/haskellwiki/Yi) or an [IDE](http://Leksah.Org/)?

Then, there is the book, which even has your question already in the title: [Real World Haskell](http://Book.RealWorldHaskell.Org/) _and_ which is also [available for free](http://Book.RealWorldHaskell.Org/read/)!

Another thing you might want to look at, is [Functional Reactive Programming](http://Google.Com/search?q=Functional+Reactive+Programming). (It is used in Frag, for example.) The interesting thing about FRP is that it allows you to look at the problem of, say, GUI programming from a very different angle. If you read the GUI chapter in the RWH book, you will see that it talks about how you can write a GUI application just like in C, only better. FRP OTOH allows you to write it in a _totally different_ way that wouldn't even be _possible_ in C.

A lot of times (I'm not saying that this is the case in your question, but it is a recurring pattern) when someone says "but can Haskell be used in the real world", what they are _really_ saying is "I know how to do this in C, and in Haskell I cannot do it in exactly the same way, therefore it must be impossible in Haskell, therefore Haskell is not ready for the real world". But what they are missing out on, is that there might be a totally different and _much better_ way to solve the problem. (It's like saying "Erlang doesn't have threads, therefore it cannot possibly be used to implement concurrent systems.") And FRP is just one example.
