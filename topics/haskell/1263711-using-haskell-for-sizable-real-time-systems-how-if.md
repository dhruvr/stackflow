
# Using Haskell for sizable real-time systems: how (if?)?

## Question
        
I've been curious to understand if it is possible to apply the power of Haskell to embedded realtime world, and in googling have found the [Atom](http://hackage.haskell.org/package/atom) package. I'd assume that in the complex case the code might have all the classical C bugs - crashes, memory corruptions, etc, which would then need to be traced to the original Haskell code that caused them. So, this is the first part of the question: "If you had the experience with Atom, how did you deal with the task of debugging the low-level bugs in compiled C code and fixing them in Haskell original code ?"

I searched for some more examples for Atom, [this blog post](http://archhaskell.wordpress.com/2009/08/01/atom-a-domain-specific-language-for-hard-realtime-applications/) mentions the resulting C code 22KLOC (and obviously no code:), the [included example](http://hackage.haskell.org/packages/archive/atom/0.1.0/doc/html/src/Language-Atom-Example.html#example) is a toy. [This](http://leepike.wordpress.com/source-code/atomfibhs/) and [this](http://blog.sw17ch.com/wordpress/?p=111) references have a bit more practical code, but this is where this ends. And the reason I put "sizable" in the subject is, I'm most interested if you might share your experiences of working with the generated C code in the range of 300KLOC+.

As I am a Haskell newbie, obviously there may be other ways that I did not find due to my unknown unknowns, so any other pointers for self-education in this area would be greatly appreciated - and this is the second part of the question - "what would be some other practical methods (if) of doing real-time development in Haskell?". If the multicore is also in the picture, that's an extra plus :-)

(About usage of Haskell itself for this purpose: from what I read in [this blog post](http://mikeburrell.wordpress.com/2007/02/01/real-time-haskell/), the garbage collection and laziness in Haskell makes it rather nondeterministic scheduling-wise, but maybe in two years something has changed. [Real world Haskell programming](https://stackoverflow.com/questions/1113226/real-world-haskell-programming) question on SO was the closest that I could find to this topic)

**Note:** "real-time" above is would be closer to "hard realtime" - I'm curious if it is possible to ensure that the pause time when the main task is not executing is under 0.5ms.

## Answer
        
At Galois we use Haskell for two things:

*   Soft real time (OS device layers, networking), where 1-5 ms response times are plausible. GHC generates fast code, and has plenty of support for tuning the GC and scheduler to get the right timings.
*   for true real time systems EDSLs are used to generate code for other languages that provide stronger timing guarantees. E.g. Cryptol, Atom and Copilot.

So be careful to distinguish the EDSL (Copilot or Atom) from the host language (Haskell).

* * *

Some examples of critical systems, and in some cases, real-time systems, either written or generated from Haskell, produced by Galois.

**EDSLs**

*   [Copilot: A Hard Real-Time Runtime Monitor](http://www.cs.indiana.edu/~lepike/pub_pages/rv2010.html) \-\- an DSL for real-time avionics monitoring
*   [Equivalence and Safety Checking in Cryptol](http://corp.galois.com/blog/2009/2/5/equivalence-and-safety-checking-in-cryptol.html) \-\- a DSL for cryptographic components of critical systems

**Systems**

*   [HaLVM](https://galois.com/project/halvm/) \-\- a lightweight microkernel for embedded and mobile applications
*   [TSE](http://corp.galois.com/trusted-services-engine-tse/) \-\- a cross-domain (security level) network appliance
