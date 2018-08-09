
# Haskell lightweight threads overhead and use on multicores

## Question
        
I've been reading the "Real World Haskell" book, the chapter on concurrency and parallelism. My question is as follows:

*   Since Haskell threads are really just multiple "virtual" threads inside one "real" OS-thread, does this mean that creating a lot of them (like 1000) will not have a drastic impact on performance? I.e., can we say that the overhead incurred from creating a Haskell thread with `forkIO` is (almost) negligible? Please bring pactical examples if possible.
    
*   Doesn't the concept of lightweight threads prevent us from using the benefints of multicore architectures? As I understand, it is not possible for two Haskell threads to execute concurrently on two separate cores, because they are really one single thread from the operating system's point of view. Or does the Haskell runtime do some clever tricks to ensure that multiple CPU's can be made use of?

## Answer
        
GHC's runtime provides an execution environment supporting billions of sparks, thousands of lightweight threads, which may be distributed over multiple hardware cores. Compile with `-threaded` and use the `+RTS -N4` flags to set your desired number of cores.

![sparks/threads/workers/cores](https://i.stack.imgur.com/u53Uk.png)

Specifically:

> does this mean that creating a lot of them (like 1000) will not have a drastic impact on performance?

Well, [creating 1,000,000 of them](https://stackoverflow.com/questions/1900165/how-long-does-it-take-to-create-1-million-threads-in-haskell) is certainly possible. 1000 is so cheap it won't even show up. You can see in thread creation benchmarks, such as "thread ring" that [GHC is very, very good](http://benchmarksgame.alioth.debian.org/u64q/performance.php?test=threadring).

> Doesn't the concept of lightweight threads prevent us from using the benefints of multicore architectures?

Not at all. [GHC has been running on multicores](http://www.haskell.org/haskellwiki/GHC/Concurrency#Multicore_GHC) since 2004. The current status of the multicore runtime is [tracked here.](https://stackoverflow.com/questions/3063652/whats-the-status-of-multicore-programming-in-haskell)

How does it do it? The best place to read up on this architecture is in the paper, ["Runtime Support for Multicore Haskell"](http://community.haskell.org/~simonmar/papers/multicore-ghc.pdf):

> The GHC runtime system supports millions of lightweight threads by multiplexing them onto a handful of operating system threads, roughly one for each physical CPU. ...
> 
> Haskell threads are executed by a set of operating system threads, which we call worker threads. We maintain roughly one worker thread per physical CPU, but exactly which worker thread may vary from moment to moment ...
> 
> Since the worker thread may change, we maintain exactly one Haskell Execution Context (HEC) for each CPU. The HEC is a data structure that contains all the data that an OS worker thread requires in order to execute Haskell threads

You can monitor your threads being created, and where they're executing, [via threadscope.](http://research.microsoft.com/en-us/projects/threadscope/). Here, e.g. running the binary-trees benchmark:

![threadscope](https://i.stack.imgur.com/t82yH.png)
