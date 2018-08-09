
# Orphaned instances in Haskell

## Question
        
When compiling my Haskell application with the `-Wall` option, GHC complains about orphaned instances, for example:

    Publisher.hs:45:9:
        Warning: orphan instance: instance ToSElem Result
    

The type class `ToSElem` is not mine, it's defined by [HStringTemplate](http://www.haskell.org/haskellwiki/HStringTemplate).

Now I know how to fix this (move the instance declaration into the module where Result is declared), and I know [why GHC would prefer to avoid orphaned instances](http://www.haskell.org/ghc/docs/6.10.3/html/users_guide/separate-compilation.html#orphan-modules), but I still believe that my way is better. I don't care if the compiler is inconvenienced - rather it than me.

The reason I want to declare my `ToSElem` instances in the Publisher module is because it is the Publisher module that depends on HStringTemplate, not the other modules. I am trying to maintain a separation of concerns and avoid having every module depend on HStringTemplate.

I thought that one of the advantages of Haskell's type classes, when compared for example to Java's interfaces, is that they are open rather than closed and therefore the instances do not have to be declared in the same place as the data type. GHC's advice seems to be to ignore this.

So, what I'm looking for is either some validation that my thinking is sound and that I would be justified in ignoring/suppressing this warning, or a more convincing argument against doing things my way.

## Answer
        
I understand why you want to do this, but unfortunately, it may be only an illusion that Haskell classes seem to be "open" in the way that you say. Many people feel that the possibility of doing this is a bug in the Haskell specification, for reasons I'll explain below. Anyway, if it is really not appropriate for the instance you need to be declared either in the module where the class is declared or in the module where the type is declared, that is probably a sign that you should be using a `newtype` or some other wrapper around your type.

The reasons why orphan instances need to be avoided run far deeper than convenience of the compiler. This topic is rather controversial, as you can see from other answers. To balance the discussion, I am going to explain the point of view that one should never, ever, write orphan instances, which I think is the majority opinion among experienced Haskellers. My own opinion is somewhere in the middle, which I'll explain at the end.

The problem stems from the fact that when more than one instance declaration exists for the same class and type, there is no mechanism in standard Haskell to specify which to use. Rather, the program is rejected by the compiler.

The simplest effect of that is that you could have a perfectly working program that would suddenly stop compiling because of a change someone else makes in some far off dependency of your module.

Even worse, it's possible for a working program to start **crashing at runtime** because of a distant change. You could be using a method that you are assuming comes from a certain instance declaration, and it could silently be replaced by a different instance that is just different enough to cause your program to start inexplicably crashing.

People who want guarantees that these problems won't ever happen to them must follow the rule that if anyone, anywhere, has ever declared an instance of a certain class for a certain type, no other instance must ever be declared again in any program written by anyone. Of course, there is the workaround of using a `newtype` to declare a new instance, but that is always at least a minor inconvenience, and sometimes a major one. So in this sense, those who write orphan instances intentionally are being rather impolite.

So what should be done about this problem? The anti-orphan-instance camp says that the GHC warning is a bug, it needs to be an error that rejects any attempt to declare an orphan instance. In the meantime, we must exercise self-discipline and avoid them at all costs.

As you have seen, there are those who are not so worried about those potential problems. They actually encourage the use of orphan instances as a tool for separation of concerns, as you suggest, and say that one should just make sure on a case-by-case basis that there is no problem. I have been inconvenienced enough times by other people's orphan instances to be convinced that this attitude is too cavalier.

I think the right solution would be to add an extension to Haskell's import mechanism that would control the import of instances. That would not solve the problems completely, but it would give some help towards protecting our programs against damage from the orphan instances that already exist in the world. And then, with time, I might become convinced that in certain limited cases, perhaps an orphan instance might not be so bad. (And that very temptation is the reason that some in the anti-orphan-instance camp are opposed to my proposal.)

My conclusion from all this is that at least for the time being, I would strongly advise that you avoid declaring any orphan instances, to be considerate to others if for no other reason. Use a `newtype`.
