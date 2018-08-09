
# forall in Scala

## Question
        
As shown below, in Haskell, it's possible to store in a list values with heterogeneous types with certain context bounds on them:

    data ShowBox = forall s. Show s => ShowBox s
    
    heteroList :: [ShowBox]
    heteroList = [ShowBox (), ShowBox 5, ShowBox True]
    

How can I achieve the same in Scala, preferably without subtyping?

## Answer
        
As @Michael Kohl commented, this use of forall in Haskell is an existential type and can be exactly replicted in Scala using either the forSome construct or a wildcard. That means that @paradigmatic's answer is largely correct.

Nevertheless there's something missing there relative to the Haskell original which is that instances of its ShowBox type also capture the corresponding Show type class instances in a way which makes them available for use on the list elements even when the exact underlying type has been existentially quantified out. Your comment on @paradigmatic's answer suggests that you want to be able to write something equivalent to the following Haskell,

    data ShowBox = forall s. Show s => ShowBox s
    
    heteroList :: [ShowBox]
    heteroList = [ShowBox (), ShowBox 5, ShowBox True]
    
    useShowBox :: ShowBox -> String
    useShowBox (ShowBox s) = show s
    
    -- Then in ghci ...
    
    *Main> map useShowBox heteroList
    ["()","5","True"]
    

@Kim Stebel's answer shows the canonical way of doing that in an object-oriented language by exploiting subtyping. Other things being equal, that's the right way to go in Scala. I'm sure you know that, and have good reasons for wanting to avoid subtyping and replicate Haskell's type class based approach in Scala. Here goes ...

Note that in the Haskell above the Show type class instances for Unit, Int and Bool are available in the implementation of the useShowBox function. If we attempt to directly translate this into Scala we'll get something like,

    trait Show[T] { def show(t : T) : String }
    
    // Show instance for Unit
    implicit object ShowUnit extends Show[Unit] {
      def show(u : Unit) : String = u.toString
    }
    
    // Show instance for Int
    implicit object ShowInt extends Show[Int] {
      def show(i : Int) : String = i.toString
    }
    
    // Show instance for Boolean
    implicit object ShowBoolean extends Show[Boolean] {
      def show(b : Boolean) : String = b.toString
    }
    
    case class ShowBox[T: Show](t:T)
    
    def useShowBox[T](sb : ShowBox[T]) = sb match {
      case ShowBox(t) => implicitly[Show[T]].show(t)
      // error here      ^^^^^^^^^^^^^^^^^^^
    } 
    
    val heteroList: List[ShowBox[_]] = List(ShowBox(()), ShowBox(5), ShowBox(true))
    
    heteroList map useShowBox
    

and this fails to compile in useShowBox as follows,

    <console>:14: error: could not find implicit value for parameter e: Show[T]
             case ShowBox(t) => implicitly[Show[T]].show(t)
                                          ^
    

The problem here is that, unlike in the Haskell case, the Show type class instances aren't propagated from the ShowBox argument to the body of the useShowBox function, and hence aren't available for use. If we try to fix that by adding an additional context bound on the useShowBox function,

    def useShowBox[T : Show](sb : ShowBox[T]) = sb match {
      case ShowBox(t) => implicitly[Show[T]].show(t) // Now compiles ...
    } 
    

this fixes the problem within useShowBox, but now we can't use it in conjunction with map on our existentially quantified List,

    scala> heteroList map useShowBox
    <console>:21: error: could not find implicit value for evidence parameter
                         of type Show[T]
                  heteroList map useShowBox
                                 ^
    

This is because when useShowBox is supplied as an argument to the map function we have to choose a Show instance based on the type information we have at that point. Clearly there isn't just one Show instance which will do the job for all of the elements of this list and so this fails to compile (if we had defined a Show instance for Any then there would be, but that's not what we're after here ... we want to select a type class instance based on the most specific type of each list element).

To get this to work in the same way that it does in Haskell, we have to explicitly propagate the Show instances within the body of useShowBox. That might go like this,

    case class ShowBox[T](t:T)(implicit val showInst : Show[T])
    
    val heteroList: List[ShowBox[_]] = List(ShowBox(()), ShowBox(5), ShowBox(true))
    
    def useShowBox(sb : ShowBox[_]) = sb match {
      case sb@ShowBox(t) => sb.showInst.show(t)
    }
    

then in the REPL,

    scala> heteroList map useShowBox
    res7: List[String] = List((), 5, true)
    

Note that we've desugared the context bound on ShowBox so that we have an explicit name (showInst) for the Show instance for the contained value. Then in the body of useShowBox we can explicitly apply it. Also note that the pattern match is essential to ensure that we only open the existential type once in the body of the function.

As should be obvious, this is a lot more vebose than the equivalent Haskell, and I would strongly recommend using the subtype based solution in Scala unless you have extremely good reasons for doing otherwise.

**Edit**

As pointed out in the comments, the Scala definition of ShowBox above has a visible type parameter which isn't present in the Haskell original. I think it's actually quite instructive to see how we can rectify that using abstract types.

First we replace the type parameter with an abstract type member and replace the constructor parameters with abstract vals,

    trait ShowBox {
      type T
      val t : T
      val showInst : Show[T]
    }
    

We now need to add the factory method that case classes would otherwise give us for free,

    object ShowBox {
      def apply[T0 : Show](t0 : T0) = new ShowBox {
        type T = T0
        val t = t0
        val showInst = implicitly[Show[T]]
      } 
    }
    

We can now use plain ShowBox whereever we previously used ShowBox\[_\] ... the abstract type member is playing the role of the existential quantifier for us now,

    val heteroList: List[ShowBox] = List(ShowBox(()), ShowBox(5), ShowBox(true))
    
    def useShowBox(sb : ShowBox) = {
      import sb._
      showInst.show(t)
    }
    
    heteroList map useShowBox
    

(It's worth noting that prior to the introduction of explict forSome and wildcards in Scala this was exactly how you would represent existential types.)

We now have the existential in exactly the same place as it is in the original Haskell. I think this is as close to a faithful rendition as you can get in Scala.
