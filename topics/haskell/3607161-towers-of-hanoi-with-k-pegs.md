
# Towers of Hanoi with K pegs

## Question
        
The [Towers of Hanoi](http://en.wikipedia.org/wiki/Tower_of_Hanoi) problem is a classic problem for recursion. You are given 3 pegs with disks on one of them, and you must move all the disks from one peg to another, by following the given rules. You must also do this with the minimum number of moves.

Here's a recursive algorithm that solves the problem:

    void Hanoi3(int nDisks, char source, char intermed, char dest)
    {
        if( nDisks > 0 )
        {
            Hanoi3(nDisks - 1, source, dest, intermed);
            cout << source << " --> " << dest << endl;
            Hanoi3(nDisks - 1, intermed, source, dest);
        }
    }
    
    
    int main()
    {
        Hanoi3(3, 'A', 'B', 'C');
    
        return 0;
    }
    

Now, imagine the same problem, only with 4 pegs, so we add another intermediary peg. When faced with the problem of having to choose which intermediary peg to choose at any one point, we will choose the leftmost one, in case more than 1 is free.

I have the following recursive algorithm for this problem:

    void Hanoi4(int nDisks, char source, char intermed1, char intermed2, char dest)
    {
        if ( nDisks == 1 )
            cout << source << " --> " << dest << endl;
        else if ( nDisks == 2 )
        {
            cout << source << " --> " << intermed1 << endl;
            cout << source << " --> " << dest << endl;
            cout << intermed1 << " --> " << dest << endl;
        }
        else
        {
            Hanoi4(nDisks - 2, source, intermed2, dest, intermed1);
            cout << source << " --> " << intermed2 << endl;
            cout << source << " --> " << dest << endl;
            cout << intermed2 << " --> " << dest << endl;
            Hanoi4(nDisks - 2, intermed1, source, intermed2, dest);
        }
    }
    
    int main()
    {
        Hanoi4(3, 'A', 'B', 'C', 'D');
    
        return 0;
    }
    

Now, my question is how would I generalize this recursive approach to work for `K` pegs? The recursive function would receive a `char[]` which would hold the labels of each stack, so the function would look something like this:

    void HanoiK(int nDisks, int kStacks, char labels[]) { ... }
    

I know about the Frame-Stewart algorithm, which is most likely optimal but not proven, and which gives you the **number** of moves. However, I am interested in a strictly recursive solution that follows the pattern of the recursive solutions for 3 and 4 pegs, meaning it prints the actual moves.

For me at least, the pseudocode of the Frame-Stewart algorithm presented on Wikipedia is rather abstract, and I haven't been successful at translating it into code that prints the moves. I would accept a reference implementation of that (for random `k`), or even more detailed pseudocode.

I tried to come up with some sort of algorithm that permutes the labels array accordingly, but I've had no luck getting it to work. Any suggestions are appreciated.

**Update:**

This seems to be a lot easier to solve in a functional language. Here's an F# implementation based on LarsH's Haskell solution:

    let rec HanoiK n pegs = 
        if n > 0 then 
            match pegs with
            | p1::p2::rest when rest.IsEmpty            
                ->  printfn "%A --> %A" p1 p2
            | p1::p2::p3::rest when rest.IsEmpty        
                ->  HanoiK (n-1) (p1::p3::p2::rest)
                    printfn "%A --> %A" p1 p2
                    HanoiK (n-1) (p3::p2::p1::rest)    
            | p1::p2::p3::rest when not rest.IsEmpty    
                ->  let k = int(n / 2)
                    HanoiK k (p1::p3::p2::rest)
                    HanoiK (n-k) (p1::p2::rest)
                    HanoiK k (p3::p2::p1::rest)
    
    let _ =
        HanoiK 6 [1; 2; 3; 4; 5; 6]
    

And without treating 3 pegs as an edge case:

    let rec HanoiK n pegs = 
        if n > 0 then 
            match pegs with
            | p1::p2::rest when rest.IsEmpty            
                ->  printfn "%A --> %A" p1 p2
            | p1::p2::p3::rest     
                ->  let k = if rest.IsEmpty then n - 1 else int(n / 2) 
                    HanoiK k (p1::p3::p2::rest)
                    HanoiK (n-k) (p1::p2::rest)
                    HanoiK k (p3::p2::p1::rest)
    

Note that this does not handle degenerate cases for which there is no solution, such as `HanoiK 2 [1; 2]`

## Answer
        
Here is an implementation in Haskell **(update:** took care of 3-peg case by making k = n-1 when r=3):

    -- hanoi for n disks and r pegs [p1, p2, ..., pr]
    hanoiR :: Int -> [a] -> [(a, a)]
    
    -- zero disks: no moves needed.
    hanoiR 0 _ = []
    
    -- one disk: one move and two pegs needed.
    hanoiR 1 (p1 : p2 : rest) = [(p1, p2)] -- only needed for smart-alecks?
    
    {-
    -- n disks and 3 pegs -- unneeded; covered by (null rest) below.
    hanoiR n [p1, p2, p3] =
        hanoiR (n - 1) [p1, p3, p2] ++
        [(p1, p2)] ++
        hanoiR (n - 1) [p3, p2, p1]
    -}
    
    -- n disks and r > 3 pegs: use Frame-Stewart algorithm
    hanoiR n (p1 : p2 : p3 : rest) =
        hanoiR k (p1 : p3 : p2 : rest) ++
        hanoiR (n - k) (p1 : p2 : rest) ++
        hanoiR k (p3 : p2 : p1 : rest)
        where k
            | null rest   = n - 1
            | otherwise   = n `quot` 2
    

So load this in [GHCi](http://hackage.haskell.org/platform/) and enter

    hanoiR 4 [1, 2, 3, 4]
    

I.e. run the Towers of Hanoi with 4 disks and 4 pegs. You can name the 4 pegs whatever you want, e.g.

    hanoiR 4 ['a', 'b', 'c', 'd']
    

The output:

    [(1,2),(1,3),(2,3),(1,4),(1,2),(4,2),(3,1),(3,2),(1,2)]
    

I.e. move the top disk from peg 1 to peg 2, then the top disk from peg 1 to peg 3, etc.

I'm pretty new to Haskell so I must admit I'm proud that this works. But I may have silly mistakes so feedback is welcome.

As you can see from the code, the heuristic for k is simply floor(n / 2). I haven't tried to optimize k, though n/2 seemed like a good guess.

I've verified the correctness of the answer for 4 disks and 4 pegs. It's too late at night for me to verify more, without writing a simulator. (@_@) Here are a few more results:

    ghci>  hanoiR 6 [1, 2, 3, 4, 5]
    [(1,2),(1,4),(1,3),(4,3),(2,3),(1,4),(1,5),(1,2),
     (5,2),(4,2),(3,1),(3,4),(3,2),(4,2),(1,2)]
    ghci>  hanoiR 6 [1, 2, 3, 4]
    [(1,2),(1,4),(1,3),(4,3),(2,3),(1,2),(1,4),(2,4),(1,2),
     (4,1),(4,2),(1,2),(3,1),(3,4),(3,2),(4,2),(1,2)]
    ghci>  hanoiR 8 [1, 2, 3, 4, 5]
    [(1,3),(1,2),(3,2),(1,4),(1,3),(4,3),(2,1),(2,3),(1,3),(1,2),
     (1,4),(2,4),(1,5),(1,2),(5,2),(4,1),(4,2),(1,2),
     (3,2),(3,1),(2,1),(3,4),(3,2),(4,2),(1,3),(1,2),(3,2)]
    

Does this clarify the algorithm?

Really the essential piece is

    hanoiR k (p1 : (p3 : (p2 : rest))) ++      -- step 1; corresponds to T(k,r)
    hanoiR (n-k) (p1 : (p2 : rest)) ++         -- step 2; corresponds to T(n-k, r-1)
    hanoiR k (p3 : (p2 : (p1 : rest)))         -- step 3; corresponds to T(k,r)
    

where we concatenate the sequences of moves for steps 1, 2, and 3 of the Frame-Stewart algorithm. In order to determine the moves, we annotate F-S's steps as follows:

*   Conventionally, when hanoi is called, the goal is defined (without loss of generality) as transferring the disks from the first peg to the second peg, using all remaining pegs for temporary storage. We use this convention when recursing, to define the source, destination, and allowed storage of the divided-and-conquered subproblems.
*   Thus the source peg is p1, and the destination peg is p2. All the remaining pegs are available as temporary storage, for the top-level hanoi problem.
*   Step 1, "For some k, 1 <= k < n, transfer the top k disks to a single other peg": we choose p3 as "a single other peg".
*   Thus "without disturbing the peg that now contains the top k disks" (step 2) means to recurse using all the pegs except p3. I.e. p1, p2, and the rest beyond p3.
*   "Transfer the top k disks to the destination peg" (step 3) means transfer from the "other peg" (p3) to p2.

Does that make sense?
