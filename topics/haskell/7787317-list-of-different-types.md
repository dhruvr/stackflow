
# List of different types?

## Question
        
    data Plane = Plane { point :: Point, normal :: Vector Double }
    data Sphere = Sphere { center :: Point, radius :: Double }
    
    class Shape s where
        intersect :: s -> Ray -> Maybe Point
        surfaceNormal :: s -> Point -> Vector Double
    

I have also made both `Plane` and `Sphere` instances of `Shape`.

I'm trying to store spheres and planes in the same list, but it doesn't work. I understand that it shouldn't work because `Sphere` and `Plane` are two different types, but they are both instances of `Shape`, so shouldn't it work? How would I store shapes and planes in a list?

    shapes :: (Shape t) => [t]
    shapes = [ Sphere { center = Point [0, 0, 0], radius = 2.0 },
             Plane { point = Point [1, 2, 1], normal = 3 |> [0.5, 0.6, 0.2] }
             ]

## Answer
        
This problem represents a turning point between object-oriented and functional thinking. Sometimes even sophisticated Haskellers are still in this mental transition, and their designs often fall into the [existential typeclass](http://lukepalmer.wordpress.com/2010/01/24/haskell-antipattern-existential-typeclass/) pattern, mentioned in Thomas's answer.

A functional solution to this problem involves reifying the typeclass into a data type (usually once this is done, the need for the typeclass vanishes):

    data Shape = Shape {
        intersect :: Ray -> Maybe Point,
        surfaceNormal :: Point -> Vector Double
    }
    

Now you can easily construct a list of `Shape`s, because it is a monomorphic type. Because Haskell does not support downcasting, no information is lost by removing the representational distinction between `Plane`s and `Sphere`s. The specific data types become functions that construct `Shape`s:

    plane :: Point -> Vector Double -> Shape
    sphere :: Point -> Double -> Shape
    

If you cannot capture everything you need to know about a shape in the `Shape` data type, you can enumerate the cases with an algebraic data type, as Thomas suggested. But I would recommend against that if possible; instead, try to find the essential characteristics of a shape that you need rather than just listing off examples.
