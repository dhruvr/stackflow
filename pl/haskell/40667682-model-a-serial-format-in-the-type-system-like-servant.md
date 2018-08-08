
# Model a serial format in the type system, like Servant

## Question
      
I'm working on an API integration that ignores the existence of XML or JSON in favor of just appending character data. (The [Metro2](http://www.cdiaonline.org/Metro2/content.cfm?ItemNumber=853) format, if interested)

I'm simplifying, but imagine that a person needs to be serialized like this:

*   At pos 0, 4 chars: Number of bytes in the message
*   At pos 5: 6 chars: `"PERSON"` hard coded
*   At pos 11: 20 chars: Name, left-aligned and space-padded
*   At pos 21: 8 chars: Birthday, `YYYYMMDD`
*   At pos 29: 3 chars: Age, right-aligned and zero-padded

Numeric fields are always right-aligned and zero-padded. Text fields are always left-aligned and space padded.

For example:

    "0032PERSONDAVID WILCOX        19820711035"
    

Can I express this in the type system? Like what [servant](http://haskell-servant.readthedocs.io/en/stable/tutorial/ApiType.html) does? Something like this?

    newtype ByteLength = ByteLength Int
    newtype Age = Age Int
    -- etc
    
    type PersonMessage
         = Field ByteLength '0
        :| Field "PERSON" '5
        :| Field Name '11
        :| Field Date '21
        :| Field Age '29
    
    -- :| is a theoretical type operator, like :> in servant
    -- the number is the expected offset
    -- the length of the field is implicit in the type
    

Can I statically check that my implementation of the serialization matches the type?

Can I statically check that the offset of the 3rd field (`Name`) is `11`? That the lengths of the preceding fields add up to 11? I'm assuming no, since that seems like it would require full dependent type support.

Is this on the right track?

    instance ToMetro Age where
       -- get the length into the type system using a type family?
       field = Numeric '3
    
       -- express how this is encoded. Would need to use the length from the type family. Or if that doesn't work, put it in the constructor.
       toMetro age = Numeric age
    

_Update:_ Example of a function I would like to statically validate:

    personToMetro :: Person -> PersonMessage
    personToMetro p = error "Make sure that what I return is a PersonMessage"
## Answer
      
Just to give you some inspiration, just do what Servant does and have different types for the different combinators you support:

    {-# LANGUAGE GADTs, DataKinds, KindSignatures, TypeOperators, ScopedTypeVariables #-}
    
    module Seriavant where
    
    import GHC.TypeLits
    import Data.Proxy
    import Data.List (stripPrefix)
    
    data Skip (n :: Nat) = Skip deriving Show
    data Token (n :: Nat) = Token String deriving Show
    data Lit (s :: Symbol) = Lit deriving Show
    
    data (:>>) a b = a :>> b deriving Show
    infixr :>>
    
    class Deserialize a where
        deserialize :: String -> Maybe (a, String)
    
    instance (KnownNat n) => Deserialize (Skip n) where
        deserialize s = do
            (_, s') <- trySplit (natVal (Proxy :: Proxy n)) s
            return (Skip, s')
    
    instance (KnownNat n) => Deserialize (Token n) where
        deserialize s = do
            (t, s') <- trySplit (natVal (Proxy :: Proxy n)) s
            return (Token t, s')
    
    instance (KnownSymbol lit) => Deserialize (Lit lit) where
        deserialize s = do
            s' <- stripPrefix (symbolVal (Proxy :: Proxy lit)) s
            return (Lit, s')
    
    instance (Deserialize a, Deserialize b) => Deserialize (a :>> b) where
        deserialize s = do
            (x, s') <- deserialize s
            (y, s'') <- deserialize s'
            return (x :>> y, s'')
    
    trySplit :: Integer -> [a] -> Maybe ([a], [a])
    trySplit 0 xs = return ([], xs)
    trySplit n (x:xs) = do
        (xs', ys) <- trySplit (n-1) xs
        return (x:xs', ys)
    trySplit _ _ = Nothing
    

Yeah so this is quite spartan, but it already allows you to do

    type MyFormat = Token 4 :>> Lit "PERSON" :>> Skip 1 :>> Token 4
    
    testDeserialize :: String -> Maybe MyFormat
    testDeserialize = fmap fst . deserialize
    

which works like this:

>     *Seriavant> testDeserialize "1"
>     Nothing
>     *Seriavant> testDeserialize "1234PERSON Foo "
>     Just (Token "1234" :>> (Lit :>> (Skip :>> Token "Foo ")))
>     

**EDIT**: Turns out I completely misread the question, and Sean is asking for serialization, not deserialization... But of course we can do that as well:

    class Serialize a where
        serialize :: a -> String
    
    instance (KnownNat n) => Serialize (Skip n) where
        serialize Skip = replicate (fromIntegral $ natVal (Proxy :: Proxy n)) ' '
    
    instance (KnownNat n) => Serialize (Token n) where
        serialize (Token t) = pad (fromIntegral $ natVal (Proxy :: Proxy n)) ' ' t
    
    instance (KnownSymbol lit) => Serialize (Lit lit) where
        serialize Lit = symbolVal (Proxy :: Proxy lit)
    
    instance (Serialize a, Serialize b) => Serialize (a :>> b) where
        serialize (x :>> y) = serialize x ++ serialize y
    
    pad :: Int -> a -> [a] -> [a]
    pad 0 _x0 xs = xs
    pad n x0 (x:xs) = x : pad (n-1) x0 xs
    pad n x0 [] = replicate n x0
    

(of course this has horrible performance with all this `String` concatenation etc. but that's not the point here)

>     *Seriavant> serialize ((Token "1234" :: Token 4) :>> (Lit :: Lit "FOO") :>> (Skip :: Skip 2) :>> (Token "Bar" :: Token 10))
>     "1234FOO  Bar       "
>     

Of course, if we know the format, we can avoid those pesky type annotations:

    type MyFormat = Token 4 :>> Lit "PERSON" :>> Skip 1 :>> Token 4
    
    testSerialize :: MyFormat -> String
    testSerialize = serialize
    

>     *Seriavant> testSerialize (Token "1234" :>> Lit :>> Skip :>> Token "Bar")
>     "1234PERSON Bar "
>
    