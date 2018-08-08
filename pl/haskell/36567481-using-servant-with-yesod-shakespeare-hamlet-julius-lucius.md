
# Using Servant with Yesod shakespeare (Hamlet, Julius, Lucius)

## Question
      
How i can use shakespeare (from yesod) for servant webservices APIs?

I try:

    type TestAPI 
        = "tests" :> Get '[JSON] [Test]
        :<|> "Test.html" :> Get '[HTML] Html
    
    serverTestAPI :: ServerT TestAPI AppM
    serverTestAPI = tests 
               :<|> test
               :<|> testHtml
    
    tests :: AppM [Test]
    tests = do return [ Test 1 "Test 1"
                      , Test 2 "Test 2"
                      ]
    
    testHtml = [hamlet|
                    $doctype 5
                     .........
                     |]
    

But I get error!
## Answer
      
As @Carsten points out, in this case what you want is `shamlet`. The key thing is implementing proper instances of the `ToMarkup` typeclass. I recommend you to read this [excellent introduction](http://www.yesodweb.com/book/shakespearean-templates) on Shakespeare templates. A working example:

    data Person = Person
      { firstName :: String
      , lastName  :: String
      } deriving Generic 
    
    instance ToJSON Person
    
    type PersonAPI = "persons" :> Get '[JSON, HTML] [Person]
    
    people :: [Person]
    people =
      [ Person "Isaac"  "Newton"
      , Person "Albert" "Einstein"
      ]
    
    instance ToMarkup Person where
      toMarkup person = showPerson person
    
      -- this isn't properly implemented
      preEscapedToMarkup p = showPerson p
    
    -- HTML serialization of a list of persons
    instance ToMarkup [Person] where
      toMarkup persons = showPersons persons
    
      preEscapedToMarkup p = showPersons p
    
    showPerson :: Person -> Html
    showPerson p = [shamlet|
    <body>
        <p>This is my page.
        <h1>#{firstName p}
    |]
    
    showPersons :: [Person] -> Html
    showPersons p = [shamlet|
    <body>
        <p>This is my page.
         $forall person <- p
          <h1>#{firstName person}
    |]
    