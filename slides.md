% Your Web Service as a Type
% (Typing REST APIs with Servant)

# Most frameworks/languages share these problems.

[columns]

[column=0.5]

*REST problems*

* Explode in subtle ways
* Complexity is becomes nightmare to maintain
* Partially and/or inconsistently documented
* Mix boilerplate with important business logic


[column=0.5]

*Types can fix that!*

* Explode in obvious ways
* Provide a framework for development
* Be a form of documentation, and have 100% coverage
* Make generic programming an option

[/columns]

# Types can fix that!

*Let's see how!*


# APIs have shapes

![Your API as a tree](tree.png)

# Types have shapes (type operators)

[columns]

[column=0.5]

*head :> tail*

* For "joining edges"
* Constructor for a type level non-empty list
* Not directly inhabitable

[column=0.5]

*branch1 :<|> branch2*

* For "branching"
* Constructor for alternatives (disjunction)
* Inhabitable via :<|>

[/columns]

[columns]

[column=0.5]

```haskell
data (path :: k) :> a
    deriving (Typeable)
    infixr 9 :>
```

[column=0.5]

```haskell
data a :<|> b = a :<|> b
    deriving (Typeable, Eq, Show)
infixr 8 :<|>
```

[/columns]

# APIs have shapes

![Your API as a tree](tree.png)

# Shape as a type!

```haskell

type MakeCard =
    "card"
    :> QueryFlag "loud"
    :> ReqBody '[FormUrlEncoded, JSON] Name
    :> Post '[JSON] PersonalisedCard

type RandomInt =
    "random_number" :> Get '[JSON] Int

type CardAPI = "v1.0.0" :> (MakeCard :<|> RandomInt)
```
# How would a typed API even work?

Before we can type the APIs, I have to explain some "fundamentals":

* DataKinds
* PolyKinds
* GHC.TypeLits
* Data.Proxy
* TypeFamilies

# Data.Proxy

## Proxy: 

```haskell
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DataKinds #-}

import Data.Proxy
import GHC.TypeLits

-- | A concrete, poly-kinded proxy type  
data Proxy a = Proxy

stringProxy :: Proxy "I AM A TYPE-LEVEL STRING!"
stringProxy = Proxy

listProxy :: Proxy '[Int, Bool, String]
listProxy = Proxy

symbolVal :: KnownSymbol str => Proxy str -> String
```

# Type families

* Just functions at the type level
* We will use them in the associated form (appearing in a type  class).
* These are called "associated type synonyms".
* They are a specific case of top-level "open" or "closed" type families, but
  give better errors and are clearer in their intentions.

# Silly type family example

## Associated type family

```haskell
{-# LANGUAGE TypeFamilies #-}

class Frobable a where
  type FrobingResult a -- Associated type synonym

  frob :: Proxy a -> FrobingResult a
```

# Silly type family example

## Some uninhabitable types

```haskell
data EatsBools
data MeaningOfLife

widget :: Proxy (EatsBools :> MeaningOfLife)
widget = Proxy
```

# Silly type family example
## Some instances

```haskell
instance Frobable rem
  => Frobable (EatsBools :> rem) where
  type FrobResult (EatsBools :> rem) =
    Bool -> Maybe (FrobResult rem)

  frob :: Proxy (EatsBools :> rem)
       -> FrobResult (EatsBools :> rem)
  frob _ True = Just $ frob (Proxy :: Proxy rem)
  frob _ False = Nothing

instance Frobable MeaningOfLife where
  type FrobResult MeaningOfLife = Int

  frob :: Proxy MeaningOfLife
       -> FrobResult MeaningOfLife
  frob _ = 42
```

# The results

## GHCI

```haskell
> :t frob
frob :: Frobable a => Proxy a -> FrobResult a

> :t widget
widget :: Proxy (EatsBools :> MeaningOfLife)

> :t frob widget
frob widget :: FrobResult (EatsBools :> MeaningOfLife)

> let x = frob widget
> :t x
```

# The results
## GHCI

```haskell
> :t frob
frob :: Frobable a => Proxy a -> FrobResult a

> :t widget
widget :: Proxy (EatsBools :> MeaningOfLife)

> :t frob widget
frob widget :: FrobResult (EatsBools :> MeaningOfLife)

> let x = frob widget
> :t x
x :: Bool -> Maybe Int

> frob widget True
```

# The results
## GHCI

```haskell
> :t frob
frob :: Frobable a => Proxy a -> FrobResult a

> :t widget
widget :: Proxy (EatsBools :> MeaningOfLife)

> :t frob widget
frob widget :: FrobResult (EatsBools :> MeaningOfLife)

> let x = frob widget
> :t x
x :: Bool -> Maybe Int

> frob widget True
Just 42
```

# Recap

* APIs are painful, we now apply type band-aids to the owie.
* The tree-like shape of your API can be expressed with types.
* XXX TODO Something about type families/proxy

# Ugly server boilerplate

API server code is often ugly because it often mixes business logic with...

* Parsing/printing
* Web server code 
* HTTP bits

# Types help us do the things better

Let's see if we can express our business logic by itself, and cram all of the
boiler plate into instances somewhere.

# Unwravelling the types one step at a time

## HasServer: A type class with associated type

```haskell
class HasServer layout where
  type Server layout :: *
  route :: Proxy layout
        -> Server layout
        -> RoutingApplication
```

# Unwravelling the types one step at a time

## Alternative server instance

```haskell
instance (HasServer a, HasServer b) =>
        HasServer (a :<|> b) where

  type Server (a :<|> b) = Server a :<|> Server b

  route Proxy (a :<|> b) request respond =
    route pa a request $ \ mResponse ->
      if isMismatch mResponse
        then route pb b request $ \mResponse' ->
                respond (mResponse <> mResponse')
        else respond mResponse

    where pa = Proxy :: Proxy a
          pb = Proxy :: Proxy b
```
# Unwravelling the types one step at a time

## Query parameter instance

```haskell
instance (KnownSymbol sym, FromText a, HasServer sub)
      => HasServer (QueryParam sym a :> sub) where

  type Server (QueryParam sym a :> sub) =
    Maybe a -> Server sub

  route Proxy subserver req respond = do
    let query = parseQueryText $ rawQueryString req
        paramname = cs $ symbolVal ps
        param = fmap fromText
              . join $ lookup paramname query
    route (Proxy :: Proxy sub)
          (subserver param)
          request respond
    where
      ps = Proxy :: Proxy sym
```

# Unwravelling the types one step at a time

## Terminal Delete instance

```haskell
instance HasServer Delete where
  type Server Delete = EitherT (Int, String) IO ()

  route Proxy action request respond
    | pathIsEmpty request
    && requestMethod request == methodDelete = do
        e <- runEitherT action
        . . .
```

# Remember our types?

## Our api-as-a-type
```haskell
type MakeCard =
    "card"
    :> QueryFlag "loud"
    :> ReqBody '[FormUrlEncoded, JSON] Name
    :> Post '[JSON] PersonalisedCard

type RandomInt =
    "random_number" :> Get '[JSON] Int

type CardAPI = "v1.0.0" :> (MakeCard :<|> RandomInt)
```

# Seperate printing/parsing code

## Instances for printing/parsing
```haskell
instance ToFormUrlEncoded Name where
    toFormUrlEncoded (Name full) =
      [("full_name", full)]

instance FromFormUrlEncoded Name where
    fromFormUrlEncoded [("full_name", full)] =
      Right $ Name full
    fromFormUrlEncoded _ =
      Left "specify full_name"

instance FromJSON PersonalisedCard
instance ToJSON PersonalisedCard

instance FromJSON Name
instance ToJSON Name
```

# We can now business logic cleanly

## Server in a slide
```haskell
server :: Server CardAPI
server = makeCard :<|> randomNumber

makeCard :: Monad m
         => Bool -> Name -> m PersonalisedCard 
makeCard loud (Name full_name) =
    return . PersonalisedCard $
      if loud
        then "HELLO " <> toUpper full_name <> "!!1"
        else "Hello " <> full_name <> "."

randomNumber :: Monad m => m Int
randomNumber = return 4
```

# API type to documentation.

## Define some instances for HasDocs

```haskell
docs :: HasDocs layout => Proxy layout -> API                                   


instance ToParam (QueryFlag "loud") where
  toParam _ =
    DocQueryParam "loud"
                  ["true", "false"]
                  "Get the personalised card loudly.\
                  \ Default is false."
                  Normal
```

# API type to documentation.

## Define some more instances


```haskell
instance ToSample Int where
  toSample = Just 4 -- Fair dice roll

instance ToSample Name where
  toSample = Just $ Name "Hubert Cumberdale"

instance ToSample PersonalisedCard where
  toSamples =
    [ ("If you use ?loud",
      , PersonalisedCard "HELLO, HUBERT CUMBERDALE!!1")
    , ("If you do not use ?loud"
      , PersonalisedCard "Hello, Hubert Cumberdale.")
    ]
```

# Markdown the things

## Given an API we can get an API (terrible name)

```haskell
docs :: HasDocs layout => Proxy layout -> API                                   

markdown :: API -> String
```

# GET /v1.0.0/random_numbers

#### Response:

- Status code 200

- Supported content types are:

    - `application/json`

- Response body as below.

```javascript
[4]
```

# POST /v1.0.0/card

#### GET Parameters:

- loud
     - **Values**: *true, false*
     - **Description**: Get the personalised card loudly. Default is false.

# POST /v1.0.0/card

#### Request:

- Supported content types are:

    - `application/x-www-form-urlencoded`
    - `application/json`

- Example: `application/x-www-form-urlencoded`

```
full_name=Hubert%20Cumberdale
```

- Example: `application/json`

```javascript
{"_nameFull":"Hubert Cumberdale"}
```

# POST /v1.0.0/card

#### Response:

- Status code 201

- Supported content types are:

    - `application/json`

- If you use ?loud

```javascript
{"_cardBody":"HELLO, HUBERT CUMBERDALE!!1"}
```

- If you do not use ?loud

```javascript
{"_cardBody":"Hello, Hubert Cumberdale."}
```

# Clients for free (tackling complexity)

Consider an unversioned API that has:

* Three breaking changes
* Six users

How many changes must you make to fix all of the things?

# Clients for free (tackling complexity)

![Complexity to maintain](square.png)

# Client types

## Given an API, we can generate a Client
```haskell
client
  :: HasClient layout => Proxy layout -> Client layout                     
```

## The magic
```haskell
class HasClient layout where
  type Client layout :: *
  clientWithRoute
    :: Proxy layout -> Req -> Client layout


  instance (HasClient a, HasClient b)
        => HasClient (a :<|> b) where               
    type Client (a :<|> b) = Client a :<|> Client b                               
    clientWithRoute Proxy req =                                                   
      clientWithRoute (Proxy :: Proxy a) req :<|>                                 
      clientWithRoute (Proxy :: Proxy b) req   
```

# Client types

## "Client" was distributed over "Alternative" (:<|>)

```haskell
createCard
    :: Bool
    -> Name
    -> BaseUrl
    -> EitherT ServantError IO PersonalisedCard

getDice
    :: BaseUrl
    -> EitherT ServantError IO [Int]

(createCard :<|> getDice) = client cardApi
```
 
# API type to type safe URLs

## Given a slightly intimidating type signature

```haskell
safeLink
    :: forall endpoint api. ( IsElem endpoint api
                            , HasLink endpoint)
    => Proxy api
    -> Proxy endpoint
    -> MkLink endpoint
```

# API type to type safe URLs

## We can generate safe links

```haskell
let nums = Proxy :: Proxy ("v1.0.0" :> RandomInts)
print $ safeLink cardApi nums 


let make_card = Proxy :: Proxy ("v1.0.0" :> MakeCard)
print $ safeLink cardApi make_card True
```

>> v1.0.0/random_numbers
>> v1.0.0/card?loud

# Conclusion

* Web services have problems, types fix some
* By defining your API as a type, you can get:
	- Server boilerplate
	- Documentation
	- Clients (Haskell, jquery, PureScript)
	- Safe links
