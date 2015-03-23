{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

import Data.Aeson
import Data.Monoid
import Data.Proxy
import Data.Text
import Control.Monad.Trans.Either
import GHC.Generics
import Network.Wai
import Network.Wai.Handler.Warp

import Servant
import Servant.Docs
import Servant.Utils.Links
import Servant.Client

-- * Example

-- | A greet message data type
newtype Name = Name { _nameFull :: Text }
  deriving (Generic, Show)

newtype PersonalisedCard = PersonalisedCard { _cardBody :: Text }
  deriving (Generic, Show)

instance ToFormUrlEncoded Name where
    toFormUrlEncoded (Name full) = [("full_name", full)]

instance FromFormUrlEncoded Name where
    fromFormUrlEncoded [("full_name", full)] = Right $ Name full
    fromFormUrlEncoded _                     = Left "specify full_name"

instance FromJSON PersonalisedCard
instance ToJSON PersonalisedCard

instance FromJSON Name
instance ToJSON Name

type MakeCard =
    "card"
    :> QueryFlag "loud"
    :> ReqBody '[FormUrlEncoded, JSON] Name
    :> Post '[JSON] PersonalisedCard

type RandomInts =
    "random_numbers" :> Get '[JSON] [Int]

type CardAPI = "v1.0.0" :> (MakeCard :<|> RandomInts)

cardApi :: Proxy CardAPI
cardApi = Proxy

-- Server-side handlers.
--
-- There's one handler per endpoint, which, just like in the type
-- that represents the API, are glued together using :<|>.
--
-- Each handler runs in the 'EitherT (Int, String) IO' monad.
server :: Server CardAPI
server = makeCard :<|> randomNumber

makeCard :: Monad m => Bool -> Name -> m PersonalisedCard 
makeCard loud (Name full_name) =
    return . PersonalisedCard $
        if loud
            then "HELLO " <> toUpper full_name <> "!!1"
            else "Hello " <> full_name <> "."

randomNumber :: Monad m => m [Int]
randomNumber = return [4] -- fair dice roll

-- Turn the server into a WAI app. 'serve' is provided by servant,
-- more precisely by the Servant.Server module.
card :: Application
card = serve cardApi server

-- Run the server.
--
-- 'run' comes from Network.Wai.Handler.Warp
runCardServer :: Port -> IO ()
runCardServer port = run port card

-----------------------------------------------------------------------------
-- Docs

cardDocs :: API
cardDocs = docs cardApi

instance ToParam (QueryFlag "loud") where
  toParam _ =
    DocQueryParam "loud"
                  ["true", "false"]
                  "Get the personalised card loudly.\
                  \ Default is false."
                  Normal

instance ToSample [Int] where
  toSample = Just [4] -- Fair dice roll

instance ToSample Name where
  toSample = Just $ Name "Hubert Cumberdale"

instance ToSample PersonalisedCard where
  toSamples =
    [ ("If you use ?loud", PersonalisedCard "HELLO, HUBERT CUMBERDALE!!1")
    , ("If you do not use ?loud", PersonalisedCard "Hello, Hubert Cumberdale.")
    ]

createCard
    :: Bool
    -> Name
    -> BaseUrl
    -> EitherT ServantError IO PersonalisedCard

getDice
    :: BaseUrl
    -> EitherT ServantError IO [Int]

(createCard :<|> getDice) = client cardApi

class Frobable a where
  type FrobResult a -- Type family declaration

  frob :: Proxy a -> FrobResult a


data EatsBools
data MeaningOfLife

widget :: Proxy (EatsBools :> MeaningOfLife)
widget = Proxy

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

  frob :: Proxy MeaningOfLife -> FrobResult MeaningOfLife
  frob _ = 42
  

-- Put this all to work!
main :: IO ()
main = do
    print $ frob widget True
    putStrLn $ markdown cardDocs
    let nums = Proxy :: Proxy ("v1.0.0" :> RandomInts)
    print $ safeLink cardApi nums 

    let make_card = Proxy :: Proxy ("v1.0.0" :> MakeCard)
    print $ safeLink cardApi make_card True

    let url = BaseUrl Http "localhost" 8001
    -- runEitherT (createCard True (Name "Hubert Cumberdale") url)  >>= print
    runCardServer 8001
