module UI.AJAX where
        
import Control.Monad.Aff
import Network.HTTP.Affjax

import Data.Int
import Data.Maybe
import Data.Either
import Data.Foreign
import Data.Foreign.Class

import Control.Monad.Aff
import Control.Monad.Eff.Exception (Error(), error)

import Control.Monad.Error.Class (throwError)

import qualified Thermite.Action as T

import Network.HTTP.Affjax
import Network.HTTP.StatusCode (StatusCode(..))
import Network.HTTP.MimeType (MimeType(..))
import Network.HTTP.RequestHeader (RequestHeader(..))
import Network.HTTP.Method (Method(..))

-- | A type synonym for readability.
type AjaxAction eff response = forall state. T.Action (ajax :: AJAX | eff) state (Either String response)
    
ajax :: forall eff a. (IsForeign a) => Affjax eff Foreign -> AjaxAction eff a
ajax aff = T.async \k -> 
    runAff (\_ -> k (Left "Web service call failed"))
           (\r -> k (readWith (const "Cannot parse response") r))
           aff1
  where
  aff1 = do
    r <- aff
    case r.status of
      StatusCode n | n >= fromNumber 200 && n < fromNumber 300 -> return r.response
      _ -> throwError (error "Bad status code")

listUsers :: forall eff. AjaxAction eff (Array User) 
listUsers = ajax $ get "/users/"

newtype User = User
        { name :: String
        , email :: String
        }

instance isForeignUser :: IsForeign User where
        read value = User <$> (
                { name: _, email: _} 
                <$> readProp "name" value
                <*> readProp "email" value
                )

instance showUser :: Show User where
    show (User u) = "Name: " ++ u.name ++ ", Email: " ++ u.email
