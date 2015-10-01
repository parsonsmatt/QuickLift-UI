module UI where

import Control.Monad.Eff
import Data.Array
import Debug.Trace
import Data.Either
import qualified Thermite as T
import qualified Thermite.Html as T
import qualified Thermite.Html.Elements as T
import qualified Thermite.Html.Attributes as A
import qualified Thermite.Events as T
import qualified Thermite.Action as T
import qualified Thermite.Types as T

import Control.Monad.Aff
import UI.AJAX

type State =
           { page :: Page
           , friends :: Array User
           }

data Page = HelloWorld | Friends

data Action = Change

initialState :: State
initialState = State { page: HelloWorld, friends: [] }

performAction :: T.PerformAction _ State _ Action
performAction _ Change = do
    Right users <- listUsers
    toggleState' users

toggleState' users = T.modifyState \o ->
                     case o of
                          State { page: HelloWorld } -> State { page: Friends, friends: users }
                          State { page: Friends } -> State { page: HelloWorld, friends: [] }

putUsers :: forall a eff. (Show a) => a -> Eff (trace :: Trace | eff) Unit
putUsers users = trace (show users) 

initFriends :: Array User
initFriends = []

toggleState = T.modifyState \State o -> case o of
                                       { page: HelloWorld, friends: f } -> State { page: Friends, friends: o.friends }
                                       { page: Friends, friends: f } -> State { page: HelloWorld, friends: [] }

render :: T.Render _ State _ Action
render ctx (State { page: HelloWorld, friends: _ }) _ _ = T.h1 (T.onClick ctx (\_ -> Change)) [T.text "Hello world!"]
render ctx (State { page: Friends, friends: f }) _ _ = 
    T.div' 
        [ T.h1 (T.onClick ctx (\_ -> Change)) [T.text "Omg frands"]
        , T.ul' (map (\u -> T.li' [T.text (show u)]) f) 
        ]

spec :: T.Spec _ State _ Action
spec = T.simpleSpec initialState performAction render

main = do
    T.render (T.createClass spec) unit
