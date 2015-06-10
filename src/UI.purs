module UI where

import qualified Thermite as T
import qualified Thermite.Html as T
import qualified Thermite.Html.Elements as H
import qualified Thermite.Html.Attributes as A
import qualified Thermite.Events as T
import qualified Thermite.Action as T
import qualified Thermite.Types as T

data State =
        HelloWorld

initialState :: State
initialState = HelloWorld

performAction :: T.PerformAction _ State _ Action
performAction _ HelloWorld = id

render :: T.Render _ State _ Action
render HelloWorld = 
        H.div (A.className "container") (H.h1' "Hello World!" )

spec :: T.Spec _ State _ Action
spec = T.simpleSpec initialState performAction render

main = do
    let component = T.createClass spec
    T.render component unit
