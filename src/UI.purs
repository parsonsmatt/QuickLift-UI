module UI where

import qualified Thermite as T
import qualified Thermite.Html as T
import qualified Thermite.Html.Elements as T
import qualified Thermite.Html.Attributes as A
import qualified Thermite.Events as T
import qualified Thermite.Action as T
import qualified Thermite.Types as T

import UI.AJAX

data State = HelloWorld | Friends

data Action = Change

initialState :: State
initialState = HelloWorld

performAction :: T.PerformAction _ State _ Action
performAction _ Change = toggleState

toggleState = T.modifyState \o -> case o of
                                       HelloWorld -> Friends
                                       Friends -> HelloWorld

render :: T.Render _ State _ Action
render ctx HelloWorld _ _ = T.h1 (T.onClick ctx (\_ -> Change)) [T.text "Hello world!"]
render ctx Friends _ _ = T.h1 (T.onClick ctx (\_ -> Change)) [T.text "Omg frands"]

spec :: T.Spec _ State _ Action
spec = T.simpleSpec initialState performAction render

main = do
    let component = T.createClass spec
    T.render component unit 
