module Helpers where

import Halogen.HTML.Properties as HP
import Halogen.HTML as HH

class_ :: forall r i. String -> HP.IProp (class :: String | r) i
class_ str =
  HP.class_ (HH.ClassName str)
