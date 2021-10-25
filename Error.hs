module Error where
import qualified Tokens as Tk (Position(..))

data TokenError = TkErr 
    { name :: String
    , pos :: Tk.Position
    } deriving Show 