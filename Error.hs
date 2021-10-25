module Error where
import qualified Tokens as Tk (Position(..))

data TokenError = TkErr 
    { name :: String
    , pos :: Tk.Position
    } 

instance Show TokenError where
    show err = show (name err) ++ " at " ++ show (pos err)