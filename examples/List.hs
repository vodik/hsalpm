import Control.Monad
import Language.Haskell.HsColour.ANSI
import System.IO.Unsafe

import Foreign.C
import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.Marshal.Alloc
import Foreign.Storable

import Alpm.List

ppList :: Ptr (AlpmList CChar) -> IO ()
ppList ptr = do
    (AlpmList d n p) <- peek ptr
    str <- peekCString d

    putStrLn $ unwords [ "PTR:", inYellow ptr, str ]
    putStrLn $ "     " ++ unwords [ "PREV:", inMagenta p, "NEXT:", inMagenta n ]
    when (n /= nullPtr) $ ppList n
  where
    inYellow  = highlight [ Foreground Yellow  ] . show
    inMagenta = highlight [ Foreground Magenta ] . show

main :: IO ()
main = do
    let list = packAlpmList $ map show [1..9]
    withForeignPtr list ppList
