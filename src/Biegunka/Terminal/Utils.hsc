{-# LANGUAGE ForeignFunctionInterface #-}
module Biegunka.Terminal.Utils where

import Foreign
import Foreign.C.Error
import Foreign.C.Types

#include <sys/ioctl.h>
#include <unistd.h>


#let alignment t = "%lu", (unsigned long)offsetof(struct {char x__; t (y__); }, y__)


data CWinSize = CWinSize { wsRow, wsCol :: CUShort }

instance Storable CWinSize where
  sizeOf _ = (#size struct winsize)
  alignment _ = (#alignment struct winsize) 
  peek ptr = do
    row <- (#peek struct winsize, ws_row) ptr
    col <- (#peek struct winsize, ws_col) ptr
    return $ CWinSize row col
  poke ptr (CWinSize row col) = do
    (#poke struct winsize, ws_row) ptr row
    (#poke struct winsize, ws_col) ptr col


data WinSize a = WinSize
  { height :: !a
  , width  :: !a
  } deriving (Show, Read)

instance Functor WinSize where
  fmap f (WinSize { height = h, width = w }) = WinSize { height = f h, width = f w }


-- | Terminal width and height
size :: Integral n => IO (WinSize n)
size = with (CWinSize 0 0) $ \ws -> do
  throwErrnoIfMinus1 "ioctl" $
    ioctl (#const STDOUT_FILENO) (#const TIOCGWINSZ) ws
  CWinSize row col <- peek ws
  return $ WinSize (fromIntegral row) (fromIntegral col)

foreign import ccall "sys/ioctl.h ioctl"
  ioctl :: CInt -> CInt -> Ptr CWinSize -> IO CInt
