{-# LANGUAGE CApiFFI #-}
module RRDTool.LowLevel
  ( rrd_strversion
  , rrd_version
  ) where

import Foreign.C.String (CString, peekCString)

foreign import capi "rrd.h rrd_strversion" raw_rrd_strversion :: CString

rrd_strversion :: IO String
rrd_strversion = peekCString raw_rrd_strversion

foreign import capi "rrd.h rrd_version" rrd_version :: Double
