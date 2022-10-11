{-# LANGUAGE CApiFFI #-}
module RRDTool.LowLevel
  ( rrd_strversion
  , rrd_version
    -- ** Error Handling
  , rrd_clear_error
  , rrd_test_error
  , rrd_get_error
  ) where

import Foreign.C.String (CString, peekCString)

foreign import capi "rrd.h rrd_strversion" raw_rrd_strversion :: CString

rrd_strversion :: IO String
rrd_strversion = peekCString raw_rrd_strversion

foreign import capi "rrd.h rrd_version" rrd_version :: Double

foreign import capi "rrd.h rrd_clear_error" rrd_clear_error :: IO ()

foreign import capi "rrd.h rrd_test_error" rrd_test_error :: Int

foreign import capi "rrd.h rrd_get_error" raw_rrd_get_error :: CString

rrd_get_error :: IO String
rrd_get_error = peekCString raw_rrd_get_error
