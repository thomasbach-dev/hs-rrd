-- |
-- Copyright:  (c) Thomas Bach, 2022
-- License: BSD-3
--
-- The low-level bindings to @[librrd](https://oss.oetiker.ch/rrdtool/doc/librrd.en.html)@. Sadly
-- the documentation there is pretty scarce. So the best option currently is to redefine things from
-- the header file @[rrd.h](https://github.com/oetiker/rrdtool-1.x/blob/master/src/rrd.h)@.
--
-- I follow suggestions from [rrdthreads](https://oss.oetiker.ch/rrdtool/prog/rrdthreads.en.html)
-- and import the @_r@ suffixed variants where applicable.
{-# LANGUAGE CApiFFI #-}
module RRDTool.LowLevel
  ( -- ** Error Handling
    rrd_clear_error
  , rrd_test_error
  , rrd_get_error
    -- ** Utils
  , rrd_strversion
  , rrd_version
  ) where

import Foreign.C.String (CString, peekCString)

-- | Get the current @librrd@ version encoded as a @'Double'@ e.g. @1.7002@.
foreign import capi unsafe "rrd.h rrd_version" rrd_version :: Double

foreign import capi "rrd.h rrd_clear_error" rrd_clear_error :: IO ()

foreign import capi "rrd.h rrd_test_error" rrd_test_error :: Int

foreign import capi "rrd.h rrd_get_error" raw_rrd_get_error :: CString

rrd_get_error :: IO String
rrd_get_error = peekCString raw_rrd_get_error

foreign import capi unsafe "rrd.h rrd_strversion" raw_rrd_strversion :: CString

-- | Get the current @librrd@ version encdoded as a @'String'@ e.g. @"1.7.2"@.
rrd_strversion :: IO String
rrd_strversion = peekCString raw_rrd_strversion
