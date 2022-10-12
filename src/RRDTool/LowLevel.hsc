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
  ( rrd_new_context
  , rrd_get_context
  , RRDContext(..)
    -- ** Error Handling
  , rrd_clear_error
  , rrd_test_error
  , rrd_get_error
    -- ** Utils
  , rrd_strversion
  , rrd_version
  ) where

import Foreign (Storable (..), Ptr)
import Foreign.C (CString, peekCString)

#include <rrd.h>

-- | As defined in @[rrd.h](https://github.com/oetiker/rrdtool-1.x/blob/master/src/rrd.h#L356):
--
-- > typedef struct rrd_context {
-- >      char      lib_errstr[256];
-- >      char      rrd_error[4096];
-- >  } rrd_context_t;
data {-# CTYPE "rrd.h" "rrd_context_t" #-} RRDContext = RRDContext
  { libErrstr :: String
  , rrdError  :: String
  } deriving (Eq, Show)

instance Storable RRDContext where
  sizeOf _ = #{size rrd_context_t}
  alignment _ = #{alignment rrd_context_t}
  peek ptr = do
    libErrstr <- peekCString =<< #{peek rrd_context_t, lib_errstr} ptr
    rrdError <-  peekCString =<< #{peek rrd_context_t, rrd_error} ptr
    pure RRDContext{..}
  poke _ = undefined

foreign import capi "rrd.h rrd_get_context" rrd_get_context_raw :: IO (Ptr RRDContext)

rrd_get_context :: IO RRDContext
rrd_get_context = rrd_get_context_raw >>= peek

foreign import capi "rrd.h rrd_new_context" rrd_new_context_raw :: IO (Ptr RRDContext)

rrd_new_context :: IO RRDContext
rrd_new_context = rrd_new_context_raw >>= peek

foreign import capi "rrd.h rrd_clear_error" rrd_clear_error :: IO ()

foreign import capi "rrd.h rrd_test_error" rrd_test_error :: Int

foreign import capi "rrd.h rrd_get_error" raw_rrd_get_error :: CString

rrd_get_error :: IO String
rrd_get_error = peekCString raw_rrd_get_error

foreign import capi unsafe "rrd.h rrd_strversion" raw_rrd_strversion :: CString

-- | Get the current @librrd@ version encdoded as a @'String'@ e.g. @"1.7.2"@.
rrd_strversion :: IO String
rrd_strversion = peekCString raw_rrd_strversion

-- | Get the current @librrd@ version encoded as a @'Double'@ e.g. @1.7002@.
foreign import capi unsafe "rrd.h rrd_version" rrd_version :: Double
