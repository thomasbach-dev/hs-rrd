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
  ( rrd_get_context
  , rrd_get_context_raw
  , RRDContext
  , rrdContextLibErrstr
  , rrdContextRRDError
  , rrd_new_context
  , rrd_new_context_raw
    -- ** Error Handling
  , rrd_clear_error
  , rrd_test_error
  , rrd_get_error
    -- ** Utils
  , rrd_strversion
  , rrd_version
  ) where

import Foreign (Ptr, plusPtr, ForeignPtr, FunPtr, newForeignPtr, withForeignPtr)
import Foreign.C (CString, peekCString)

#include <rrd.h>

-- | “Every thread SHOULD call rrd_get_context() before its first call to any librrd_th function in
-- order to set up thread specific data.”
--
-- This sets 'rrd_free_context' as the finalizer to the result of 'rrd_get_context_raw'.
rrd_get_context :: IO (ForeignPtr RRDContext)
rrd_get_context = do
  ctx <- rrd_get_context_raw
  newForeignPtr rrd_free_context ctx

foreign import capi "rrd.h rrd_get_context" rrd_get_context_raw :: IO (Ptr RRDContext)

-- | As defined in [rrd.h](https://github.com/oetiker/rrdtool-1.x/blob/master/src/rrd.h#L356):
--
-- > typedef struct rrd_context {
-- >      char      lib_errstr[256];
-- >      char      rrd_error[4096];
-- >  } rrd_context_t;
data {-# CTYPE "rrd.h" "rrd_context_t" #-} RRDContext

-- | Get the @lib_errstr@ part of the given 'RRDContext'.
rrdContextLibErrstr :: ForeignPtr RRDContext -> IO String
rrdContextLibErrstr fPtr =
  withForeignPtr fPtr (\ptr -> peekCString $ #{ptr rrd_context_t, lib_errstr} ptr)

-- | Get the @rrd_error@ part of the given 'RRDContext'.
rrdContextRRDError :: ForeignPtr RRDContext -> IO String
rrdContextRRDError fPtr =
  withForeignPtr fPtr (\ptr -> peekCString $ #{ptr rrd_context_t, rrd_error} ptr)

foreign import capi "rrd.h &rrd_free_context" rrd_free_context :: FunPtr (Ptr RRDContext -> IO ())

-- | Allocate memory for a 'RRDContext' object. 'rrd_free_context' is set as a finalizer.
--
-- You most probably do not need this function!
rrd_new_context :: IO (ForeignPtr RRDContext)
rrd_new_context = rrd_new_context_raw >>= newForeignPtr rrd_free_context

foreign import capi "rrd.h rrd_new_context" rrd_new_context_raw :: IO (Ptr RRDContext)

foreign import capi "rrd.h rrd_clear_error" rrd_clear_error :: IO ()

foreign import capi "rrd.h rrd_test_error" rrd_test_error :: Int

foreign import capi "rrd.h rrd_get_error" rrd_get_error_raw :: CString

rrd_get_error :: IO String
rrd_get_error = peekCString rrd_get_error_raw

foreign import capi unsafe "rrd.h rrd_strversion" rrd_strversion_raw :: CString

-- | Get the current @librrd@ version encdoded as a @'String'@ e.g. @"1.7.2"@.
rrd_strversion :: IO String
rrd_strversion = peekCString rrd_strversion_raw

-- | Get the current @librrd@ version encoded as a @'Double'@ e.g. @1.7002@.
foreign import capi unsafe "rrd.h rrd_version" rrd_version :: Double
