module ProcLib.Types.RunProcOpts
  ( RunProcOpts( RunProcOpts ), HasRunProcOpts( dryRunL, verboseL )
  , defOpts, defRunProcOpts )
where

-- base --------------------------------

import Numeric.Natural  ( Natural )

-- data-default ------------------------

import Data.Default  ( Default( def ) )

-- lens --------------------------------

import Control.Lens.Lens  ( Lens', lens )

--------------------------------------------------------------------------------

class HasRunProcOpts α where
  dryRunL  :: Lens' α Natural
  verboseL :: Lens' α Natural

data RunProcOpts = RunProcOpts { _dryRunL  :: Natural
                               , _verboseL :: Natural }

instance HasRunProcOpts RunProcOpts where
  dryRunL  = lens _dryRunL  (\ r d -> r { _dryRunL = d })
  verboseL = lens _verboseL (\ r v -> r { _verboseL = v })

-- | default RunProcOpts, with 0 verbosity and 0 mock-level.
defRunProcOpts :: RunProcOpts
defRunProcOpts = RunProcOpts 0 0

-- | default RunProcOpts, shorter name for convenience
defOpts :: RunProcOpts
defOpts = defRunProcOpts

instance Default RunProcOpts where
  def = defRunProcOpts

-- that's all, folks! ----------------------------------------------------------
