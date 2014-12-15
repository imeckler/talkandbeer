module Graph where

import Dict(..)

type Graph v e = Dict Int (v, Dict Int e)

