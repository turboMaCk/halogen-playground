module Main where

import Prelude

import Control.Monad.Aff (Aff)
import Control.Monad.Eff (Eff)
import Control.Monad.Aff.Console (log)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Free (foldFree)

import Halogen.Aff (HalogenEffects, awaitBody, runHalogenAff)
import Halogen.VDom.Driver (runUI)
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH

import Counter as Counter


ui' :: forall eff. H.Component HH.HTML Counter.Query Unit Void (Aff (HalogenEffects (console :: CONSOLE | eff)))
ui' = H.hoist (foldFree evalMyAlgebra) Counter.ui
  where
  evalMyAlgebra :: Counter.MyAlgebra ~> Aff (HA.HalogenEffects (console :: CONSOLE | eff))
  evalMyAlgebra (Counter.NoOp next) = pure next
  evalMyAlgebra (Counter.Log msg next) = log msg *> pure next


main :: Eff (HalogenEffects (console :: CONSOLE)) Unit
main = runHalogenAff do
  body <- awaitBody
  runUI ui' unit body
