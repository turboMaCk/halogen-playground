module Counter (Query(..), MyAlgebra(..), ui) where

import Prelude

import Control.Monad.Free (Free, liftF)

import Data.Maybe (Maybe(..))

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE


type State = { count :: Int }


data Query a = Increment a
             | Decrement a


data MyAlgebra a = Log String a
                 | NoOp a

type MyMonad = Free MyAlgebra


output :: String -> MyMonad Unit
output msg = liftF (Log msg unit)

noOp :: MyMonad Unit
noOp = liftF (NoOp unit)


ui :: H.Component HH.HTML Query Unit Void MyMonad
ui =
  H.component
    { initialState : const initialState
    , render
    , eval
    , receiver : const Nothing
    }
  where

  initialState :: State
  initialState = { count: 0 }

  render :: State -> H.ComponentHTML Query
  render state =
    HH.div_
      [ HH.h1_
          [ HH.text "Toggle Button" ]
      , HH.button
          [ HE.onClick (HE.input_ Increment) ]
          [ HH.text "+" ]
      , HH.text $ show state.count
      , HH.button
        [ HE.onClick (HE.input_ Decrement) ]
        [ HH.text "-" ]
      ]

  eval :: Query ~> H.ComponentDSL State Query Void MyMonad
  eval (Increment next) = do
    H.modify (\state -> { count: state.count + 1 })
    H.lift $ output "State was incremented"
    pure next
  eval (Decrement next) = do
    H.modify (\state -> { count: state.count - 1 })
    H.lift $ output "State was decremented"
    pure next
