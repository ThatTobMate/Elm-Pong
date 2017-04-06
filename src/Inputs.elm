module Inputs exposing (..)

import Keyboard
import Time exposing (Time, inSeconds, fps)

type alias Input =
  { space : Bool 
  , userPaddle: Int
  , computerPaddle: Int
  , delta: Time
  }

delta: Signal Time
delta =
  Signal.map inSeconds (fps 35)

input: Signal Input
input =
    Signal.sampleOn delta <|
      Signal.map3 Input
        Keyboard.space
        (Signal.map .y Keyboard.arrows)
        delta