module Main exposing (..)

import Html exposing (..)
import Game exposing (..)

-- MAIN

main = 
  Html.program
    { init = Game.init
    , update = \msg model -> (Game.update msg model, Cmd.none)
    , view = Game.view 
    , subscriptions = Game.subscriptions
    }