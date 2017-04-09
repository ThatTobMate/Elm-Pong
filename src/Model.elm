module Model exposing (..)

import Window exposing (Size)
import Game exposing (Resize)

-- MODEL

(gameWidth, gameHeight) = (400, 600)
(halfWidth, halfHeight) = (200, 300)

type State = Play | Pause

-- Make a record that the player and ball types can extend from. Contians the coordinates and the velocity for the Paddle and the Ball

type alias Base extra =
  { extra |
    x : Float
  , y : Float
  , vx : Float
  , vy : Float
  }

-- Use the Base type
type alias Ball =
  { x : Float
  , y : Float
  , vx : Float
  , vy : Float
  }

-- Extend the Base type and add direction and score keys
type alias Paddle =
  { x : Float
  , y : Float
  , vx : Float
  , vy : Float
  , direction : Int
  , score : Int
  , speed : Float
  }

-- Create the Model for our game
type alias Model =
  { state : State
  , ball : Ball
  , player : Paddle
  , computer : Paddle
  , size : Size
  }

-- Function to set the players start position on the x axis
player: Float -> Paddle
player y =
  Paddle 0 y 0 0 0 0 600


defaultModel : Model
defaultModel =
  { state = Pause
  , ball = Ball 0 0 200 200
  , player = player (20 - halfHeight)
  , computer = player (halfHeight - 20)
  , size = Size 0 0
  }

init: (Model, Cmd Msg)
init =
  (defaultModel, Task.perform Resize Window.size)