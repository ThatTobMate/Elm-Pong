module Game exposing (..)

import Color exposing (..)
import Collage exposing (..)
import Element exposing (..)
import Html exposing (..)
import Time exposing (..)
import Window exposing (Size)
import Random exposing (int, step, initialSeed)
import Text
import AnimationFrame
import Task
import Keyboard

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

-- MESSAGES
type Msg
  = Resize Size
  | Player Int
  | Computer Int
  | Frames Time
  | StopStart
  | NoOp


-- UPDATE
update : Msg -> Model -> Model
update msg model =
  case msg of
    NoOp -> model
    Resize size -> { model | size = size }
    -- We want to update the players direction state when we recieve the Paddle message. 
    -- The syntax below shows us returning new state where we copy the current model state, and edit the player record. Within the player record we only want to update the direction.
    Player direction ->
      let
        { player } = model
      in
        { model | player = { player | direction = direction } }

    Computer direction ->
      let
        { computer } = model
      in
        { model | computer = { computer | direction = direction } }

    StopStart ->
      let newState =
        case model.state of
          Play -> Pause
          Pause -> Play
      in   
        { model | state = newState }

    Frames delta ->
      let
        { state, player, computer, ball } = model

        playerPoint = 
          if ball.y > halfHeight then 1 else 0

        computerPoint =
          if ball.y < -halfHeight then 1 else 0

        newState =
          if playerPoint /= computerPoint then Pause else state

        newBall =
          if state == Pause then
            ball
          else
            moveBall delta ball player computer
      in
        { model | 
          state = newState
        , ball = newBall
        , player = movePaddle delta playerPoint player
        , computer = moveComputer delta computerPoint computer newBall
        }

moveBall: Time -> Ball -> Paddle -> Paddle -> Ball
moveBall delta ball player computer =
  if not (near 0 halfHeight ball.y) then
    { ball | x = 0, y = 0 }
  else 
    handleCollisions 
      delta
      { ball |
        vy = handleVelocity ball.vy (within player ball) (within computer ball)
      , vx = handleVelocity ball.vx (ball.x < 7 - halfWidth) (ball.x > halfWidth - 7)
      }

fst : ( a, b ) -> a
fst (a, _) = a

getInt : Float -> Int
getInt t = round t

initSeed : Int -> Random.Seed
initSeed x = initialSeed x

getRand : Int -> ( Int, Random.Seed )
getRand x = step (int 3 35) (initSeed x)

moveComputer : Time -> Int -> Paddle -> Ball -> Paddle
moveComputer delta point computer ball =
  let
    movedComputer =
      handleCollisions delta { computer | vx = toFloat -1 * computer.speed}
    rand =
      fst (getRand (getInt (Time.inMilliseconds delta)))
  in
    { movedComputer |
      x = clamp (22 - halfWidth) (halfWidth - 22) (ball.x + toFloat rand)
    , score = computer.score + point  
    }

movePaddle : Time -> Int -> Paddle -> Paddle
movePaddle delta point player =
  let
    movedPaddle =
      handleCollisions delta { player | vx = toFloat player.direction * player.speed }
  in
    { movedPaddle |
      x = clamp (22 - halfWidth) (halfWidth - 22) movedPaddle.x
    , score = player.score + point
    }

near: Float -> Float -> Float -> Bool
near paddle threshold ball =
  ball >= paddle - threshold && ball <= paddle + threshold

within: Paddle -> Ball -> Bool
within paddle ball =
  near paddle.y 8 ball.y && near paddle.x 20 ball.x


handleCollisions :
  number 
  -> { a | vx : number, vy : number, x : number, y : number }
  -> { a | vy : number, vx : number, x : number, y : number }
handleCollisions delta obj =
  {obj |
    x = obj.x + obj.vx * delta
  , y = obj.y + obj.vy * delta
  }


handleVelocity: Float -> Bool -> Bool -> Float
handleVelocity v leftBumper rightBumper =
  if leftBumper then
    abs v
  else if rightBumper then
    -(abs v)
  else
    v

handleInput : Bool -> number -> Msg
handleInput down keyCode =
  case (down, keyCode) of
    (True, 39) -> Player 1
    (True, 37) -> Player -1
    (False, 39) -> Player 0
    (False, 37) -> Player 0

    (False, 32) -> StopStart
    _ -> NoOp


-- SUBSCRIPTIONS
subscriptions: Model -> Sub Msg
subscriptions model =
  Sub.batch
    [ Window.resizes Resize
    , Keyboard.downs (handleInput True)
    , Keyboard.ups (handleInput False)
    , AnimationFrame.diffs (Frames << inSeconds)
    ]

-- VIEW

view: Model -> Html Msg
view model =
  let 
    { state, ball, player, computer } = model
    { width, height } = model.size
    scores =
      formatTxt (Text.height 50) (toString player.score ++ " : " ++ toString computer.score)
  in
    toHtml <|
    container width height middle <|
    collage gameWidth gameHeight
      [ rect gameWidth gameHeight
          |> filled courtOrange
      --, (rect gameWidth 5, black)
      --    |> make { x = 0, y = 0 }
      , (oval 15 15, yellow)
          |> make ball
      , (rect 40 10, white)
          |> make player
      , (rect 40 10, white)
          |> make computer
      , toForm scores
          |> move (0, halfWidth/2)
      , toForm (if state == Play then spacer 1 1 else formatTxt identity msg)
          |> move (0, 10 - gameHeight/2)
      ]

courtOrange : Color
courtOrange =
  rgb 214 124 66

formatTxt : (Text.Text -> Text.Text) -> String -> Element
formatTxt f string =
  Text.fromString string
    |> Text.color white
    |> Text.monospace
    |> f
    |> leftAligned

msg : String
msg = "SPACE to start and &larr;&rarr; to move"

make : { a | x : Float, y : Float } -> ( Shape, Color ) -> Form
make obj (shape, color) =
  shape
    |> filled color
    |> move (obj.x, obj.y)
