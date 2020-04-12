module ClickOnCanvas exposing (..)

import Browser
import Browser.Events as E
import Browser.Dom as Dom
import Task
import Html exposing (Html)
import Html.Attributes exposing (id, width, height, style)
import Json.Decode as Decode exposing (Decoder, Value)
import Math.Matrix4 as Mat4 exposing (Mat4)
import Math.Vector3 exposing (Vec3, vec3)
import WebGL


-- MAIN
main =
  Browser.element
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }


-- MODEL


type alias Model =
  { point : Vec3
  , canvas : {x : Float, y : Float, width : Float, height : Float}
  }

init : () -> (Model, Cmd Action)
init () =
    ({point = (vec3 0 0 0)
    , canvas = {x = 0, y = 0, width = 0, height = 0}}, getCanvas )


-- UPDATE


type Action
    = MouseClick { x : Float, y : Float }
    | Canvas (Result Dom.Error Dom.Element)
    | Resize

update : Action -> Model -> (Model, Cmd Action)
update action model =
    case action of
        MouseClick position ->
            if (isClickOnCanvas model position.x position.y) then
                let
                    coord = mouseCoordToWorldCoord model (position.x, position.y)
                in
                    ({model | point = (vec3 (Tuple.first coord) (Tuple.second coord) 0)}, Cmd.none)
            else
                (model, Cmd.none)
        Canvas canvas ->
            case canvas of
                Ok el ->
                    ({model | canvas = {x = el.element.x
                    , y = el.element.y
                    , width = el.element.width
                    , height = el.element.height}}, Cmd.none)
                Err _ ->
                    (model, Cmd.none)
        Resize ->
            (model, getCanvas)

mouseCoordToWorldCoord : Model -> (Float, Float) -> (Float, Float)
mouseCoordToWorldCoord model (mx, my) =
    (mx - (200 + model.canvas.x), (my - (200 + model.canvas.y)) * -1)

isClickOnCanvas : Model -> Float -> Float -> Bool
isClickOnCanvas model mouseX mouseY =
    (mouseX > model.canvas.x && mouseX < (model.canvas.x + model.canvas.width) &&
    mouseY > model.canvas.y && mouseY < (model.canvas.y + model.canvas.height))


-- SUBSCRIPTIONS


subscriptions : Model -> Sub Action
subscriptions _ =
    Sub.batch
    [ E.onClick mousePosition
    , E.onResize (\_ _ -> Resize)
    ]

mousePosition : Decoder Action
mousePosition =
    Decode.map2(\x y -> MouseClick { x = x, y = y})
        (Decode.field "pageX" Decode.float)
        (Decode.field "pageY" Decode.float)

getCanvas :  Cmd Action
getCanvas =
    Dom.getElement "canvas"
            |> Task.attempt Canvas

-- VIEW


view : Model -> Html Action
view model =
  WebGL.toHtmlWith
    [ WebGL.clearColor 0 0 0 1 ]
    [ id "canvas", width 400, height 400, style "display" "block"
    ]
    [ WebGL.entity vertexShader fragmentShader (points model.point) uniforms
    ]

type alias Uniforms =
  { perspective : Mat4
  , camera : Mat4
  , modelMat : Mat4
  }


uniforms =
  { perspective = Mat4.makeOrtho2D -200 200 -200 200
  , camera = Mat4.makeLookAt (vec3 0 0 3) (vec3 0 0 0) (vec3 0 1 0)
  , modelMat = Mat4.scale (vec3 (1/200) (1/200) 0) Mat4.identity
  }


-- MESH


type alias Vertex =
  { color : Vec3
  , position : Vec3
  }

points : Vec3 -> WebGL.Mesh Vertex
points pos =
    let
        p1 = Vertex (vec3 0 1 0) pos
    in
    WebGL.points
    [ p1 ]


-- SHADERS


vertexShader : WebGL.Shader Vertex Uniforms { vcolor : Vec3 }
vertexShader =
  [glsl|
    attribute vec3 position;
    attribute vec3 color;
    uniform mat4 perspective;
    uniform mat4 camera;
    uniform mat4 modelMat;
    varying vec3 vcolor;
    void main () {
        gl_Position = perspective *  vec4(position, 1.0);
        gl_PointSize = 12.0;
        vcolor = color;
    }
  |]

fragmentShader : WebGL.Shader {} Uniforms { vcolor : Vec3 }
fragmentShader =
  [glsl|
    precision mediump float;
    varying vec3 vcolor;
    void main () {
        gl_FragColor = 0.8 * vec4(vcolor, 1.0);
    }
  |]
