module Rectangle exposing (..)

import Browser
import Browser.Events as E
import Html exposing (Html)
import Html.Attributes exposing (width, height, style)
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
  Float

init : () -> (Model, Cmd Msg)
init () =
    ( 0, Cmd.none )


-- UPDATE


type Msg
  = TimeDelta Float


update : Msg -> Model -> (Model, Cmd Msg)
update msg angle =
  case msg of
    TimeDelta dt ->
      ( angle + dt / 5000, Cmd.none )


-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
  E.onAnimationFrameDelta TimeDelta


-- VIEW


view : Model -> Html Msg
view t =
  WebGL.toHtml
    [ width 400, height 400, style "display" "block"
    ]
    [ WebGL.entity vertexShader fragmentShader rectangleMesh uniforms
    ]


type alias Uniforms =
  { perspective : Mat4
  , camera : Mat4
  }


uniforms =
  { perspective = Mat4.makePerspective 45 1 0.01 100
  , camera = Mat4.makeLookAt (vec3 0 0 5) (vec3 0 0 0) (vec3 0 1 0)
  }


-- MESH


type alias Vertex =
  { color : Vec3
  , position : Vec3
  }


rectangleMesh : WebGL.Mesh Vertex
rectangleMesh =
    let
        leftU = Vertex (vec3 1 0 0) (vec3 -1 1 0)
        rightU = Vertex (vec3 1 0 0) (vec3 1 1 0)
        rightB = Vertex (vec3 1 0 0) (vec3 1 -1 0)
        leftB = Vertex (vec3 1 0 0) (vec3 -1 -1 0)
    in
    WebGL.triangles
    [ (leftU, rightU, leftB)
    , (leftB, rightU, rightB)
    ]


-- SHADERS


vertexShader : WebGL.Shader Vertex Uniforms { vcolor : Vec3 }
vertexShader =
  [glsl|
    attribute vec3 position;
    attribute vec3 color;
    uniform mat4 perspective;
    uniform mat4 camera;
    varying vec3 vcolor;
    void main () {
        gl_Position = perspective * camera * vec4(position, 1.0);
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
