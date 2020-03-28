module Octahedron exposing (..)

import Browser
import Browser.Events as E
import Html exposing (Html)
import Html.Attributes exposing (width, height, style)
import Math.Matrix4 as Mat4 exposing (Mat4)
import Math.Vector3 exposing (Vec3, vec3)
import WebGL
import Color exposing (Color)

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
    (0, Cmd.none)


-- UPDATE

type Msg
    = TimeDelta Float


update : Msg -> Model -> (Model, Cmd Msg)
update msg angle =
  case msg of
    TimeDelta dt ->
      ( angle + dt / 5000, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions _ =
  E.onAnimationFrameDelta TimeDelta


-- VIEW

view : Model -> Html Msg
view theta =
  WebGL.toHtml
    [ width 400, height 400, style "display" "block"
    ]
    (scene theta)

scene : Model -> List WebGL.Entity
scene angle =
    [ WebGL.entity vertexShader fragmentShader (octahedronMesh Color.green) (uniforms 0 0 angle)
    ]

type alias Uniforms =
  { translation : Mat4
  , rotation : Mat4
  , perspective : Mat4
  , camera : Mat4
  }

uniforms : Float -> Float -> Float -> Uniforms
uniforms x y angle =
  { translation =
        (Mat4.translate (vec3 x y 0) Mat4.identity)
  , rotation =
        Mat4.mul
            (Mat4.makeRotate (3 * angle) (vec3 0 0 1))
            (Mat4.makeRotate (2 * angle) (vec3 1 0 0))
  , perspective = Mat4.makePerspective 45 1 0.01 100
  , camera = Mat4.makeLookAt (vec3 0 0 5) (vec3 0 0 0) (vec3 0 1 0)
  }


-- MESH

type alias Vertex =
  { color : Vec3
  , position : Vec3
  }

octahedronMesh : Color -> WebGL.Mesh Vertex
octahedronMesh color =
    let
        rgba = Color.toRgba color

        verticesFromVec3 (a, b, c) =
            ( Vertex (vec3 rgba.red rgba.green rgba.blue) a
            , Vertex (vec3 rgba.green rgba.red rgba.blue) b
            , Vertex (vec3 rgba.blue rgba.green rgba.red) c)
    in
    WebGL.triangles (List.map verticesFromVec3 octahedronTriangles)

octahedronTriangles : List (Vec3, Vec3, Vec3)
octahedronTriangles  =
    let
        a = 1.0 / sqrt 2
        b = 1
    in
    [ (vec3 -a 0 -a, vec3 -a 0 a, vec3 0 b 0)
    , (vec3 a 0 -a, vec3 -a 0 -a, vec3 0 b 0)
    , (vec3 a 0 a, vec3 a 0 -a, vec3 0 b 0)
    , (vec3 -a 0 a, vec3 a 0 a, vec3 0 b 0)
    , (vec3 -a 0 -a, vec3 a 0 -a, vec3 0 -b 0)
    , (vec3 -a 0 a, vec3 -a 0 -a, vec3 0 -b 0)
    , (vec3 a 0 -a, vec3 a 0 a, vec3 0 -b 0)
    , (vec3 a 0 a, vec3 -a 0 a, vec3 0 -b 0)
    ]



-- SHADER

vertexShader : WebGL.Shader Vertex Uniforms { vcolor : Vec3 }
vertexShader =
  [glsl|
    attribute vec3 position;
    attribute vec3 color;
    uniform mat4 perspective;
    uniform mat4 camera;
    uniform mat4 translation;
    uniform mat4 rotation;
    varying vec3 vcolor;
    void main () {
        gl_Position = perspective * camera * translation * rotation * vec4(position, 1.0);
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

