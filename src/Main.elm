module Main exposing (..)

import Angle exposing (Angle)
import Browser
import Browser.Dom
import Browser.Events
import Camera3d
import Color
import Direction3d
import File exposing(File)
import File.Select as Select
import Html exposing (Html, button, p, text, div)
import Html.Attributes exposing (rows, cols, style, type_, checked, placeholder, value, hidden)
import Html.Events exposing (onClick)
import Illuminance
import LuminousFlux exposing (LuminousFlux)
import Json.Decode as Decode exposing (Decoder)
import Length exposing (Meters)
import Pixels exposing (Pixels)
import Point3d
import Quantity exposing (Quantity)
import Scene3d exposing (Entity)
import Scene3d.Light as Light exposing (Chromaticity, Light)
import Scene3d.Material as Material
import Scene3d.Mesh as Mesh exposing (Mesh)
import SolidAngle
import Sphere3d
import Task
import Temperature
import Triangle3d
import Viewpoint3d
import Ply exposing (parsePly)
import Array
import Point3d exposing (Point3d)
import Vector3d exposing (Vector3d)
import TriangularMesh exposing (TriangularMesh)
import Math.Vector3 exposing (Vec3, getX, getY, getZ)
import Ply exposing (PlyModel)
import Edgebreaker as EB
import Html.Attributes exposing (height)
import Ply exposing (emptyPly)
import Html exposing (textarea)
import Html exposing (hr)

type WorldCoordinates
    = WorldCoordinates

-- MODEL
type alias Model =
  { width : Quantity Int Pixels
  , height : Quantity Int Pixels
  , azimuth : Angle
  , elevation : Angle
  , orbiting : Bool
  , mesh1 : Mesh.Uniform WorldCoordinates
  , ply : Maybe String
  , filename : String
  , centerX : Float
  , centerY : Float
  , centerZ : Float
  , savedPly : Ply.PlyModel
  , clersRes : String
  }

init : () -> ( Model, Cmd Msg )
init () =
  ( { width = Quantity.zero
    , height = Quantity.zero
    , azimuth = Angle.degrees 45
    , elevation = Angle.degrees 30
    , orbiting = False
    , mesh1 = Mesh.facets [Triangle3d.from (Point3d.meters 0 0 0) (Point3d.meters 1 0 0) (Point3d.meters 1 1 0) ]
    , ply = Nothing
    , filename = ""
    , centerX = 0
    , centerY = 0
    , centerZ = 0
    , savedPly = emptyPly
    , clersRes = ""
    }
  , Task.perform
      (\{ viewport } ->
          Resize
              (Pixels.int (round viewport.width))
              (Pixels.int (round viewport.height))
      )
      Browser.Dom.getViewport
  )

parsePlyToMesh : String -> (Maybe PlyModel, Mesh.Uniform WorldCoordinates)
parsePlyToMesh ply = case parsePly ply of
  Nothing -> (Nothing, Mesh.facets [Triangle3d.from (Point3d.meters 0 0 0) (Point3d.meters 1 0 0) (Point3d.meters 1 1 0) ])
  Just x ->
    let
      extractVert v =
        { position = Point3d.meters (getX v.position) (getY v.position) (getZ v.position)
        , normal = Vector3d.unitless (getX v.normal) (getY v.normal) (getZ v.normal)
        }
      verts = List.map extractVert x.verts |> Array.fromList
      faces = List.map (\f -> f.verts) x.faces
    in  (Just x,TriangularMesh.indexed verts faces |> Mesh.indexedFaces)

type Msg
  = Resize (Quantity Int Pixels) (Quantity Int Pixels)
  | PlyRequested
  | PlySelected File
  | PlyLoaded String
  | MouseDown
  | MouseUp
  | MouseMove (Quantity Float Pixels) (Quantity Float Pixels)
  | EdgeBreak

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Resize width height -> ( { model | width = width, height = height }, Cmd.none )
    PlyRequested -> (model, Select.file [] PlySelected)
    PlySelected file -> ({model | filename= File.name file}, Task.perform PlyLoaded (File.toString file) )
    PlyLoaded content ->
      let (plyOut, meshOut) = parsePlyToMesh content
      in case plyOut of
        Nothing -> ( { model | mesh1 = meshOut, ply = Just content }, Cmd.none)
        Just p -> ( { model | mesh1 = meshOut
                    , ply = Just content
                    , savedPly = p
                    , centerX = getX p.center
                    , centerY = getY p.center
                    , centerZ = getZ p.center
                    }, Cmd.none)
      
    MouseDown -> ( { model | orbiting = True }, Cmd.none )
    MouseUp -> ( { model | orbiting = False }, Cmd.none )
    MouseMove dx dy ->
        if model.orbiting then
            let rotationRate = Angle.degrees 0.1 |> Quantity.per Pixels.pixel
                newAzimuth = model.azimuth |> Quantity.minus (dx |> Quantity.at rotationRate)
                newElevation = model.elevation
                  |> Quantity.plus (dy |> Quantity.at rotationRate)
                  |> Quantity.clamp (Angle.degrees -180) (Angle.degrees 180)
            in
            ( { model | azimuth = newAzimuth, elevation = newElevation }, Cmd.none)
        else ( model, Cmd.none )
    EdgeBreak ->
      case EB.compressPly model.savedPly of
          Err e -> ( {model| clersRes = e}, Cmd.none )
          Ok res -> ( {model| clersRes = res.clers |> List.reverse |> String.fromList}, Cmd.none )

decodeMouseMove : Decoder Msg
decodeMouseMove =
    Decode.map2 MouseMove
        (Decode.field "movementX" (Decode.map Pixels.float Decode.float))
        (Decode.field "movementY" (Decode.map Pixels.float Decode.float))

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.batch
    [ Browser.Events.onResize (\width height -> Resize (Pixels.int width) (Pixels.int height))
    , if model.orbiting then
          Sub.batch
              [ Browser.Events.onMouseMove decodeMouseMove
              , Browser.Events.onMouseUp (Decode.succeed MouseUp)
              ]
      else Browser.Events.onMouseDown (Decode.succeed MouseDown)
    ]

view : Model -> Browser.Document Msg
view model =
  case model.ply of
    Nothing ->
      { title = "Please select a Ply file"
      , body = 
        [ div 
          [ style "position" "absolute"
          , style "top" "5px"
          , style "left" "5px"
          , style "z-index" "3"
          ]
          [ button [ onClick PlyRequested] [ text "Load PLY" ] ]
        ]
      }
      
    Just _ ->
      let
        viewpoint =
            Viewpoint3d.orbitZ
                { focalPoint = Point3d.meters model.centerX model.centerY model.centerZ
                , azimuth = model.azimuth
                , elevation = model.elevation
                , distance = Length.meters 3
                }
        entityMaterial = Material.nonmetal {baseColor = Color.orange, roughness=0.1}
        plyEntity = Scene3d.mesh entityMaterial model.mesh1
        softLighting =
          Light.overhead
            { upDirection = Direction3d.negativeZ
            , chromaticity = Light.colorTemperature (Temperature.kelvins 1500)
            , intensity = Illuminance.lux 70
            }
        camera =
          Camera3d.perspective
            { viewpoint = viewpoint
            , verticalFieldOfView = Angle.degrees 50
            }
      in
      { title = "Viewing " ++ model.filename
      , body =
        [ div 
          [ style "position" "absolute"
          , style "top" "5px"
          , style "left" "5px"
          , style "z-index" "3"
          ]
          [ button [ onClick PlyRequested] [ text "Load PLY" ]
          , button [ onClick EdgeBreak] [ text "Try Edgebreak" ]
          , hr [] []
          , textarea [rows 10, cols 60, value model.clersRes] []
          ]
        , Scene3d.custom
          { camera = camera
          , clipDepth = Length.meters 0.1
          , dimensions = ( model.width, model.height )
          , background = Scene3d.backgroundColor Color.lightBlue
          , entities = [ plyEntity]
          , lights = Scene3d.oneLight softLighting
          , exposure = Scene3d.exposureValue 5
          , whiteBalance = Light.incandescent
          , antialiasing = Scene3d.supersampling 3
          , toneMapping = Scene3d.noToneMapping
          }
        ]
      }

main : Program () Model Msg
main =
  Browser.document
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }