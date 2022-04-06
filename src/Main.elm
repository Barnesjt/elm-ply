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
import Html exposing (Html, button, p, text, div, textarea, hr, h2, h1)
import Html.Attributes exposing (rows, cols, style, type_, checked, placeholder, value, hidden, height)
import Html.Events exposing (onClick)
import Illuminance
import LuminousFlux exposing (LuminousFlux)
import Json.Decode as Decode exposing (Decoder)
import Length exposing (Meters)
import List exposing (singleton)
import Pixels exposing (Pixels)
import Point3d
import Quantity exposing (Quantity)
import Scene3d exposing (Entity)
import Scene3d.Light as Light exposing (Chromaticity, Light)
import Scene3d.Material as Material
import Scene3d.Mesh as Mesh exposing (Mesh)
import SolidAngle
import Sphere3d
import String
import Task
import Temperature
import Triangle3d
import Viewpoint3d
import Ply
import Array
import Point3d exposing (Point3d)
import Vector3d exposing (Vector3d)
import TriangularMesh exposing (TriangularMesh)
import Math.Vector3 exposing (Vec3, getX, getY, getZ)
import Edgebreaker as EB
import Dict exposing (Dict)
import SingleSlider
import Html exposing (a)
import Html.Attributes exposing (href)
import Html exposing (h3)
import Html exposing (Attribute)
import Dropdown
import Http
import List.Extra
import Html exposing (tr)


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
  , meshC : Mesh.Uniform WorldCoordinates
  , meshL : Mesh.Uniform WorldCoordinates
  , meshE : Mesh.Uniform WorldCoordinates
  , meshR : Mesh.Uniform WorldCoordinates
  , meshS : Mesh.Uniform WorldCoordinates
  , ply : Maybe String
  , filename : String
  , centerX : Float
  , centerY : Float
  , centerZ : Float
  , savedPly : Maybe Ply.PlyModel
  , ebRes : Maybe EB.EBRes
  , errorText : String
  , slider : SingleSlider.SingleSlider Msg
  , fov : Float
  , selectedPly : Maybe String
  , showClers : Bool
  , isLoading : Bool
  }

init : () -> ( Model, Cmd Msg )
init () =
  ( { width = Quantity.zero
    , height = Quantity.zero
    , azimuth = Angle.degrees 45
    , elevation = Angle.degrees 30
    , orbiting = False
    , mesh1 = Mesh.facets [Triangle3d.from (Point3d.meters 0 0 0) (Point3d.meters 1 0 0) (Point3d.meters 1 1 0) ]
    , meshC = Mesh.facets [Triangle3d.from (Point3d.meters 0 0 0) (Point3d.meters 1 0 0) (Point3d.meters 1 1 0) ]
    , meshL = Mesh.facets [Triangle3d.from (Point3d.meters 0 0 0) (Point3d.meters 1 0 0) (Point3d.meters 1 1 0) ]
    , meshE = Mesh.facets [Triangle3d.from (Point3d.meters 0 0 0) (Point3d.meters 1 0 0) (Point3d.meters 1 1 0) ]
    , meshR = Mesh.facets [Triangle3d.from (Point3d.meters 0 0 0) (Point3d.meters 1 0 0) (Point3d.meters 1 1 0) ]
    , meshS = Mesh.facets [Triangle3d.from (Point3d.meters 0 0 0) (Point3d.meters 1 0 0) (Point3d.meters 1 1 0) ]
    , ply = Nothing
    , filename = ""
    , centerX = 0
    , centerY = 0
    , centerZ = 0
    , savedPly = Nothing
    , ebRes = Nothing
    , errorText = ""
    , slider = fovSlider
    , fov = 50
    , selectedPly = Nothing
    , showClers = True
    , isLoading = False
    }
  , Task.perform
      (\{ viewport } ->
          Resize
              (Pixels.int (round viewport.width))
              (Pixels.int (round viewport.height))
      )
      Browser.Dom.getViewport
  )

homePageUrl : String
homePageUrl = "https://jackbarnes.dev/elm-ply/"

repositoryUrl : String
repositoryUrl = "https://github.com/barnesjt/elm-ply"

edgebreakerUrl : String
edgebreakerUrl = "https://faculty.cc.gatech.edu/~jarek/papers/EdgeBreaker.pdf"

edgebreakerCornerUrl : String
edgebreakerCornerUrl = "https://faculty.cc.gatech.edu/~jarek/papers/EdgeBreaker.pdf"

plyFiles : List (String,String)
plyFiles =
    [ ("Tetrahedron (4 faces)","https://jackbarnes.dev/elm-ply/ply-files/tetrahedron.ply")
    , ("Octahedron (8 faces)","https://jackbarnes.dev/elm-ply/ply-files/octahedron.ply")
    , ("Icosahedron (20 faces)","https://jackbarnes.dev/elm-ply/ply-files/icosahedron.ply")
    , ("Sphere (8192 faces)", "https://jackbarnes.dev/elm-ply/ply-files/sphere.ply")
    , ("Bunny (10k faces)", "https://jackbarnes.dev/elm-ply/ply-files/bunny.ply")
    , ("Dragon (20k faces, 1 handle)","https://jackbarnes.dev/elm-ply/ply-files/dragon.ply")
    , ("Feline (10k faces, 2 handles)","https://jackbarnes.dev/elm-ply/ply-files/feline.ply")
    , ("Buddha (20k faces, 6 handles)","https://jackbarnes.dev/elm-ply/ply-files/happy.ply")
    ]

plyItems : List Dropdown.Item
plyItems = List.map (\p -> {text= Tuple.first p, value=Tuple.second p, enabled=True}) plyFiles

fovSlider : SingleSlider.SingleSlider Msg
fovSlider =
  SingleSlider.init
    { min = 10
    , max = 100
    , value = 50
    , step = 1
    , onChange = FOVChange
    }
    |> SingleSlider.withMinFormatter (\_ -> "")
    |> SingleSlider.withMaxFormatter (\_ -> "")
    |> SingleSlider.withValueFormatter (\_ _ -> "")

parsePlyToMesh : String -> (Maybe Ply.PlyModel, Mesh.Uniform WorldCoordinates)
parsePlyToMesh ply = case Ply.parsePly ply of
  Nothing -> (Nothing, Mesh.facets [])
  Just x ->
    let
      extractVert v =
        { position = Point3d.meters (getX v.position) (getY v.position) (getZ v.position)
        , normal = Vector3d.unitless (getX v.normal) (getY v.normal) (getZ v.normal)
        }
      verts = List.map extractVert x.verts |> Array.fromList
      faces = List.map (\f -> f.verts) x.faces
    in  (Just x,TriangularMesh.indexed verts faces |> Mesh.indexedFaces)

plyToMesh : Ply.PlyModel -> Mesh.Uniform WorldCoordinates
plyToMesh ply =
  let
    extractVert v =
      { position = Point3d.meters (getX v.position) (getY v.position) (getZ v.position)
      , normal = Vector3d.unitless (getX v.normal) (getY v.normal) (getZ v.normal)
      }
    verts = List.map extractVert ply.verts |> Array.fromList
    faces = List.map (\f -> f.verts) ply.faces
  in  TriangularMesh.indexed verts faces |> Mesh.indexedFaces

type Msg
  = ResetApp
  | Resize (Quantity Int Pixels) (Quantity Int Pixels)
  | PlyRequested
  | PlySelected File
  | PlyLoaded String
  | MouseDown
  | MouseUp
  | MouseMove (Quantity Float Pixels) (Quantity Float Pixels)
  | EdgeBreak
  | FOVChange Float
  | DropdownChanged (Maybe String)
  | GetRemotePly
  | GotRemotePly (Result Http.Error String)
  | ToggleDisplay
  | Loading 

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    ResetApp -> init ()
    Resize width height -> ( { model | width = width, height = height }, Cmd.none )
    PlyRequested -> (model, Select.file [] PlySelected)
    PlySelected file -> ({model | isLoading = True, filename= File.name file, savedPly = Nothing, ebRes = Nothing}, Task.perform PlyLoaded (File.toString file) )
    PlyLoaded content ->
      let (plyOut, meshOut) = parsePlyToMesh content
      in case plyOut of
        Nothing -> ( { model | isLoading = False, mesh1 = meshOut, ply = Just content }, Cmd.none)
        Just p -> 
          let 
            newModel = 
              { model | mesh1 = meshOut
              , ply = Just content
              , savedPly = Just p
              , centerX = getX p.center
              , centerY = getY p.center
              , centerZ = getZ p.center
              , isLoading = False
              }
          in
          update EdgeBreak newModel
      
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
      case model.savedPly of
        Nothing -> ({model| errorText = "No Valid Ply Model Loaded"}, Cmd.none)
        Just m -> case EB.compressPly m of
          Err e -> ( {model| errorText = e}, Cmd.none )
          Ok res -> 
            let
              cPly = makePlyFromFaceInds res.origPly res.clersFaces.c
              lPly = makePlyFromFaceInds res.origPly res.clersFaces.l
              ePly = makePlyFromFaceInds res.origPly res.clersFaces.e
              rPly = makePlyFromFaceInds res.origPly res.clersFaces.r
              sPly = makePlyFromFaceInds res.origPly res.clersFaces.s
              cMesh = plyToMesh cPly
              lMesh = plyToMesh lPly
              eMesh = plyToMesh ePly
              rMesh = plyToMesh rPly
              sMesh = plyToMesh sPly

            in
            ( {model| ebRes = Just res, meshC = cMesh, meshL = lMesh, meshE = eMesh, meshR = rMesh, meshS = sMesh }, Cmd.none )
    FOVChange newFov ->
      let
        newSlider =
            SingleSlider.update newFov model.slider
      in
      ( {model | slider = newSlider, fov = newFov}, Cmd.none )
    DropdownChanged str ->
      update GetRemotePly {model | selectedPly = str}
    GetRemotePly ->
      case model.selectedPly of
        Nothing -> ( model, Cmd.none )
        Just "" -> ( model, Cmd.none )
        Just url -> 
          ({model | isLoading = True}, Http.get { url = url, expect = Http.expectString GotRemotePly})
    GotRemotePly (Err _) ->
      ( model, Cmd.none ) 
    GotRemotePly (Ok str) ->
      let 
        fName = case model.selectedPly of
          Nothing -> ""
          Just f -> case String.split "/" f |> List.Extra.last of
            Nothing -> f
            Just ply -> ply
      in
      update (PlyLoaded str) {model | filename= fName, savedPly = Nothing, ebRes = Nothing}
    ToggleDisplay ->
      ({model | showClers = not model.showClers}, Cmd.none)
    Loading -> (model, Cmd.none)

makePlyFromFaceInds : Ply.PlyModel -> List Int -> Ply.PlyModel
makePlyFromFaceInds ply inds = 
  let
    newPlyBase = Ply.PlyModel ply.verts [] [] ply.nvert (List.length inds) ply.minCoord ply.center ply.maxCoord
    getFace : Ply.PlyModel -> Int -> Maybe Ply.Face 
    getFace plyIn ind = List.Extra.getAt ind plyIn.faces

    getAllFaces plyIn fInds acc =
      case fInds of
        [] -> acc
        (ind::rest) -> case getFace plyIn ind of
          Nothing -> getAllFaces plyIn rest acc
          Just f -> getAllFaces plyIn rest {acc|faces=f::acc.faces}
  in getAllFaces ply inds newPlyBase
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
  let
    title = 
      div
        [ style "position" "absolute"
        , style "right" "5%"
        , style "z-index" "3"
        ]
        [ "EdgeBreaker" |> text |> singleton |> h1 [style "text-shadow" "2px 2px 4px white", style "font-size" "xxx-large"] ]
    links color shadow = 
      div
        [ style "position" "absolute"
        , style "bottom" "15px"
        , style "left" "50%"
        , style "transform" "translate(-50%, 0%)"
        , style "right" "0px"
        , style "z-index" "2"
        , style "display" "inline-flex"
        , style "justify-content" "space-around"
        ]
        [ homePageUrl |> text |> singleton |> a (hrefAndStyle homePageUrl color shadow) |> singleton |> div []
        , repositoryUrl |> text |> singleton |> a (hrefAndStyle repositoryUrl color shadow) |> singleton |> div [style "margin-left" "50px", style "margin-right" "50px"]
        , "Edgebreaker (pdf)" |> text |> singleton |> a (hrefAndStyle edgebreakerUrl color shadow) |> singleton |> div []
        ]
    reset =
      div
        [ style "position" "absolute"
        , style "right" "0%"
        , style "bottom" "0%"
        , style "transform" "translate(-10%, -50%)"
        ]
        [ button [onClick ResetApp, style "font-weight" "bold", style "font-size" "large"] [text "LOAD NEW MODEL (RESET)"]]
  in
  if model.isLoading then
    { title = "Please wait..."
    , body = 
      [ div
        [ style "position" "absolute"
        , style "top" "50%"
        , style "left" "50%"
        , style "transform" "translate(-50%, -50%)"
        , style "z-index" "3"
        , style "display" "grid"
        , style "justify-items" "center"
        , style "font-size" "xxx-large"
        , style "font-weight" "bold"
        , style "background-color" "light-blue"
        ]
        [text "Loading..."]
      ]
    }
  else 
  case model.ply of
    Nothing ->
      { title = "Please select a Ply file"
      , body = 
        [ div 
          [ style "position" "absolute"
          , style "top" "50%"
          , style "left" "50%"
          , style "transform" "translate(-50%, -50%)"
          , style "z-index" "3"
          , style "display" "grid"
          , style "justify-items" "center"
          ]
          [ button [ onClick PlyRequested] [ text "Load PLY from File" ] |> singleton |> div []
          , text "Or choose a file below:" |> singleton |> div [style "margin" "10%"]
          , Dropdown.dropdown {items = plyItems, emptyItem = Just {value = "", text = "Select a model to begin", enabled = True}, onChange = DropdownChanged} [] model.selectedPly |> singleton |> div []
          ]
          , title
          , links "black" False
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
        cMaterial = Material.matte (Color.rgb255 0 255 255)
        lMaterial = Material.matte (Color.rgb255 0 190 0)
        eMaterial = Material.matte (Color.rgb255 255 255 0)
        rMaterial = Material.matte (Color.rgb255 255 0 0)
        sMaterial = Material.matte (Color.rgb255 0 0 255)
        plyEntity = Scene3d.mesh entityMaterial model.mesh1
        cEntity = Scene3d.mesh cMaterial model.meshC
        lEntity = Scene3d.mesh lMaterial model.meshL
        eEntity = Scene3d.mesh eMaterial model.meshE
        rEntity = Scene3d.mesh rMaterial model.meshR
        sEntity = Scene3d.mesh sMaterial model.meshS
        softLighting =
          Light.overhead
            { upDirection = Direction3d.positiveZ
            , chromaticity = Light.colorTemperature (if model.showClers then (Temperature.kelvins 3000) else (Temperature.kelvins 1500))
            , intensity = Illuminance.lux (if model.showClers then 150 else 70)
            }
        camera =
          Camera3d.perspective
            { viewpoint = viewpoint
            , verticalFieldOfView = Angle.degrees model.fov
            }
        colorBox color = div [style "width" "15px", style "height" "15px", style "background-color" color, style "border-radius" "3px"] []
      in
      { title = "Viewing " ++ model.filename
      , body =
        [ div 
          [ style "position" "absolute"
          , style "top" "5px"
          , style "left" "5px"
          , style "z-index" "1"
          ]
          [ div [] [h2 [style "margin" "0px"] [text "Field of View: "], SingleSlider.view model.slider ]
          , hr [] []
          , case model.savedPly of
              Nothing -> div [] []
              Just ply -> 
                div [] 
                  [ "PLY Info:" |> text |> singleton |> h2 [style "margin" "0px"]
                  , model.filename |> (++) "Filename: " |> text |> singleton |> p [style "margin" "0px"]
                  , String.fromInt ply.nface |> (++) "Faces: " |> text |> singleton |> p [style "margin" "0px"] 
                  , String.fromInt ply.nvert |> (++) "Vertices: " |> text |> singleton |> p [style "margin" "0px"] 
                  ]
          , case model.ebRes of
              Nothing -> div [] []
              Just res -> 
                div []
                  [ hr [] [] 
                  , button [onClick ToggleDisplay, style "font-weight" "bold", style "font-size" "large"] [text (if model.showClers then "Toggle Off CLERS Display" else "Toggle On CLERS Display")]
                  ,  hr [] []
                  , "Edgebreaker Output:" |> text |> singleton |> h2 [style "margin" "0px"]
                  , "CLERS Frequency:" |> text |> singleton |> h3 [style "margin-bottom" "0px"]
                  , "C: " ++ (List.Extra.count (\c -> c == 'C') res.clers |> String.fromInt)  |> text |> singleton |> (::) (colorBox "rgb(0,255,255)") |> p [style "margin" "0px", style "display" "flex"]
                  , "R: " ++ (List.Extra.count (\c -> c == 'R') res.clers |> String.fromInt)  |> text |> singleton |> (::) (colorBox "rgb(255,0,0)")   |> p [style "margin" "0px", style "display" "flex"]
                  , "L: " ++ (List.Extra.count (\c -> c == 'L') res.clers |> String.fromInt)  |> text |> singleton |> (::) (colorBox "rgb(0,190,0)") |> p [style "margin" "0px", style "display" "flex"]
                  , "S: " ++ (List.Extra.count (\c -> c == 'S') res.clers |> String.fromInt)  |> text |> singleton |> (::) (colorBox "rgb(0,0,255)")   |> p [style "margin" "0px", style "display" "flex"]
                  , "E: " ++ (List.Extra.count (\c -> c == 'E') res.clers |> String.fromInt)  |> text |> singleton |> (::) (colorBox "rgb(255,255,0)")   |> p [style "margin" "0px", style "display" "flex"]
                  , "Total: " ++ (List.length res.clers |> String.fromInt)  |> text |> singleton |> p [style "margin" "0px"]
                  , "Complete CLERS Output:" |> text |> singleton |> h3 []
                  , textarea [rows 6, cols 30, value (String.fromList res.clers)] []
                  , "Complete Delta Output:" |> text |> singleton |> h3 []
                  , textarea [rows 6, cols 30, value (v3sToString res.delta)] []
                  ]
          ]
        , reset
        , title
        , links "white" True
        , Scene3d.custom
          { camera = camera
          , clipDepth = Length.meters 0.1
          , dimensions = ( model.width, model.height )
          , background = Scene3d.backgroundColor Color.lightBlue
          , entities = if model.showClers then [cEntity, lEntity, eEntity, rEntity, sEntity] else [plyEntity]
          , lights = Scene3d.oneLight softLighting
          , exposure = Scene3d.exposureValue 5
          , whiteBalance = Light.incandescent
          , antialiasing = Scene3d.supersampling 3
          , toneMapping = Scene3d.noToneMapping
          }
        ]
      }

hrefAndStyle : String -> String -> Bool -> List (Attribute msg)
hrefAndStyle link color shadow =
  let 
    res = 
      [ href link
      , style "color" color
      , style "text-decoration" "none"
      ]
    s = style "text-shadow" "2px 2px 2px black"
  in
  if shadow then s::res
  else res

v3sToString : List Vec3 -> String
v3sToString vecs =
  let
    vSAcc v3s acc =
      case v3s of
        [] -> acc |> String.dropRight 1
        (v::vs) -> vSAcc vs (acc ++ v3ToString v ++ "\n")
  in vSAcc vecs ""

v3ToString : Vec3 -> String
v3ToString v = 
  (getX v |> String.fromFloat) ++ 
  "\n" ++ 
  (getY v |> String.fromFloat) ++ 
  "\n" ++ 
  (getZ v |> String.fromFloat)

main : Program () Model Msg
main =
  Browser.document
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }
