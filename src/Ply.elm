module Ply exposing (..)

import Math.Vector3 exposing (Vec3)
import Parser exposing (..)
import String exposing (lines)
import List exposing (drop)
import List exposing (take)
import Math.Vector3 exposing (vec3, distance)
import List.Extra

type alias PlyModel = 
  { verts : List Vert
  , edges : List Edge
  , faces : List Face
  , corners : List Corner
  , nvert : Int
  , nface : Int
  }

type alias Vert =
  { position : Vec3
  , normal : Vec3
  }

type alias Edge =
  { verts : (Int, Int)
  , faces : (Int, Int)
  , index : Int
  }

type alias Face =
  { verts : (Int,Int,Int)
  , normal : Vec3
  }

type alias Corner =
  { v : Int
  , p : Int
  , n : Int
  , t : Int
  , o : Int
  }

parsePly : String -> Maybe PlyModel
parsePly input = 
  let 
    plyLines = lines input
    numVert = getNumVert plyLines |> Maybe.withDefault 0
    numFace = getNumFace plyLines |> Maybe.withDefault 0
    headerEnd = getEndHeaderInd plyLines |> Maybe.withDefault 0 
    noHeadLines = drop headerEnd plyLines
    vertLines = take numVert noHeadLines
    faceLines = drop numVert noHeadLines
    vertList = parseVerts vertLines
    faceList = parseFaces faceLines

  in 
    if (numVert == 0 || numFace == 0 || headerEnd == 0)
    then Nothing
    else
      Just 
        { verts = vertList
        , edges = []
        , faces = faceList
        , corners = []
        , nvert = numVert
        , nface = numFace
        }

getNumVert : List String -> Maybe Int
getNumVert input =
  case input of
    [] -> Nothing
    (x::xs) -> 
      case run (getElemCount "vertex") x of
        Ok res -> Just res
        Err _ -> case run isHeaderEnd x of
          Ok _ -> Nothing
          Err _ -> getNumVert xs

getNumFace : List String -> Maybe Int
getNumFace input =
  case input of
    [] -> Nothing
    (x::xs) -> 
      case run (getElemCount "face") x of
        Ok res -> Just res
        Err _ -> case run isHeaderEnd x of
          Ok _ -> Nothing
          Err _ -> getNumFace xs

getEndHeaderInd : List String -> Maybe Int
getEndHeaderInd input = 
  let 
    helper : List String -> Int -> Maybe Int
    helper strs count = case strs of
      [] -> Nothing
      (x::xs) -> case run isHeaderEnd x of
        Ok _ -> Just count
        Err _ -> helper xs (count + 1)
  in helper input 1
  
getElemCount : String -> Parser Int
getElemCount name =
  succeed identity
    |. token "element"
    |. spaces
    |. token name
    |. spaces
    |= int

isHeaderEnd : Parser ()
isHeaderEnd = Parser.token "end_header"

parseVerts : List String -> List Vert
parseVerts input = 
  let 
    helper : String -> Vert
    helper str = case run parseVert str of
      Err _ -> Vert (vec3 0 0 0) (vec3 0 0 0)
      Ok res -> 
        let 
          x = List.Extra.getAt 0 res |> Maybe.withDefault 0 
          y =  List.Extra.getAt 1 res |> Maybe.withDefault 0 
          z = List.Extra.getAt 2 res |> Maybe.withDefault 0
          nx = List.Extra.getAt 3 res |> Maybe.withDefault 0 
          ny =  List.Extra.getAt 4 res |> Maybe.withDefault 0 
          nz = List.Extra.getAt 5 res |> Maybe.withDefault 0
        in Vert (vec3 x y z) (vec3 nx ny nz)
  in List.map helper input |> List.filter (\v -> distance v.position (vec3 0 0 0) > 0)

parseFaces : List String -> List Face
parseFaces input = 
  let helper : String -> Face
      helper str = case run parseFace str of
        Err _ -> Face (0,0,0) (vec3 0 0 0)
        Ok res -> 
          let 
            v1 = List.Extra.getAt 1 res |> Maybe.withDefault 0 
            v2 =  List.Extra.getAt 2 res |> Maybe.withDefault 0 
            v3 = List.Extra.getAt 3 res |> Maybe.withDefault 0
          in Face (v1,v2,v3) (vec3 0 0 0)
  in List.map helper input |> List.filter (\f -> sumTriple f.verts > 0)

sumTriple : (Int, Int, Int) -> Int
sumTriple (a,b,c) = a+b+c

parseVert : Parser (List Float)
parseVert =
  sequence
    { start = ""
    , separator = ""
    , end = ""
    , spaces = spaces
    , item = parseFloat
    , trailing = Forbidden
    }

negFloat : Parser Float
negFloat =
  succeed identity
    |. symbol "-"
    |= map (\x -> x * (-1)) float

parseFloat : Parser Float
parseFloat = oneOf [float, negFloat]


parseFace : Parser (List Int)
parseFace =
  sequence
    { start = ""
    , separator = ""
    , end = ""
    , spaces = spaces
    , item = int
    , trailing = Forbidden
    }