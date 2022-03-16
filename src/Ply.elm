module Ply exposing (..)

import Math.Vector3 exposing (Vec3)
import Parser exposing (..)
import String exposing (lines)
import List exposing (drop)
import List exposing (take)
import Math.Vector3 exposing (vec3, distance)
import List.Extra
import Tuple3
import List exposing (foldl)
import BoundingBox2d exposing (minY)
import Math.Vector3 exposing (getX, getY, getZ)
import Basics.Extra exposing (maxSafeInteger, minSafeInteger)

type alias PlyModel = 
  { verts : List Vert
  , edges : List Edge
  , faces : List Face
  , corners : List Corner
  , nvert : Int
  , nface : Int
  , minCoord : Vec3
  , center : Vec3
  , maxCoord : Vec3
  }

emptyPly =
  { verts = []
  , edges = []
  , faces = []
  , corners = []
  , nvert = 0
  , nface = 0
  , minCoord = vec3 0 0 0
  , center = vec3 0 0 0
  , maxCoord = vec3 0 0 0
  }

type alias Vert =
  { position : Vec3
  , normal : Vec3
  , index : Int
  }

type alias Edge =
  { verts : (Int, Int)
  , faces : (Int, Int)
  , index : Int
  }

type alias Face =
  { verts : (Int,Int,Int)
  , normal : Vec3
  , index : Int
  }

type alias Corner =
  { v : Int
  , t : Int
  , p : Int
  , pv : Int
  , n : Int
  , nv : Int
  , o : Int
  , i : Int
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
    cornerList = makeCorners faceList --|> Debug.log "corners"
    vertSpan = plyMinCenterMax vertList

  in 
    if (numVert == 0 || numFace == 0 || headerEnd == 0)
    then Nothing
    else
      Just 
        { verts = vertList
        , edges = []
        , faces = faceList
        , corners = cornerList
        , nvert = numVert
        , nface = numFace
        , minCoord = vertSpan.min
        , center = vertSpan.center
        , maxCoord = vertSpan.max
        }

plyMinCenterMax : List Vert -> {min:Vec3, center:Vec3, max:Vec3}
plyMinCenterMax verts = 
  let minX = List.foldl (\v acc -> getX v.position |> min acc ) maxSafeInteger verts
      minY = List.foldl (\v acc -> getY v.position |> min acc ) maxSafeInteger verts
      minZ = List.foldl (\v acc -> getZ v.position |> min acc ) maxSafeInteger verts
      maxX = List.foldl (\v acc -> getX v.position |> max acc ) minSafeInteger verts
      maxY = List.foldl (\v acc -> getY v.position |> max acc ) minSafeInteger verts
      maxZ = List.foldl (\v acc -> getZ v.position |> max acc ) minSafeInteger verts
      midX = ((maxX - minX) / 2.0) + minX
      midY = ((maxY - minY) / 2.0) + minY
      midZ = ((maxZ - minZ) / 2.0) + minZ
  in
    { min = vec3 minX minY minZ
    , center = vec3 midX midY midZ
    , max = vec3 maxX maxY maxZ
    }

sortCorners : List Corner -> List Corner
sortCorners corners = List.sortWith cornerComp corners

cornerComp : { a | nv : comparable, pv : comparable } -> { b | nv : comparable, pv: comparable } -> Order
cornerComp c1 c2 =
  let minC1 = min c1.nv c1.pv
      minC2 = min c2.nv c2.pv
      maxC1 = max c1.nv c1.pv
      maxC2 = max c2.nv c2.pv
  in
  if (minC1 < minC2) then LT
  else if minC1 == minC2
    then if maxC1 < maxC2 then LT
    else GT
  else GT



makeCorners : List Face -> List Corner
makeCorners faces = 
  List.indexedMap makeCorner faces 
    |> List.concat
    |> sortCorners
    |> addCornerOps []
    |> List.sortBy .i

addCornerOps : List Corner -> List Corner -> List Corner
addCornerOps res corners = case corners of
    [] -> res
    (c::[]) -> res
    (c1::c2::cs) -> 
      let minC1 = min c1.nv c1.pv
          minC2 = min c2.nv c2.pv
          maxC1 = max c1.nv c1.pv
          maxC2 = max c2.nv c2.pv
      in if minC1 == minC2 && maxC1 == maxC2 then
        addCornerOps ({c1 | o = c2.i} :: {c2 | o = c1.i} :: res) cs 
        else addCornerOps (c2::cs) (c1::res)


makeCorner : Int -> Face -> List Corner
makeCorner index face =
  let
    c1 = Corner (Tuple3.first face.verts) index (index * 3 + 2) (Tuple3.third face.verts) (index * 3 + 1) (Tuple3.second face.verts) (-1) (index * 3)
    c2 = Corner (Tuple3.second face.verts) index (index * 3) (Tuple3.first face.verts) (index * 3 + 2) (Tuple3.third face.verts) (-1) (index * 3 + 1)
    c3 = Corner (Tuple3.third face.verts) index (index * 3 + 1) (Tuple3.second face.verts) (index * 3) (Tuple3.first face.verts) (-1) (index * 3 + 2)
  in [c1,c2,c3]


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
    helper : Int -> String -> Vert
    helper index str = case run parseVert str of
      Err _ -> Vert (vec3 0 0 0) (vec3 0 0 0) index
      Ok res -> 
        let 
          x = List.Extra.getAt 0 res |> Maybe.withDefault 0 
          y =  List.Extra.getAt 1 res |> Maybe.withDefault 0 
          z = List.Extra.getAt 2 res |> Maybe.withDefault 0
          nx = List.Extra.getAt 3 res |> Maybe.withDefault 0 
          ny =  List.Extra.getAt 4 res |> Maybe.withDefault 0 
          nz = List.Extra.getAt 5 res |> Maybe.withDefault 0
        in Vert (vec3 x y z) (vec3 nx ny nz) index
  in List.indexedMap helper input |> List.filter (\v -> distance v.position (vec3 0 0 0) > 0)

parseFaces : List String -> List Face
parseFaces input = 
  let helper : Int -> String -> Face
      helper index str = case run parseFace str of
        Err _ -> Face (0,0,0) (vec3 0 0 0) index
        Ok res -> 
          let 
            v1 = List.Extra.getAt 1 res |> Maybe.withDefault 0 
            v2 =  List.Extra.getAt 2 res |> Maybe.withDefault 0 
            v3 = List.Extra.getAt 3 res |> Maybe.withDefault 0
          in Face (v1,v2,v3) (vec3 0 0 0) index
  in List.indexedMap helper input |> List.filter (\f -> sumTriple f.verts > 0)

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