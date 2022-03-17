module Edgebreaker exposing (compressPly, EBRes, ClersParse)

import Ply
import Math.Vector3 exposing (Vec3, sub, add)
import List.Extra exposing (getAt)
import List exposing (member)

type alias EBState = 
  { ply : Ply.PlyModel
  , next : Int
  , vVert : List Int -- M or Visited Verticies
  , vTri : List Int  -- U or Visited Triangles
  , delta : List Vec3
  , clers : List Char
  }

type alias EBRes =
  { delta : List Vec3
  , clers : List Char
  , verts : List Int
  , faces : List Int
  , origPly : Ply.PlyModel
  , clersFaces : ClersParse
  }

type alias ClersParse =
  { c : List Int
  , l : List Int
  , e : List Int
  , r : List Int
  , s : List Int
  }

compressPly : Ply.PlyModel -> Result String EBRes
compressPly ply =
  let 
    makeState = compressInit ply
    res =
      case makeState of
        Err str -> Err str
        Ok state -> compressWithState state

    getClersFaces : ClersParse -> List Char -> List Int -> ClersParse
    getClersFaces input clers faces =
      case clers of
        [] -> input
        (next::rem) -> case faces of
          [] -> input
          (f::fs) -> case next of
            'C' -> getClersFaces {input | c = f::input.c} rem fs
            'L' -> getClersFaces {input | l = f::input.l} rem fs
            'E' -> getClersFaces {input | e = f::input.e} rem fs
            'R' -> getClersFaces {input | r = f::input.r} rem fs
            _ ->   getClersFaces {input | s = f::input.s} rem fs
    clersFaces x = getClersFaces {c=[], l=[], e=[], r=[], s=[]} (x.clers |> List.reverse) (x.vTri |> List.reverse)
  in
    case res of
      Err str -> Err str
      Ok x ->
        let clersFaceLists = clersFaces x
        in
        Ok 
        { delta = x.delta |> List.reverse
        , clers = x.clers |> List.reverse
        , verts = x.vVert |> List.reverse
        , faces = x.vTri |> List.reverse
        , origPly = ply
        , clersFaces = clersFaceLists
        }

compressWithState : EBState -> Result String EBState
compressWithState state =
  case getAt state.next state.ply.corners of
    Nothing -> "Failed to get corner " ++ String.fromInt state.next |> Err
    Just c ->
      let newVTri = c.t :: state.vTri
          rTri = --c.n.o.t
            case getAt c.n state.ply.corners of
              Nothing -> "Failed to get corner " ++ String.fromInt c.n |> Err
              Just n -> case getAt n.o state.ply.corners of
                Nothing -> "Failed to get corner " ++ String.fromInt n.o |> Err
                Just o -> Ok o
          
          lTri = --c.p.o.t
            case getAt c.p state.ply.corners of
              Nothing -> "Failed to get corner " ++ String.fromInt c.p |> Err
              Just p -> case getAt p.o state.ply.corners of
                Nothing -> "Failed to get corner " ++ String.fromInt p.o |> Err
                Just o -> Ok o
      in
      if member c.v state.vVert |> not then --if it's not visited already, we do the procedure for the C op
        let 
          newClers = 'C' :: state.clers
          newVVert = c.v :: state.vVert
          cvPos = case getAt c.v state.ply.verts of
            Nothing -> "Failed to get vertex " ++ String.fromInt c.v |> Err
            Just cv -> Ok cv.position
          cpvPos = case getAt c.p state.ply.corners of
            Nothing -> "Failed to get corner " ++ String.fromInt c.p |> Err
            Just cp -> case getAt cp.v state.ply.verts of
              Nothing -> "Failed to get vertex " ++ String.fromInt cp.v |> Err
              Just cpv -> Ok cpv.position
          
          cnvPos = case getAt c.n state.ply.corners of
            Nothing -> "Failed to get corner " ++ String.fromInt c.n |> Err
            Just cn -> case getAt cn.v state.ply.verts of
              Nothing -> "Failed to get vertex " ++ String.fromInt cn.v |> Err
              Just cnv -> Ok cnv.position

          covPos = case getAt c.o state.ply.corners of
            Nothing -> "Failed to get corner " ++ String.fromInt c.o |> Err
            Just co -> case getAt co.v state.ply.verts of
              Nothing -> "Failed to get vertex " ++ String.fromInt co.v |> Err
              Just cov -> Ok cov.position

          deltToAdd = -- c.v.pos - c.p.v.pos - c.n.v.pos + c.o.v.pos
            case cvPos of
              Err _ -> cvPos
              Ok cvP -> case cpvPos of
                Err _ -> cpvPos
                Ok cpvP -> case cnvPos of
                  Err _ -> cnvPos
                  Ok cnvP -> case covPos of
                    Err _ -> covPos
                    Ok covP -> add (sub (sub cvP cpvP) cnvP) covP |> Ok
        in
          case rTri of
            Err e -> Err e
            Ok rt -> case deltToAdd of
              Err e -> Err e
              Ok delt -> 
                let 
                  newState = 
                    { state | next = rt.i
                    , vVert = newVVert
                    , vTri = newVTri
                    , delta = delt :: state.delta
                    , clers = newClers
                    }
                    --|> Debug.log "C op"
                in compressWithState newState
      else
        case rTri of
          Err e -> Err e
          Ok rT -> case lTri of
            Err e -> Err e
            Ok lT ->
              if (member rT.t state.vTri) then
                if (member lT.t state.vTri) then
                  let --Op E, and POP
                    newState =
                      { state
                      | clers = 'E' :: state.clers
                      , vTri = newVTri
                      }
                      --|> Debug.log "E op"
                  in Ok newState
                else
                  let --Op R, move to left Triangle
                    newState =
                      { state
                      | clers = 'R':: state.clers
                      , vTri = newVTri
                      , next = lT.i
                      }
                      --|> Debug.log "R op"
                  in compressWithState newState
              else if (member lT.t state.vTri) then
                let -- Op L, move to right Triangle
                  newState =
                    { state 
                    | clers = 'L' :: state.clers
                    , vTri = newVTri
                    , next = rT.i
                    }
                    --|> Debug.log "L op"
                in compressWithState newState
                else
                  let -- Op S, Recursive call to visiting right branch, Then move to left
                    newState =
                      { state
                      | clers = 'S' :: state.clers
                      , vTri = newVTri
                      , next = rT.i
                      }
                      --|> Debug.log "S op"
                    splitCompress = compressWithState newState
                  in case splitCompress of
                    Err _ -> splitCompress
                    Ok resState -> compressWithState ({resState | next = lT.i}) --|> Debug.log "S resume op")

compressInit : Ply.PlyModel -> Result String EBState
compressInit ply =
  let initTri = 0 in --we'll start at face 0
  case getAt initTri ply.corners of
    Nothing -> Err "Cannot get first corner"
    Just c -> case getAt c.p ply.corners of
      Nothing -> Err "Cannot get second corner"
      Just p -> case getAt c.n ply.corners of
        Nothing -> Err "Cannot get second corner"
        Just n -> case getAt c.v ply.verts of
          Nothing -> Err "Cannot get vert of first corner"
          Just cVert -> case getAt p.v ply.verts of
            Nothing -> Err "Cannot get vert of second corner"
            Just pVert -> case getAt n.v ply.verts of
              Nothing -> Err "Cannot get vert of third corner"
              Just nVert -> 
                let
                  deltInit = 
                    [ sub nVert.position cVert.position
                    , sub cVert.position pVert.position
                    , pVert.position  
                    ]
                  m = [c.v, n.v, p.v]
                  u = [initTri]
                in
                Ok
                  ({ ply = ply
                  , next = c.o
                  , vVert = m
                  , vTri = u
                  , delta = deltInit
                  , clers = ['C']
                  }) -- |> Debug.log "Start C op")