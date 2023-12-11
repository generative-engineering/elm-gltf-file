module Gltf.Decode.Mesh exposing (Mesh(..), assembleDefaultScene, meshesFromDefaultScene)

import Array
import Bytes exposing (Bytes)
import Bytes.Decode
import Color exposing (Color)
import Geometry.Interop.LinearAlgebra.Point3d as Point3d
import Geometry.Interop.LinearAlgebra.Vector3d as Vector3d
import Gltf.Decode.Json.Raw as Raw
import Json.Decode
import Length exposing (Meters)
import Math.Matrix4 exposing (Mat4)
import Math.Vector3
import Point3d exposing (Point3d)
import Quantity exposing (Unitless)
import Result.Extra
import TriangularMesh exposing (TriangularMesh)
import Vector3d exposing (Vector3d)


type alias Vertex coordinates =
    { position : Point3d Meters coordinates
    , normal : Vector3d Unitless coordinates
    , uv : ( Float, Float )
    }


getNode : Raw.Gltf -> Int -> Result String Raw.Node
getNode gltf nodeIndex =
    case Array.get nodeIndex gltf.nodes of
        Just node ->
            Ok node

        Nothing ->
            Err <| "There is no node at index " ++ String.fromInt nodeIndex ++ ", as the node array is only " ++ (String.fromInt <| Array.length gltf.nodes) ++ " items long"


getMeshes : Raw.Gltf -> Raw.Node -> Result String (List ( Raw.Mesh, Mat4 ))
getMeshes gltf node =
    let
        nodeMesh =
            case node.mesh of
                Just meshIndex ->
                    getMesh gltf meshIndex
                        |> Result.map (\mesh -> [ ( mesh, node.matrix ) ])

                Nothing ->
                    Ok []

        childMeshes =
            node.children
                |> List.map (getNode gltf)
                |> List.map (Result.andThen <| getMeshes gltf)
                |> List.map (Result.map (List.map <| \( childMesh, childMatrix ) -> ( childMesh, Math.Matrix4.mul node.matrix childMatrix )))
                |> Result.Extra.combine
                |> Result.map List.concat
    in
    Result.map2 (++) nodeMesh childMeshes


getMesh : Raw.Gltf -> Int -> Result String Raw.Mesh
getMesh gltf meshIndex =
    case Array.get meshIndex gltf.meshes of
        Just mesh ->
            Ok mesh

        Nothing ->
            Err <| "There is no mesh at index " ++ String.fromInt meshIndex ++ ", as the mesh array is only " ++ (String.fromInt <| Array.length gltf.meshes) ++ " items long"


mapAccessor : (Raw.Accessor -> Result String (Bytes.Decode.Decoder a)) -> Raw.Gltf -> Bytes -> Int -> Result String a
mapAccessor decoderConstructor gltf bytes accessorIndex =
    case Array.get accessorIndex gltf.accessors of
        Just accessor ->
            Result.andThen
                (\decoder ->
                    case accessor.bufferView of
                        Just bufferViewIndex ->
                            case Array.get bufferViewIndex gltf.bufferViews of
                                Just bufferView ->
                                    let
                                        raw =
                                            Bytes.Decode.decode
                                                (skipBytes
                                                    (accessor.byteOffset + bufferView.byteOffset)
                                                    decoder
                                                )
                                                bytes
                                    in
                                    case raw of
                                        Just result ->
                                            Ok result

                                        Nothing ->
                                            Err <| "Could not fetch accessor " ++ String.fromInt accessorIndex ++ " VEC3 data"

                                Nothing ->
                                    Err <| "There is no buffer view at index " ++ String.fromInt bufferViewIndex ++ ", as the buffer view array is only " ++ (String.fromInt <| Array.length gltf.bufferViews) ++ " items long"

                        Nothing ->
                            Err <| "The accessor at index " ++ String.fromInt accessorIndex ++ " has no buffer view index"
                )
            <|
                decoderConstructor accessor

        Nothing ->
            Err <| "There is no accessor at index " ++ String.fromInt accessorIndex ++ ", as the accessor array is only " ++ (String.fromInt <| Array.length gltf.accessors) ++ " items long"


loop : Int -> Bytes.Decode.Decoder a -> Int -> Bytes.Decode.Decoder (List ( a, a, a ))
loop increment decoder count =
    Bytes.Decode.loop ( 0, [] )
        (\( done, vertices ) ->
            if done < count then
                Bytes.Decode.map3 (\x y z -> ( x, y, z ))
                    decoder
                    decoder
                    decoder
                    |> Bytes.Decode.map (\vector -> Bytes.Decode.Loop ( done + increment, vector :: vertices ))

            else
                Bytes.Decode.Done (List.reverse vertices)
                    |> Bytes.Decode.succeed
        )


getFloatVec3 : Raw.Gltf -> Bytes -> Int -> Result String (List ( Float, Float, Float ))
getFloatVec3 =
    mapAccessor
        (\accessor ->
            let
                decoderResult =
                    case accessor.componentType of
                        Raw.Float ->
                            Ok <| Bytes.Decode.float32 Bytes.LE

                        _ ->
                            Err <| "Accessor" ++ (accessor.name |> Maybe.map (\name -> " " ++ name) |> Maybe.withDefault "") ++ " was expected to point to a float"
            in
            decoderResult
                |> Result.map
                    (\decoder ->
                        loop 1 decoder accessor.count
                    )
        )


getIntVec3 : Raw.Gltf -> Bytes -> Int -> Result String (List ( Int, Int, Int ))
getIntVec3 =
    mapAccessor
        (\accessor ->
            let
                decoderResult =
                    case accessor.componentType of
                        Raw.UnsignedShort ->
                            Ok <| Bytes.Decode.unsignedInt16 Bytes.LE

                        Raw.UnsignedInt ->
                            Ok <| Bytes.Decode.unsignedInt32 Bytes.LE

                        _ ->
                            Err <| "Accessor" ++ (accessor.name |> Maybe.map (\name -> " " ++ name) |> Maybe.withDefault "") ++ " was expected to point to an integer"
            in
            decoderResult
                |> Result.map
                    (\decoder ->
                        loop 3 decoder accessor.count
                    )
        )


getPositions : Raw.Gltf -> Bytes -> Raw.MeshPrimitive -> Result String (List (Point3d Meters coordinates))
getPositions gltf bytes primitive =
    case primitive.attributes.position of
        Just positionIndex ->
            getFloatVec3 gltf bytes positionIndex
                |> Result.map (List.map (\( x, y, z ) -> Point3d.meters x y z))

        Nothing ->
            Err <| "No positions were found on this primitive"


getNormals : Raw.Gltf -> Bytes -> Raw.MeshPrimitive -> Result String (Maybe (List (Vector3d Unitless coordinates)))
getNormals gltf bytes primitive =
    case primitive.attributes.normal of
        Just normalIndex ->
            getFloatVec3 gltf bytes normalIndex
                |> Result.map (Just << List.map (\( x, y, z ) -> Vector3d.unitless x y z))

        Nothing ->
            Ok Nothing


getIndices : Raw.Gltf -> Bytes -> Raw.MeshPrimitive -> Result String (List ( Int, Int, Int ))
getIndices gltf bytes primitive =
    case primitive.indices of
        Just indicesIndex ->
            getIntVec3 gltf bytes indicesIndex

        Nothing ->
            Err <| "No indices were found on this primitive"


getColor : Raw.Gltf -> Raw.MeshPrimitive -> Result String (Maybe Color)
getColor gltf primitive =
    case primitive.material of
        Just materialIndex ->
            case Array.get materialIndex gltf.materials of
                Just material ->
                    case material.pbrMetallicRoughness of
                        Just pbr ->
                            Ok <| Just (Color.fromRgba pbr.baseColorFactor)

                        Nothing ->
                            Err <| "no PBR metallic roughness was found on material" ++ (material.name |> Maybe.map (\name -> " " ++ name) |> Maybe.withDefault "") ++ " at index " ++ String.fromInt materialIndex

                Nothing ->
                    Err <| "No material was found at index " ++ String.fromInt materialIndex ++ " (There are only " ++ String.fromInt (Array.length gltf.materials) ++ " materials)"

        Nothing ->
            Ok Nothing


type alias Primitives coordinates =
    { positions : List (Point3d Meters coordinates)
    , normals : Maybe (List (Vector3d Unitless coordinates))
    , indices : List ( Int, Int, Int )
    , color : Maybe Color
    }


getPrimitiveAttributes : Raw.Gltf -> Bytes -> Raw.Mesh -> Int -> Raw.MeshPrimitive -> Result String (Primitives coordinates)
getPrimitiveAttributes gltf bytes mesh primitiveIndex primitive =
    Result.map4 Primitives
        (getPositions gltf bytes primitive
            |> Result.mapError (\_ -> "The mesh" ++ (mesh.name |> Maybe.map (\name -> " " ++ name) |> Maybe.withDefault "") ++ "'s primitive (" ++ String.fromInt primitiveIndex ++ ") has no positions")
        )
        (getNormals gltf bytes primitive
            |> Result.mapError (\_ -> "The mesh" ++ (mesh.name |> Maybe.map (\name -> " " ++ name) |> Maybe.withDefault "") ++ "'s primitive (" ++ String.fromInt primitiveIndex ++ ") has no normals")
        )
        (getIndices gltf bytes primitive
            |> Result.mapError (\_ -> "The mesh" ++ (mesh.name |> Maybe.map (\name -> " " ++ name) |> Maybe.withDefault "") ++ "'s primitive (" ++ String.fromInt primitiveIndex ++ ") has no indices")
        )
        (getColor gltf primitive
            |> Result.mapError (\_ -> "The mesh" ++ (mesh.name |> Maybe.map (\name -> " " ++ name) |> Maybe.withDefault "") ++ "'s primitive (" ++ String.fromInt primitiveIndex ++ ") has no color specified in its material")
        )


type Mesh coordinates
    = Triangles (TriangularMesh (Point3d Meters coordinates))
    | Faces (TriangularMesh { position : Point3d Meters coordinates, normal : Vector3d Unitless coordinates })


toMesh : Mat4 -> Primitives coordinates -> ( Mesh coordinates, Maybe Color )
toMesh transformation primitives =
    let
        triangularMesh vertexList =
            TriangularMesh.indexed (Array.fromList vertexList) primitives.indices
    in
    ( case primitives.normals of
        Just normals ->
            Faces <|
                triangularMesh <|
                    List.map2
                        (\position normal ->
                            { position = Point3d.transformBy transformation position
                            , normal = Vector3d.transformBy transformation normal
                            }
                        )
                        primitives.positions
                        normals

        Nothing ->
            Triangles <|
                triangularMesh <|
                    List.map (Point3d.transformBy transformation) primitives.positions
    , primitives.color
    )


toMeshes : Raw.Gltf -> Bytes -> ( Raw.Mesh, Mat4 ) -> Result String (List ( Mesh coordinates, Maybe Color ))
toMeshes gltf buffers ( mesh, transformation ) =
    List.indexedMap (getPrimitiveAttributes gltf buffers mesh) mesh.primitives
        |> List.map (Result.map (toMesh transformation))
        |> Result.Extra.combine


skipBytes : Int -> Bytes.Decode.Decoder a -> Bytes.Decode.Decoder a
skipBytes skip decoder =
    Bytes.Decode.bytes skip |> Bytes.Decode.andThen (\_ -> decoder)


meshesFromDefaultScene : Bytes -> Result String (List ( Mesh coordinates, Maybe Color ))
meshesFromDefaultScene bytes =
    let
        decoder =
            Bytes.Decode.string 4
                |> Bytes.Decode.andThen
                    (\magic ->
                        case magic of
                            "glTF" ->
                                Bytes.Decode.succeed "glTF"

                            _ ->
                                Bytes.Decode.fail
                    )
                |> Bytes.Decode.andThen (\_ -> Bytes.Decode.unsignedInt32 Bytes.LE)
                |> Bytes.Decode.andThen
                    (\version ->
                        case version of
                            2 ->
                                Bytes.Decode.succeed 2

                            _ ->
                                Bytes.Decode.fail
                    )
                |> Bytes.Decode.andThen (\_ -> Bytes.Decode.unsignedInt32 Bytes.LE)
                |> Bytes.Decode.andThen (\_ -> Bytes.Decode.unsignedInt32 Bytes.LE)
                |> Bytes.Decode.andThen
                    (\length ->
                        Bytes.Decode.unsignedInt32 Bytes.LE
                            |> Bytes.Decode.andThen (\_ -> Bytes.Decode.succeed length)
                    )
                |> Bytes.Decode.andThen (\length -> Bytes.Decode.string length)
                |> Bytes.Decode.andThen
                    (\json ->
                        case Json.Decode.decodeString Raw.decoder json of
                            Ok gltf ->
                                Bytes.Decode.unsignedInt32 Bytes.LE
                                    |> Bytes.Decode.andThen
                                        (\length ->
                                            Bytes.Decode.unsignedInt32 Bytes.LE
                                                |> Bytes.Decode.andThen (\_ -> Bytes.Decode.succeed length)
                                        )
                                    |> Bytes.Decode.andThen
                                        (\length ->
                                            Bytes.Decode.bytes length
                                        )
                                    |> Bytes.Decode.andThen (\b -> Bytes.Decode.succeed ( gltf, b ))

                            Err _ ->
                                Bytes.Decode.fail
                    )
                |> Bytes.Decode.decode
    in
    case decoder bytes of
        Just ( gltf, valuesBytes ) ->
            assembleDefaultScene gltf valuesBytes

        Nothing ->
            Err "The file is malformed"


assembleDefaultScene : Raw.Gltf -> Bytes -> Result String (List ( Mesh coordinates, Maybe Color ))
assembleDefaultScene gltf valuesBytes =
    case gltf.scene of
        Just sceneIndex ->
            case Array.get sceneIndex gltf.scenes of
                Just scene ->
                    scene.nodes
                        |> Array.toList
                        |> List.map (getNode gltf)
                        |> List.map (Result.andThen (getMeshes gltf))
                        |> List.map (Result.andThen (\meshes -> meshes |> List.map (toMeshes gltf valuesBytes) |> Result.Extra.combine))
                        |> List.map (Result.map List.concat)
                        |> Result.Extra.combine
                        |> Result.map List.concat

                Nothing ->
                    Err <| "The default scene index (" ++ String.fromInt sceneIndex ++ ") is out of bound of the array of scenes (" ++ (String.fromInt <| Array.length gltf.scenes) ++ " items)"

        Nothing ->
            Err "There is no default scene in the file"
