module Gltf.Decode.Mesh exposing (BaseColor(..), Material, Mesh(..), assembleDefaultScene, meshesFromDefaultScene)

import Array exposing (Array)
import Base64
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
import WebGL.Texture


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


mapAccessor : (Raw.Accessor -> Result String (Bytes.Decode.Decoder a)) -> Raw.Gltf -> Array Bytes -> Int -> Result String a
mapAccessor decoderConstructor gltf buffers accessorIndex =
    case Array.get accessorIndex gltf.accessors of
        Just accessor ->
            Result.andThen
                (\decoder ->
                    case accessor.bufferView of
                        Just bufferViewIndex ->
                            case Array.get bufferViewIndex gltf.bufferViews of
                                Just bufferView ->
                                    case Array.get bufferView.buffer buffers of
                                        Just bytes ->
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
                                            Err <| "There is no buffer at index " ++ String.fromInt bufferView.buffer ++ ", as the buffer array is only " ++ (String.fromInt <| Array.length buffers) ++ " items long"

                                Nothing ->
                                    Err <| "There is no buffer view at index " ++ String.fromInt bufferViewIndex ++ ", as the buffer view array is only " ++ (String.fromInt <| Array.length gltf.bufferViews) ++ " items long"

                        Nothing ->
                            Err <| "The accessor at index " ++ String.fromInt accessorIndex ++ " has no buffer view index"
                )
            <|
                decoderConstructor accessor

        Nothing ->
            Err <| "There is no accessor at index " ++ String.fromInt accessorIndex ++ ", as the accessor array is only " ++ (String.fromInt <| Array.length gltf.accessors) ++ " items long"


loop : ( Bytes.Decode.Decoder a, Int ) -> Int -> Bytes.Decode.Decoder (List a)
loop ( decoder, increment ) count =
    Bytes.Decode.loop ( 0, [] )
        (\( done, accumulated ) ->
            if done < count then
                decoder
                    |> Bytes.Decode.map (\value -> Bytes.Decode.Loop ( done + increment, value :: accumulated ))

            else
                Bytes.Decode.Done (List.reverse accumulated)
                    |> Bytes.Decode.succeed
        )


getFloatVec2 : Raw.Gltf -> Array Bytes -> Int -> Result String (List ( Float, Float ))
getFloatVec2 =
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
                    (\floatDecoder ->
                        let
                            uvDecoder =
                                Bytes.Decode.map2 Tuple.pair floatDecoder floatDecoder
                        in
                        loop ( uvDecoder, 1 ) accessor.count
                    )
        )


getFloatVec3 : Raw.Gltf -> Array Bytes -> Int -> Result String (List ( Float, Float, Float ))
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
                    (\floatDecoder ->
                        let
                            vecDecoder =
                                Bytes.Decode.map3 (\x y z -> ( x, y, z ))
                                    floatDecoder
                                    floatDecoder
                                    floatDecoder
                        in
                        loop ( vecDecoder, 1 ) accessor.count
                    )
        )


getIntTriples : Raw.Gltf -> Array Bytes -> Int -> Result String (List ( Int, Int, Int ))
getIntTriples =
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
                    (\intDecoder ->
                        let
                            tripleDecoder =
                                Bytes.Decode.map3 (\i j k -> ( i, j, k ))
                                    intDecoder
                                    intDecoder
                                    intDecoder
                        in
                        loop ( tripleDecoder, 3 ) accessor.count
                    )
        )


getPositions : Raw.Gltf -> Array Bytes -> Raw.Mesh -> Int -> Raw.MeshPrimitive -> Result String (List (Point3d Meters coordinates))
getPositions gltf buffers mesh primitiveIndex primitive =
    case primitive.attributes.position of
        Just positionIndex ->
            getFloatVec3 gltf buffers positionIndex
                |> Result.map (List.map (\( x, y, z ) -> Point3d.meters x y z))

        Nothing ->
            Err ("The mesh" ++ (mesh.name |> Maybe.map (\name -> " " ++ name) |> Maybe.withDefault "") ++ "'s primitive (" ++ String.fromInt primitiveIndex ++ ") has no positions")


getNormals : Raw.Gltf -> Array Bytes -> Raw.MeshPrimitive -> Result String (Maybe (List (Vector3d Unitless coordinates)))
getNormals gltf buffers primitive =
    case primitive.attributes.normal of
        Just normalIndex ->
            getFloatVec3 gltf buffers normalIndex
                |> Result.map (Just << List.map (\( x, y, z ) -> Vector3d.unitless x y z))

        Nothing ->
            Ok Nothing


getUvs : Raw.Gltf -> Array Bytes -> Raw.MeshPrimitive -> Result String (Maybe (List ( Float, Float )))
getUvs gltf buffers primitive =
    case primitive.attributes.texCoords of
        Just texCoordsIndex ->
            Result.map Just (getFloatVec2 gltf buffers texCoordsIndex)

        Nothing ->
            Ok Nothing


getIndices : Raw.Gltf -> Array Bytes -> Raw.Mesh -> Int -> Raw.MeshPrimitive -> Result String (List ( Int, Int, Int ))
getIndices gltf buffers mesh primitiveIndex primitive =
    case primitive.indices of
        Just indicesIndex ->
            getIntTriples gltf buffers indicesIndex

        Nothing ->
            Err ("The mesh" ++ (mesh.name |> Maybe.map (\name -> " " ++ name) |> Maybe.withDefault "") ++ "'s primitive (" ++ String.fromInt primitiveIndex ++ ") has no indices")


getMaterial : Raw.Gltf -> Array Bytes -> Raw.MeshPrimitive -> Result String Material
getMaterial gltf buffers primitive =
    case primitive.material of
        Just materialIndex ->
            case Array.get materialIndex gltf.materials of
                Just material ->
                    case material.pbrMetallicRoughness of
                        Just pbr ->
                            getPbrBaseColor gltf buffers pbr
                                |> Result.map
                                    (\baseColor ->
                                        { baseColor = baseColor
                                        , metallic = pbr.metallicFactor
                                        , roughness = pbr.roughnessFactor
                                        }
                                    )

                        Nothing ->
                            Err <| "no PBR metallic roughness was found on material" ++ (material.name |> Maybe.map (\name -> " " ++ name) |> Maybe.withDefault "") ++ " at index " ++ String.fromInt materialIndex

                Nothing ->
                    Err <| "No material was found at index " ++ String.fromInt materialIndex ++ " (There are only " ++ String.fromInt (Array.length gltf.materials) ++ " materials)"

        Nothing ->
            Ok
                { baseColor = ConstantColor Color.white
                , metallic = 1.0
                , roughness = 1.0
                }


getPbrBaseColor : Raw.Gltf -> Array Bytes -> Raw.MaterialPbrMetallicRoughness -> Result String BaseColor
getPbrBaseColor gltf buffers pbr =
    case pbr.baseColorTexture of
        Just { index, texCoord } ->
            if texCoord == 0 then
                case Array.get index gltf.textures of
                    Just { sampler, source } ->
                        Result.map2
                            (\url options -> ColorTexture { url = url, options = options })
                            (getTextureSourceUrl gltf buffers source)
                            (getTextureOptions gltf sampler)

                    Nothing ->
                        Err <| "No texture found at index " ++ String.fromInt index

            else
                Err "Only texture coordinate 0 is currently supported"

        Nothing ->
            Ok (ConstantColor (Color.fromRgba pbr.baseColorFactor))


getTextureSourceUrl : Raw.Gltf -> Array Bytes -> Maybe Int -> Result String String
getTextureSourceUrl gltf buffers maybeImageIndex =
    case maybeImageIndex of
        Just imageIndex ->
            case Array.get imageIndex gltf.images of
                Just image ->
                    case image.uri of
                        Just url ->
                            -- Image has a URL, just return it
                            Ok url

                        Nothing ->
                            -- Image references part of a buffer,
                            -- form a data URL for it
                            case ( image.mimeType, image.bufferView ) of
                                ( Just mimeType, Just bufferView ) ->
                                    getImageDataUrl gltf buffers mimeType bufferView

                                ( _, Nothing ) ->
                                    Err "No buffer view supplied for texture"

                                ( Nothing, Just _ ) ->
                                    Err "No MIME type supplied for texture"

                Nothing ->
                    Err <| "No texture image found at index " ++ String.fromInt imageIndex

        Nothing ->
            Err "No source image defined for texture"


getImageDataUrl : Raw.Gltf -> Array Bytes -> Raw.ImageMimeType -> Int -> Result String String
getImageDataUrl gltf buffers mimeType bufferViewIndex =
    case Array.get bufferViewIndex gltf.bufferViews of
        Just bufferView ->
            case Array.get bufferView.buffer buffers of
                Just buffer ->
                    getBufferSlice bufferView.byteOffset bufferView.byteLength buffer
                        |> Result.map (dataUrlFromBytes (imageMimeTypeToString mimeType))

                Nothing ->
                    Err <| "No buffer found at index " ++ String.fromInt bufferView.buffer

        Nothing ->
            Err <| "No buffer view found at index " ++ String.fromInt bufferViewIndex


imageMimeTypeToString : Raw.ImageMimeType -> String
imageMimeTypeToString mimeType =
    case mimeType of
        Raw.ImagePng ->
            "image/png"

        Raw.ImageJpeg ->
            "image/jpeg"


dataUrlFromBytes : String -> Bytes -> String
dataUrlFromBytes mimeType data =
    let
        base64Data =
            Base64.fromBytes data |> Maybe.withDefault ""
    in
    "data:" ++ mimeType ++ ";base64," ++ base64Data


getBufferSlice : Int -> Int -> Bytes -> Result String Bytes
getBufferSlice offset length buffer =
    let
        decoder =
            Bytes.Decode.bytes offset |> Bytes.Decode.andThen (\_ -> Bytes.Decode.bytes length)
    in
    case Bytes.Decode.decode decoder buffer of
        Just slice ->
            Ok slice

        Nothing ->
            Err "Buffer view is out of range"


getTextureOptions : Raw.Gltf -> Maybe Int -> Result String WebGL.Texture.Options
getTextureOptions gltf maybeSamplerIndex =
    let
        defaultOptions =
            WebGL.Texture.defaultOptions
    in
    case maybeSamplerIndex of
        Just samplerIndex ->
            case Array.get samplerIndex gltf.samplers of
                Just sampler ->
                    let
                        magnify =
                            case sampler.magFilter of
                                Nothing ->
                                    defaultOptions.magnify

                                Just Raw.MagNearest ->
                                    WebGL.Texture.nearest

                                Just Raw.MagLinear ->
                                    WebGL.Texture.linear

                        minify =
                            case sampler.minFilter of
                                Nothing ->
                                    defaultOptions.minify

                                Just Raw.MinNearest ->
                                    WebGL.Texture.nearest

                                Just Raw.MinLinear ->
                                    WebGL.Texture.linear

                                Just Raw.NearestMipmapNearest ->
                                    WebGL.Texture.nearestMipmapNearest

                                Just Raw.LinearMipmapNearest ->
                                    WebGL.Texture.linearMipmapNearest

                                Just Raw.NearestMipmapLinear ->
                                    WebGL.Texture.nearestMipmapLinear

                                Just Raw.LinearMipmapLinear ->
                                    WebGL.Texture.linearMipmapLinear

                        wrapOption sampleWrap =
                            case sampleWrap of
                                Raw.ClampToEdge ->
                                    WebGL.Texture.clampToEdge

                                Raw.MirroredRepeat ->
                                    WebGL.Texture.mirroredRepeat

                                Raw.Repeat ->
                                    WebGL.Texture.repeat
                    in
                    Ok
                        { magnify = magnify
                        , minify = minify
                        , horizontalWrap = wrapOption sampler.wrapS
                        , verticalWrap = wrapOption sampler.wrapT
                        , flipY = False
                        }

                Nothing ->
                    Err <| "No sampler found at index " ++ String.fromInt samplerIndex

        Nothing ->
            Ok defaultOptions


type alias Primitives coordinates =
    { positions : List (Point3d Meters coordinates)
    , normals : Maybe (List (Vector3d Unitless coordinates))
    , uvs : Maybe (List ( Float, Float ))
    , indices : List ( Int, Int, Int )
    , material : Material
    }


getPrimitiveAttributes : Raw.Gltf -> Array Bytes -> Raw.Mesh -> Int -> Raw.MeshPrimitive -> Result String (Primitives coordinates)
getPrimitiveAttributes gltf buffers mesh primitiveIndex primitive =
    Result.map5 Primitives
        (getPositions gltf buffers mesh primitiveIndex primitive)
        (getNormals gltf buffers primitive)
        (getUvs gltf buffers primitive)
        (getIndices gltf buffers mesh primitiveIndex primitive)
        (getMaterial gltf buffers primitive)


type Mesh coordinates
    = Triangles (TriangularMesh (Point3d Meters coordinates))
    | Faces (TriangularMesh { position : Point3d Meters coordinates, normal : Vector3d Unitless coordinates })
    | TexturedTriangles (TriangularMesh { position : Point3d Meters coordinates, uv : ( Float, Float ) })
    | TexturedFaces (TriangularMesh { position : Point3d Meters coordinates, normal : Vector3d Unitless coordinates, uv : ( Float, Float ) })


type BaseColor
    = ConstantColor Color
    | ColorTexture
        { url : String
        , options : WebGL.Texture.Options
        }


type alias Material =
    { baseColor : BaseColor
    , metallic : Float
    , roughness : Float
    }


toMesh : Mat4 -> Primitives coordinates -> ( Mesh coordinates, Material )
toMesh transformation primitives =
    let
        triangularMesh vertexList =
            TriangularMesh.indexed (Array.fromList vertexList) primitives.indices
    in
    ( case ( primitives.normals, primitives.uvs ) of
        ( Nothing, Nothing ) ->
            Triangles <|
                triangularMesh <|
                    List.map (Point3d.transformBy transformation) primitives.positions

        ( Just normals, Nothing ) ->
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

        ( Nothing, Just uvs ) ->
            TexturedTriangles <|
                triangularMesh <|
                    List.map2
                        (\position uv ->
                            { position = Point3d.transformBy transformation position
                            , uv = uv
                            }
                        )
                        primitives.positions
                        uvs

        ( Just normals, Just uvs ) ->
            TexturedFaces <|
                triangularMesh <|
                    List.map3
                        (\position normal uv ->
                            { position = Point3d.transformBy transformation position
                            , normal = Vector3d.transformBy transformation normal
                            , uv = uv
                            }
                        )
                        primitives.positions
                        normals
                        uvs
    , primitives.material
    )


toMeshes : Raw.Gltf -> Array Bytes -> ( Raw.Mesh, Mat4 ) -> Result String (List ( Mesh coordinates, Material ))
toMeshes gltf buffers ( mesh, transformation ) =
    List.indexedMap (getPrimitiveAttributes gltf buffers mesh) mesh.primitives
        |> List.map (Result.map (toMesh transformation))
        |> Result.Extra.combine


skipBytes : Int -> Bytes.Decode.Decoder a -> Bytes.Decode.Decoder a
skipBytes skip decoder =
    Bytes.Decode.bytes skip |> Bytes.Decode.andThen (\_ -> decoder)


meshesFromDefaultScene : Bytes -> Result String (List ( Mesh coordinates, Material ))
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
                                    |> Bytes.Decode.andThen (\b -> Bytes.Decode.succeed ( gltf, Array.fromList [ b ] ))

                            Err _ ->
                                Bytes.Decode.fail
                    )
                |> Bytes.Decode.decode
    in
    case decoder bytes of
        Just ( gltf, buffers ) ->
            assembleDefaultScene gltf buffers

        Nothing ->
            Err "The file is malformed"


assembleDefaultScene : Raw.Gltf -> Array Bytes -> Result String (List ( Mesh coordinates, Material ))
assembleDefaultScene gltf buffers =
    case gltf.scene of
        Just sceneIndex ->
            case Array.get sceneIndex gltf.scenes of
                Just scene ->
                    scene.nodes
                        |> Array.toList
                        |> List.map (getNode gltf)
                        |> List.map (Result.andThen (getMeshes gltf))
                        |> List.map (Result.andThen (\meshes -> meshes |> List.map (toMeshes gltf buffers) |> Result.Extra.combine))
                        |> List.map (Result.map List.concat)
                        |> Result.Extra.combine
                        |> Result.map List.concat

                Nothing ->
                    Err <| "The default scene index (" ++ String.fromInt sceneIndex ++ ") is out of bound of the array of scenes (" ++ (String.fromInt <| Array.length gltf.scenes) ++ " items)"

        Nothing ->
            Err "There is no default scene in the file"
