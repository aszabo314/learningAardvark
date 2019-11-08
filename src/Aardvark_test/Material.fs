namespace Aardvark_test

open System
open Aardvark.Base
open Aardvark.Base.Incremental
open Aardvark.UI
open Aardvark.UI.Primitives
open Aardvark.SceneGraph
open Aardvark_test.Model
open System.IO

module material = 

    let onePxPix (color :C3f)= 
        let pi = PixImage<byte>(Col.Format.RGB, V2i.II)
        pi.GetMatrix<C3f>().SetByCoord(fun (c : V2l) -> color) |> ignore
        pi

    let onPixTex (color :C3f) = 
        PixTexture2d(PixImageMipMap [| onePxPix color :> PixImage |], false) :> ITexture

    type ProxyMaterial =
        {
            importedMaterial : IO.Loader.IMaterial
            material : IMod<MPBRMaterial>
        }
        
        member x.DisplacemntMap =
            let loadTex f =
                f
                |> Option.map (fun f -> FileTexture(f, TextureParams.empty) :> ITexture)
                |> Option.defaultValue (onPixTex C3f.Gray50)
            Mod.bind (fun (m : MPBRMaterial)-> m.displacment.fileName |> Mod.map loadTex)  x.material :> IMod

        member x.MetallicMap =
            let loadTex f =
                f
                |> Option.map (fun f -> FileTexture(f, TextureParams.empty) :> ITexture)
                |> Option.defaultValue (onPixTex C3f.White)
            Mod.bind (fun (m : MPBRMaterial)-> m.metallic.fileName |> Mod.map loadTex)  x.material :> IMod

        member x.RoughnessMap =
            let loadTex f =
                f
                |> Option.map (fun f -> FileTexture(f, TextureParams.empty) :> ITexture)
                |> Option.defaultValue (onPixTex C3f.White)
            Mod.bind (fun (m : MPBRMaterial)-> m.roughness.fileName |> Mod.map loadTex)  x.material :> IMod

        member x.OpacityMap =
            let loadTex f =
                f
                |> Option.map (fun f -> FileTexture(f, TextureParams.empty) :> ITexture)
                |> Option.defaultValue (onPixTex C3f.White)
            Mod.bind (fun (m : MPBRMaterial)-> m.opacity.fileName |> Mod.map loadTex)  x.material :> IMod

        member x.AlbedoMap =
            adaptive {
                let! m = x.material 
                let! f = m.albedo.fileName
                let im =  x.importedMaterial :?>  IO.Loader.Material
                let d = 
                    match im.textures.TryFind DefaultSemantic.DiffuseColorTexture  with
                    |Some t -> t.texture 
                    |None -> onPixTex C3f.White
                return match f with
                        | Some file -> FileTexture(file, TextureParams.empty) :> ITexture
                        | None ->  d
            }
        
        member x.NormalMap =
            adaptive {
                let! m = x.material 
                let! f = m.normal.fileName
                let im =  x.importedMaterial :?>  IO.Loader.Material
                let d = 
                    match im.textures.TryFind DefaultSemantic.NormalMapTexture  with
                    |Some t -> t.texture 
                    |None ->  onPixTex C3f.White
                return match f with
                        | Some file -> FileTexture(file, TextureParams.empty) :> ITexture
                        | None ->  d
            }

        interface IO.Loader.IMaterial with

            member x.name = x.importedMaterial.name

            member x.TryGetUniform(s, sem) =
                match string sem with
                | "Metallic" -> Some (Mod.bind (fun (m : MPBRMaterial)-> m.metallic.factor) x.material :> IMod)
                | "MetallicMap" -> Some x.MetallicMap 
                | "Roughness" -> Some (Mod.bind (fun (m : MPBRMaterial)-> m.roughness.factor) x.material :> IMod)
                | "RoughnessMap" -> Some x.RoughnessMap 
                | "DiffuseColorTexture" -> Some (x.AlbedoMap :> IMod)
                | "AlbedoFactor" -> Some (Mod.bind (fun (m : MPBRMaterial)-> m.albedo.factor) x.material :> IMod)
                | "NormalMapStrength" -> Some (Mod.bind (fun (m : MPBRMaterial)-> m.normal.factor) x.material :> IMod)
                | "NormalMapTexture" -> Some (x.NormalMap :> IMod)
                | "Discard" -> Some (Mod.bind (fun (m : MPBRMaterial)-> m.discard) x.material :> IMod)
                | "DisplacmentStrength" -> Some (Mod.bind (fun (m : MPBRMaterial)-> m.displacment.factor) x.material :> IMod)
                | "DisplacmentMap" -> Some x.DisplacemntMap 
                | "Opacity" -> Some (Mod.bind (fun (m : MPBRMaterial)-> m.opacity.factor) x.material :> IMod)
                | "OpacityMap" -> Some x.OpacityMap 
                | _ -> x.importedMaterial.TryGetUniform(s, sem)

            member x.Dispose() = x.importedMaterial.Dispose()

    let getMaterials (s : IO.Loader.Scene) =
            let rec traverse (state : IO.Loader.IMaterial list) (n : IO.Loader.Node) =
                match n with
                    | IO.Loader.Node.Empty -> 
                        state
                    | IO.Loader.Node.Group es ->
                        List.fold traverse state es 
                    | IO.Loader.Node.Leaf m ->
                        state
                    | IO.Loader.Node.Material(m, n) ->
                        m::state
                    | IO.Loader.Node.Trafo(t, n) ->
                        traverse state n

            traverse [] s.root 
    
    let removeDigits = String.filter (Char.IsDigit >> not)

    let defaultMaterial = {
        metallic = {
            fileName = None
            factor = 0.0
        }
        roughness = {
            fileName = None
            factor = 0.8
        }
        albedo ={
            fileName = None
            factor = 1.0
        }
        normal = {
            fileName = None
            factor = 1.0
        }
        opacity = {
            fileName = None
            factor = 1.0
        }
        albedoFactor = 1.0
        normalMapStrenght = 1.0
        discard = false
        displacment = {
            fileName = None
            factor = 0.0
        }
    }

    let materials model = 
        getMaterials model
        |> List.map (fun m -> removeDigits m.name)
        |> List.distinct
        |> List.map (fun n -> n, defaultMaterial)
        |> HMap.ofList
    
module textureMappedValueControl =

    type Message =
        | SetMap of string
        | RemoveMap
        | SetFactor of float

    let update (m :TextureMappedValue) (msg : Message)  =
        match msg with
        | SetMap file -> {m with fileName = Some file}
        | RemoveMap -> {m with fileName = None}
        | SetFactor f -> {m with  factor = f}

    type Kind =
    |Linear
    |Log

    let view kind titel min max step (m : MTextureMappedValue) =
        let slider =
            match kind with
            |Linear -> inputSlider {min =min;  max = max; step = step} [] m.factor SetFactor
            |Log -> inputLogSlider {min =min;  max = max; step = step} [] m.factor SetFactor
        let openButton = 
            openDialogButton 
                { OpenDialogConfig.file with allowMultiple = false; title = sprintf "Open %s" titel; filters  = [|"*.*"|];  startPath = ""; mode  = OpenDialogMode.File}
                [ clazz "ui green button"; onChooseFile SetMap ] 
                [ text "Choose File" ]
        let removeButton = 
            m.fileName
            |> Mod.map (fun f -> match f with |Some fn -> PList.single(button [clazz "ui button"; onClick (fun _ -> RemoveMap)]  [text "Remove"]) |None -> PList.empty)
            |> AList.ofMod
            |> Incremental.div AttributeMap.empty
        let name = m.fileName |> Mod.map (Option.map (fun f -> IO.Path.GetFileNameWithoutExtension(f)) >> Option.defaultValue "none") |> Incremental.text
        Html.table [                        
            tr [] [ td [] [text titel]; td [style "width: 70%;"; attribute "colspan" "2"] [slider]]
            tr [] [ td [] [openButton]; td [] [name]; td [] [removeButton]]
        ]        

module materialControl = 

    type Message =
        | SetMetallic of textureMappedValueControl.Message
        | SetRoughness of textureMappedValueControl.Message
        | SetNormal of textureMappedValueControl.Message
        | SetAlbedo of textureMappedValueControl.Message
        | SetAlbedoFactor of float
        | SetNormalMapStrength of float
        | SetDiscard 
        | SetDisplacment of textureMappedValueControl.Message
        | SetOpacity of textureMappedValueControl.Message

    let update  (m : PBRMaterial) (msg : Message)  =
        match msg with
        | SetMetallic msg' -> { m with  metallic = textureMappedValueControl.update m.metallic msg'}
        | SetRoughness msg' -> { m with  roughness = textureMappedValueControl.update m.roughness msg'}
        | SetAlbedo msg' -> { m with  albedo = textureMappedValueControl.update m.albedo msg'}
        | SetNormal msg' -> { m with  normal = textureMappedValueControl.update m.normal msg'}
        | SetAlbedoFactor a -> { m with  albedoFactor = a}
        | SetNormalMapStrength s -> { m with  normalMapStrenght = s}
        | SetDiscard -> { m with  discard = not m.discard}
        | SetDisplacment msg' -> { m with  displacment = textureMappedValueControl.update m.displacment msg'}
        | SetOpacity msg' -> { m with  opacity = textureMappedValueControl.update m.opacity msg'}

    let view (m : MPBRMaterial) =
        let numInput name changed state  = labeledFloatInput name 0.0 1.0 0.01 changed state
        div [] [
            textureMappedValueControl.view textureMappedValueControl.Linear "Metallic" 0.0 1.0 0.01 m.metallic  |> UI.map SetMetallic
            textureMappedValueControl.view textureMappedValueControl.Linear "Roughness" 0.0 1.0 0.01 m.roughness  |> UI.map SetRoughness
            textureMappedValueControl.view textureMappedValueControl.Linear "Albedo" 0.0 1.0 0.01 m.albedo  |> UI.map SetAlbedo
            textureMappedValueControl.view textureMappedValueControl.Linear "Normal Map" 0.0 1.0 0.01 m.normal  |> UI.map SetNormal
            textureMappedValueControl.view textureMappedValueControl.Linear "Displacement" 0.0 1.0 0.01 m.displacment  |> UI.map SetDisplacment
            textureMappedValueControl.view textureMappedValueControl.Linear "Opacity" 0.0 1.0 0.01 m.opacity  |> UI.map SetOpacity
            Html.table [                        
                 tr [] [ td [] [text "Discard"]; td [style "width: 70%;"] [Html.SemUi.toggleBox  m.discard SetDiscard ]]
            ]   
        ]