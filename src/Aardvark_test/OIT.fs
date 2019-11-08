namespace Aardvark_test

open System
open Aardvark.Base
open Aardvark.Base.Rendering
open Aardvark.SceneGraph
open FShade
open Aardvark.Base.Rendering.Effects

module shadersOIT =
    open PBR
    open fshadeExt

    type UniformScope with
        member x.Roughness : float = x?Roughness

        member x.Metallic : float = x?Metallic

        member x.AlbedoFactor : float = x?AlbedoFactor

        member x.Opacity : float = x?Opacity

        member x.Discard : bool =  x?Discard

        member x.SkyMapIntensity : float =  x?SkyMapIntensity

        member x.LightPasses : float =  x?LightPasses


    let private metallicSampler =
        sampler2d {
            texture uniform?MetallicMap
            filter Filter.MinMagMipLinear
            addressU WrapMode.Wrap
            addressV WrapMode.Wrap
        }

    let private roughnessSampler =
        sampler2d {
            texture uniform?RoughnessMap
            filter Filter.MinMagMipLinear
            addressU WrapMode.Wrap
            addressV WrapMode.Wrap
        }
   
    let private opacitySampler =
        sampler2d {
            texture uniform?OpacityMap
            filter Filter.MinMagMipLinear
            addressU WrapMode.Wrap
            addressV WrapMode.Wrap
        }

    let private backgroundSampler =
        sampler2d {
            texture uniform?Background
            filter Filter.MinMagMipLinear
            addressU WrapMode.Wrap
            addressV WrapMode.Wrap
        }

    let private depthCmp =
        sampler2dShadow {
            texture uniform?Depth
            addressU WrapMode.Clamp
            addressV WrapMode.Clamp
            comparison ComparisonFunction.Greater
            filter Filter.MinMagMipLinear
        }

    let private foregroundSampler =
        sampler2d {
            texture uniform?Foreground
            filter Filter.MinMagMipLinear
            addressU WrapMode.Wrap
            addressV WrapMode.Wrap
        }

    type Fragment = {
        [<Position>]        pos     : V4d
        [<WorldPosition>]   wp      : V4d
        [<Normal>]          n       : V3d
        [<Color>]           c       : V4d
        [<TexCoord>]        tc      : V2d
        [<Semantic("Metallic")>] metallic    : float
        [<Semantic("Roughness")>] roughness    : float
    }

    let fragmentData (vert : Vertex) =
        fragment {
            let gamma  = 2.2
            
            let pos = (vert.pos.XYZ / vert.pos.W + V3d.III) * 0.5
            if  depthCmp.Sample(pos.XY, pos.Z) > 0.5 then
                discard()

            let opacity = uniform.Opacity * opacitySampler.Sample(vert.tc).X
            if uniform.Discard || opacity >= 0.99 then
                    discard()

            let albedo = pow (vert.c.XYZ * uniform.AlbedoFactor) (V3d(gamma))
            let metallic = uniform.Metallic * metallicSampler.Sample(vert.tc).X
            let roughness = uniform.Roughness * roughnessSampler.Sample(vert.tc).X

            return  {pos = vert.pos; wp =  vert.wp; n = vert.n; c = V4d(albedo,opacity);  tc = vert.tc; metallic = metallic; roughness = roughness}
        }

    [<ReflectedDefinition>]
    let pbrDirect f0 roughness metallic (albedo : V3d) (wPos : V4d) v n nDotV light alpha background = 
           
        let (exists, lDir, radiance)  = getLightParams light wPos.XYZ
      
        let oi = 
            if exists then
                let h = v + lDir |> Vec.normalize

                // cook-torrance brdf
                let ndf = DistributionGGX n h roughness 
                let g = GeometrySmith false n v lDir roughness 
                let hDotV = Vec.dot h v |>  max 0.0      
                let kS = fresnelSchlick f0 hDotV   
                let nDotL = Vec.dot n lDir |>  max 0.0
            
                let kD = (1.0 - metallic) * (V3d.III - kS)
                let diffuse = kD * albedo / Math.PI * radiance * nDotL

                let diffuseBlended = Lerp background diffuse alpha

                let numerator = ndf * g * kS
                let denominator = 4.0 * nDotV * nDotL |> max 0.001
                let specular = numerator / denominator  * radiance * nDotL

                // add to outgoing radiance from single light
                diffuseBlended + specular 

            else V3d.Zero
        oi

    [<ReflectedDefinition>]
    let pBRLightning metallic roughness albedo n (wPos : V4d) alpha background =
        
        let cameraPos = uniform.CameraLocation

        let v = cameraPos - wPos.XYZ |> Vec.normalize
        let r = Vec.reflect -v n

        //asume 0.04 as F0 for non metals, set albedo as specular color for metallics
        let f0 = Lerp (V3d(0.04)) albedo metallic

        let nDotV = Vec.dot n v |>  max 0.0
        let light = uniform.Light
        let directLight = pbrDirect f0 roughness metallic albedo wPos v n nDotV light alpha background
        directLight

    let lightingOIT (frag : Fragment)  =
        fragment {
            let col = 
                if frag.metallic < 0.0 then //no lighting, just put out the color      
                    V3d.Zero
                else //PBR lightning
                    let metallic = frag.metallic
                    let roughness = frag.roughness
                    let albedo = frag.c.XYZ
                    let n = frag.n
                    let wPos = frag.wp
                    let alpha = frag.c.W
                    let pos = (frag.pos.XYZ / frag.pos.W + V3d.III) * 0.5
                    let background = backgroundSampler.Sample(pos.XY).XYZ / uniform.LightPasses
                    pBRLightning metallic roughness albedo n wPos alpha background

            return V4d(col, 1.0)
        }

    [<ReflectedDefinition>]
    let pBRAbientLight f0 roughness metallic (albedo : V3d) n r nDotV alpha background =
        let ambientIntensity = uniform.AmbientIntensity
        let kSA = fresnelSchlickRoughness f0 roughness nDotV
        let kdA  = (1.0 - kSA) * (1.0 - metallic)
        let irradiance = diffuseIrradianceSampler.Sample(n).XYZ
        let diffuse = kdA * irradiance * albedo * ambientIntensity
        let diffuseBlended = background//Lerp background diffuse alpha

        let maxReflectLod = 4.0
        let prefilteredColor = prefilteredSpecColorSampler.SampleLevel(r, roughness * maxReflectLod).XYZ
        let brdf = samplerBRDFLtu.Sample(V2d(nDotV, roughness)).XY
        let specular = prefilteredColor * (kSA * brdf.X + brdf.Y) * ambientIntensity

        let ambient = diffuseBlended + specular
        ambient 

    [<ReflectedDefinition>]
    let pBRAbient metallic roughness albedo n (wPos : V4d) alpha background=

        let cameraPos = uniform.CameraLocation
        let v = cameraPos - wPos.XYZ |> Vec.normalize
        let r = Vec.reflect -v n

        //asume 0.04 as F0 for non metals, set albedo as specular color for metallics
        let f0 = Lerp (V3d(0.04)) albedo metallic

        let nDotV = Vec.dot n v |>  max 0.0
        let ambient = pBRAbientLight f0 roughness metallic albedo n r nDotV alpha background
        ambient

    let abientOIT (frag : Fragment) =
        fragment {
            let col = 
                if frag.metallic < 0.0 then //no lighting, jusSt put out the color      
                    V3d.Zero
                else //PBR lightning
                    let metallic = frag.metallic
                    let roughness = frag.roughness
                    let albedo = frag.c.XYZ
                    let n = frag.n
                    let wPos = frag.wp
                    let alpha = frag.c.W
                    let pos = (frag.pos.XYZ / frag.pos.W + V3d.III) * 0.5
                    let background = backgroundSampler.Sample(pos.XY).XYZ / uniform.LightPasses
                    pBRAbient metallic roughness albedo n wPos alpha background

            let occlusion = ambientOcc.Sample(frag.tc).X

            return V4d(col * occlusion, 1.0)
        }

    let transparent (vert : Vertex) =
        vertex {
            return {vert with c = V4d(V3d.Zero,0.0); pos = V4d(vert.pos.X,vert.pos.Y,vert.pos.W,vert.pos.W)}
        }

    let compose (vert : Vertex)  =
        fragment {
            let background = backgroundSampler.Sample(vert.tc)
            let foreground = foregroundSampler.Sample(vert.tc)
            let col = if foreground.W > 0.0 then foreground.XYZ else background.XYZ 
            let alpha = 1.0 - (1.0-background.W) * (1.0-foreground.W)
            return V4d(col,alpha)
        }
    