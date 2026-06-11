{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Engine.Atlas where

import Engine.Other
import Engine.Type
import qualified SDL.Constant as C
import qualified SDL.Function as F
import qualified SDL.Type as T
import qualified Control.Monad as CM
import qualified Data.IntMap as DIM
import qualified Data.Word as DW
import qualified Foreign.C.String as FCS
import qualified Foreign.C.Types as FCT
import qualified Foreign.Marshal.Utils as FMU
import qualified Foreign.Ptr as FP

init_atlas::DW.Word32->DW.Word32->Int->Atlas
init_atlas width height next=Atlas {next=next,width=fromIntegral width,height=fromIntegral height,pack=Leaf_pack {rectangle=Rectangle {left=0,down=0,right=width,up=height},used=False},region=DIM.empty}

atlas_insert_initial::DW.Word32->DW.Word32->DW.Word32->Atlas->(Atlas,Int,DW.Word32,DW.Word32,FCT.CFloat,FCT.CFloat)
atlas_insert_initial width height padding atlas=case atlas_insert_a (width+2*padding) (height+2*padding) atlas.pack of
    Just (pack,left,down,right,up)->let new_left=left+padding in let new_down=down+padding in let new_right=right-padding in let new_up=up-padding in let min_u=fromIntegral new_left/atlas.width in let min_v=fromIntegral new_down/atlas.height in let max_u=fromIntegral new_right/atlas.width in let max_v=fromIntegral new_up/atlas.height in (atlas {next=atlas.next+1,pack=pack,region=DIM.insert atlas.next (Region {min_u=min_u,min_v=min_v,max_u=max_u,max_v=max_v}) atlas.region},atlas.next,new_left,new_down,(min_u+max_u)/2,(min_v+max_v)/2)
    _->error "atlas_insert_initial: error 1"

atlas_insert::DW.Word32->DW.Word32->DW.Word32->Atlas->(Atlas,Int,DW.Word32,DW.Word32)
atlas_insert width height padding atlas=case atlas_insert_a (width+2*padding) (height+2*padding) atlas.pack of
    Just (pack,left,down,right,up)->let new_left=left+padding in let new_down=down+padding in let new_right=right-padding in let new_up=up-padding in (atlas {next=atlas.next+1,pack=pack,region=DIM.insert atlas.next (Region {min_u=fromIntegral new_left/atlas.width,min_v=fromIntegral new_down/atlas.height,max_u=fromIntegral new_right/atlas.width,max_v=fromIntegral new_up/atlas.height}) atlas.region},atlas.next,new_left,new_down)
    _->error "atlas_insert: error 1"

atlas_insert_a::DW.Word32->DW.Word32->Pack->Maybe (Pack,DW.Word32,DW.Word32,DW.Word32,DW.Word32)
atlas_insert_a width height pack=case pack of
    Leaf_pack {rectangle,used}->case rectangle of
        Rectangle {left,down,right,up}->let new_right=rectangle.left+width in let new_up=rectangle.down+height in let new_width=rectangle.right-new_right in let new_height=rectangle.up-new_up in if used||new_width<0||new_height<0 then Nothing else if new_width==0&&new_height==0 then Just (Leaf_pack {rectangle=rectangle,used=True},left,down,right,up) else let (unused_rectangle,left_rectangle,right_rectangle)=if new_width<new_height then (Rectangle {left=new_right,down=rectangle.down,right=rectangle.right,up=new_up},Rectangle {left=rectangle.left,down=rectangle.down,right=rectangle.right,up=new_up},Rectangle {left=rectangle.left,down=new_up,right=rectangle.right,up=rectangle.up}) else (Rectangle {left=rectangle.left,down=new_up,right=new_right,up=rectangle.up},Rectangle {left=rectangle.left,down=rectangle.down,right=new_right,up=rectangle.up},Rectangle {left=new_right,down=rectangle.down,right=rectangle.right,up=rectangle.up}) in Just (Node_pack {rectangle=rectangle,left_pack=Node_pack {rectangle=left_rectangle,left_pack=Leaf_pack {rectangle=Rectangle {left=rectangle.left,down=rectangle.down,right=new_right,up=new_up},used=True},right_pack=Leaf_pack {rectangle=unused_rectangle,used=False}},right_pack=Leaf_pack {rectangle=right_rectangle,used=False}},rectangle.left,rectangle.down,new_right,new_up)
    Node_pack {rectangle,left_pack,right_pack}->case atlas_insert_a width height left_pack of
        Nothing->case atlas_insert_a width height right_pack of
            Nothing->Nothing
            Just (new_right_pack,left,down,right,up)->Just (Node_pack {rectangle=rectangle,left_pack=left_pack,right_pack=new_right_pack},left,down,right,up)
        Just (new_left_pack,left,down,right,up)->Just (Node_pack {rectangle=rectangle,left_pack=new_left_pack,right_pack=right_pack},left,down,right,up)
    
load_texture::FP.Ptr T.SDL_GPUDevice->FP.Ptr T.SDL_GPUTransferBuffer->FCT.CInt->String->IO (FP.Ptr T.SDL_GPUTexture,DW.Word32,DW.Word32)
load_texture device picture_transfer_buffer picture_size path=FCS.withCString path $ \this_path->do
    surface<-F.img_load this_path
    catch_null surface
    new_surface<-F.sdl_convertsurface surface C.sdl_pixelformat_rgba32
    catch_null new_surface
    F.sdl_destroysurface surface
    width<-C.sdl_surface_w new_surface
    height<-C.sdl_surface_h new_surface
    pixel<-C.sdl_surface_pixels new_surface
    let new_width=fromIntegral width
    let new_height=fromIntegral height
    let size=4*width*height
    CM.when (picture_size<size) (error "load_texture: error 1")
    texture<-FMU.with (C.SDL_GPUTextureCreateInfo {sdl_type=C.sdl_gpu_texturetype_2d,sdl_format=C.sdl_gpu_textureformat_r8g8b8a8_unorm,sdl_usage=C.sdl_gpu_textureusage_sampler,sdl_width=new_width,sdl_height=new_height,sdl_layer_count_or_depth=1,sdl_num_levels=1,sdl_sample_count=C.sdl_gpu_samplecount_1}) (return_catch_null . F.sdl_creategputexture device)
    map_transfer_buffer<-F.sdl_mapgputransferbuffer device picture_transfer_buffer (FMU.fromBool True)
    catch_null map_transfer_buffer
    FMU.copyBytes (FP.castPtr map_transfer_buffer) (FP.castPtr pixel) (fromIntegral size)
    F.sdl_unmapgputransferbuffer device picture_transfer_buffer
    command_buffer<-F.sdl_acquiregpucommandbuffer device
    catch_null command_buffer
    F.sdl_destroysurface new_surface
    copy_pass<-F.sdl_begingpucopypass command_buffer
    catch_null copy_pass
    FMU.with (C.SDL_GPUTextureTransferInfo {sdl_transfer_buffer=picture_transfer_buffer,sdl_offset=0,sdl_pixels_per_row=new_width,sdl_rows_per_layer=new_height}) $ \texture_transfer_info->FMU.with (C.SDL_GPUTextureRegion {sdl_texture=texture,sdl_mip_level=0,sdl_layer=0,sdl_x=0,sdl_y=0,sdl_z=0,sdl_w=new_width,sdl_h=new_height,sdl_d=1}) $ \texture_region->F.sdl_uploadtogputexture copy_pass texture_transfer_info texture_region (FMU.fromBool False)
    F.sdl_endgpucopypass copy_pass
    catch_false (F.sdl_submitgpucommandbuffer command_buffer)
    return (texture,new_width,new_height)

copy_texture::FP.Ptr T.SDL_GPUDevice->FP.Ptr T.SDL_GPUTexture->FP.Ptr T.SDL_GPUTexture->DW.Word32->DW.Word32->DW.Word32->DW.Word32->IO ()
copy_texture device texture_from texture_to x y width height=do
    command_buffer<-F.sdl_acquiregpucommandbuffer device
    catch_null command_buffer
    copy_pass<-F.sdl_begingpucopypass command_buffer
    catch_null copy_pass
    FMU.with (C.SDL_GPUTextureLocation {sdl_texture=texture_from,sdl_mip_level=0,sdl_layer=0,sdl_x=0,sdl_y=0,sdl_z=0}) $ \texture_location_from->FMU.with (C.SDL_GPUTextureLocation {sdl_texture=texture_to,sdl_mip_level=0,sdl_layer=0,sdl_x=x,sdl_y=y,sdl_z=0}) $ \texture_location_to->F.sdl_copygputexturetotexture copy_pass texture_location_from texture_location_to width height 1 (FMU.fromBool False)
    F.sdl_endgpucopypass copy_pass
    catch_false (F.sdl_submitgpucommandbuffer command_buffer)