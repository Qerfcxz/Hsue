{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}

module Engine.Atlas where

import Engine.Helper
import Engine.Type
import qualified SDL.Function as SDLF
import qualified SDL.Include as SDLI
import qualified SDL.Type as SDLT
import qualified Error.Error as EE
import qualified Control.Monad as CM
import qualified Data.Word as DW
import qualified Foreign.C.String as FCS
import qualified Foreign.C.Types as FCT
import qualified Foreign.Marshal.Utils as FMU
import qualified Foreign.Ptr as FP

init_atlas::DW.Word32->DW.Word32->Atlas
init_atlas width height=Leaf_atlas {border=Border {left=0,down=0,right=width,up=height},used=False}

atlas_insert::DW.Word32->DW.Word32->DW.Word32->Atlas->(Atlas,DW.Word32,DW.Word32,DW.Word32,DW.Word32)
atlas_insert width height padding atlas=case atlas_insert_a (width+2*padding) (height+2*padding) atlas of
    Just (new_atlas,left,down,right,up)->(new_atlas,left+padding,down+padding,right-padding,up-padding)
    _->EE.quick_error "atlas_insert" 0

atlas_insert_a::DW.Word32->DW.Word32->Atlas->Maybe (Atlas,DW.Word32,DW.Word32,DW.Word32,DW.Word32)
atlas_insert_a width height atlas=case atlas of
    Leaf_atlas {border,used}->case border of
        Border {left,down,right,up}->let new_right=left+width in let new_up=down+height in if used||right<new_right||up<new_up then Nothing else if right==new_right&&up==new_up then Just (Leaf_atlas {border=border,used=True},left,down,right,up) else let (unused_border,left_border,right_border)=if right+new_up<up+new_right then (Border {left=new_right,down=down,right=right,up=new_up},Border {left=left,down=down,right=right,up=new_up},Border {left=left,down=new_up,right=right,up=up}) else (Border {left=left,down=new_up,right=new_right,up=up},Border {left=left,down=down,right=new_right,up=up},Border {left=new_right,down=down,right=right,up=up}) in Just (Node_atlas {border=border,left_atlas=Node_atlas {border=left_border,left_atlas=Leaf_atlas {border=Border {left=left,down=down,right=new_right,up=new_up},used=True},right_atlas=Leaf_atlas {border=unused_border,used=False}},right_atlas=Leaf_atlas {border=right_border,used=False}},left,down,new_right,new_up)
    Node_atlas {border,left_atlas,right_atlas}->case atlas_insert_a width height left_atlas of
        Nothing->case atlas_insert_a width height right_atlas of
            Nothing->Nothing
            Just (new_right_atlas,left,down,right,up)->Just (Node_atlas {border=border,left_atlas=left_atlas,right_atlas=new_right_atlas},left,down,right,up)
        Just (new_left_atlas,left,down,right,up)->Just (Node_atlas {border=border,left_atlas=new_left_atlas,right_atlas=right_atlas},left,down,right,up)

from_image::FP.Ptr SDLT.SDL_GPUDevice->FP.Ptr SDLT.SDL_GPUTransferBuffer->FCT.CInt->String->IO (FP.Ptr SDLT.SDL_GPUTexture,DW.Word32,DW.Word32)
from_image device picture_transfer_buffer picture_size path=FCS.withCString path $ \this_path->do
    surface<-SDLF.img_load this_path
    catch_null surface
    new_surface<-SDLF.sdl_convert_surface surface SDLI.sdl_pixelformat_rgba32
    catch_null new_surface
    SDLF.sdl_destroy_surface surface
    width<-SDLI.sdl_surface_w new_surface
    height<-SDLI.sdl_surface_h new_surface
    pitch<-SDLI.sdl_surface_pitch new_surface
    pixel<-SDLI.sdl_surface_pixels new_surface
    let new_width=fromIntegral width
    let new_height=fromIntegral height
    let new_pitch=4*width
    let size=new_pitch*height
    CM.when (picture_size<size) (EE.quick_error "from_image" 0)
    texture<-upload_texture device picture_transfer_buffer new_width new_height (\map_transfer_buffer->if pitch==new_pitch then FMU.copyBytes (FP.castPtr map_transfer_buffer) (FP.castPtr pixel) (fromIntegral size) else CM.forM_ [0..height-1] $ \y->FMU.copyBytes (FP.plusPtr map_transfer_buffer (fromIntegral (y*new_pitch))) (FP.plusPtr pixel (fromIntegral (y*pitch))) (fromIntegral new_pitch))
    SDLF.sdl_destroy_surface new_surface
    return (texture,new_width,new_height)

from_pixel::FP.Ptr SDLT.SDL_GPUDevice->FP.Ptr SDLT.SDL_GPUTransferBuffer->FCT.CInt->FP.Ptr DW.Word8->DW.Word32->DW.Word32->IO (FP.Ptr SDLT.SDL_GPUTexture)
from_pixel device picture_transfer_buffer picture_size pixel width height=let size=fromIntegral (4*width*height) in do
    CM.when (picture_size<size) (EE.quick_error "from_pixel" 0)
    upload_texture device picture_transfer_buffer width height (\map_transfer_buffer->FMU.copyBytes (FP.castPtr map_transfer_buffer) (FP.castPtr pixel) (fromIntegral size))

upload_texture::FP.Ptr SDLT.SDL_GPUDevice->FP.Ptr SDLT.SDL_GPUTransferBuffer->DW.Word32->DW.Word32->(FP.Ptr ()->IO ())->IO (FP.Ptr SDLT.SDL_GPUTexture)
upload_texture device picture_transfer_buffer width height action=do
    texture<-FMU.with (SDLI.SDL_GPUTextureCreateInfo {sdl_type=SDLI.sdl_gpu_texturetype_2d,sdl_format=SDLI.sdl_gpu_textureformat_r8g8b8a8_unorm,sdl_usage=SDLI.sdl_gpu_textureusage_sampler,sdl_width=width,sdl_height=height,sdl_layer_count_or_depth=1,sdl_num_levels=1,sdl_sample_count=SDLI.sdl_gpu_samplecount_1}) (return_catch_null . SDLF.sdl_create_gpu_texture device)
    map_transfer_buffer<-SDLF.sdl_map_gpu_transfer_buffer device picture_transfer_buffer (FMU.fromBool True)
    catch_null map_transfer_buffer
    action map_transfer_buffer
    SDLF.sdl_unmap_gpu_transfer_buffer device picture_transfer_buffer
    command_buffer<-SDLF.sdl_acquire_gpu_command_buffer device
    catch_null command_buffer
    copy_pass<-SDLF.sdl_begin_gpu_copy_pass command_buffer
    catch_null copy_pass
    FMU.with (SDLI.SDL_GPUTextureTransferInfo {sdl_transfer_buffer=picture_transfer_buffer,sdl_offset=0,sdl_pixels_per_row=width,sdl_rows_per_layer=height}) $ \texture_transfer_info->FMU.with (SDLI.SDL_GPUTextureRegion {sdl_texture=texture,sdl_mip_level=0,sdl_layer=0,sdl_x=0,sdl_y=0,sdl_z=0,sdl_w=width,sdl_h=height,sdl_d=1}) $ \texture_region->SDLF.sdl_upload_to_gpu_texture copy_pass texture_transfer_info texture_region (FMU.fromBool False)
    SDLF.sdl_end_gpu_copy_pass copy_pass
    catch_false (SDLF.sdl_submit_gpu_command_buffer command_buffer)
    return texture

copy_texture::FP.Ptr SDLT.SDL_GPUDevice->FP.Ptr SDLT.SDL_GPUTexture->FP.Ptr SDLT.SDL_GPUTexture->DW.Word32->DW.Word32->DW.Word32->DW.Word32->IO ()
copy_texture device texture_from texture_to x y width height=do
    command_buffer<-SDLF.sdl_acquire_gpu_command_buffer device
    catch_null command_buffer
    copy_pass<-SDLF.sdl_begin_gpu_copy_pass command_buffer
    catch_null copy_pass
    FMU.with (SDLI.SDL_GPUTextureLocation {sdl_texture=texture_from,sdl_mip_level=0,sdl_layer=0,sdl_x=0,sdl_y=0,sdl_z=0}) $ \texture_location_from->FMU.with (SDLI.SDL_GPUTextureLocation {sdl_texture=texture_to,sdl_mip_level=0,sdl_layer=0,sdl_x=x,sdl_y=y,sdl_z=0}) $ \texture_location_to->SDLF.sdl_copy_gpu_texture_to_texture copy_pass texture_location_from texture_location_to width height 1 (FMU.fromBool False)
    SDLF.sdl_end_gpu_copy_pass copy_pass
    catch_false (SDLF.sdl_submit_gpu_command_buffer command_buffer)