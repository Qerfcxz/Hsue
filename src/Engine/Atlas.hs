{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Engine.Atlas where

import Engine.Other
import Engine.Type
import SDL.Constant as C
import SDL.Function as F
import SDL.Type as T
import qualified Control.Monad as CM
import qualified Data.IntMap as DIM
import qualified Data.Word as DW
import qualified Foreign.C.Types as FCT
import qualified Foreign.Marshal.Utils as FMU
import qualified Foreign.Ptr as FP

init_atlas::FCT.CInt->FCT.CInt->Int->Atlas
init_atlas width height next=Atlas {next=next,width=fromIntegral width,height=fromIntegral height,pack=Leaf_pack {rectangle=Rectangle {left=0,down=0,right=width,up=height},used=False},region=DIM.empty}

atlas_insert::FCT.CInt->FCT.CInt->FCT.CInt->Atlas->(Atlas,Int,FCT.CInt,FCT.CInt,FCT.CInt,FCT.CInt,FCT.CFloat,FCT.CFloat)
atlas_insert width height padding atlas=case atlas_insert_a (width+2*padding) (height+2*padding) atlas.pack of
    Nothing->error "atlas_insert: error 1"
    Just (pack,left,down,right,up)->let new_left=left+padding in let new_down=down+padding in let new_right=right-padding in let new_up=up-padding in let min_u=fromIntegral new_left/atlas.width in let min_v=fromIntegral new_down/atlas.height in let max_u=fromIntegral new_right/atlas.width in let max_v=fromIntegral new_up/atlas.height in (atlas {next=atlas.next+1,pack=pack,region=DIM.insert atlas.next (Region {min_u=min_u,min_v=min_v,max_u=max_u,max_v=max_v}) atlas.region},atlas.next,new_left,new_down,new_right,new_up,(min_u+max_u)/2,(min_v+max_v)/2)

atlas_insert_a::FCT.CInt->FCT.CInt->Pack->Maybe (Pack,FCT.CInt,FCT.CInt,FCT.CInt,FCT.CInt)
atlas_insert_a width height pack=case pack of
    Leaf_pack {rectangle,used}->case rectangle of
        Rectangle {left,down,right,up}->let new_right=rectangle.left+width in let new_up=rectangle.down+height in let new_width=rectangle.right-new_right in let new_height=rectangle.up-new_up in if used||new_width<0||new_height<0 then Nothing else if new_width==0&&new_height==0 then Just (Leaf_pack {rectangle=rectangle,used=True},left,down,right,up) else let (unused_rectangle,left_rectangle,right_rectangle)=if new_width<new_height then (Rectangle {left=new_right,down=rectangle.down,right=rectangle.right,up=new_up},Rectangle {left=rectangle.left,down=rectangle.down,right=rectangle.right,up=new_up},Rectangle {left=rectangle.left,down=new_up,right=rectangle.right,up=rectangle.up}) else (Rectangle {left=rectangle.left,down=new_up,right=new_right,up=rectangle.up},Rectangle {left=rectangle.left,down=rectangle.down,right=new_right,up=rectangle.up},Rectangle {left=new_right,down=rectangle.down,right=rectangle.right,up=rectangle.up}) in Just (Node_pack {rectangle=rectangle,left_pack=Node_pack {rectangle=left_rectangle,left_pack=Leaf_pack {rectangle=Rectangle {left=rectangle.left,down=rectangle.down,right=new_right,up=new_up},used=True},right_pack=Leaf_pack {rectangle=unused_rectangle,used=False}},right_pack=Leaf_pack {rectangle=right_rectangle,used=False}},rectangle.left,rectangle.down,new_right,new_up)
    Node_pack {rectangle,left_pack,right_pack}->case atlas_insert_a width height left_pack of
        Nothing->case atlas_insert_a width height right_pack of
            Nothing->Nothing
            Just (new_right_pack,left,down,right,up)->Just (Node_pack {rectangle=rectangle,left_pack=left_pack,right_pack=new_right_pack},left,down,right,up)
        Just (new_left_pack,left,down,right,up)->Just (Node_pack {rectangle=rectangle,left_pack=new_left_pack,right_pack=right_pack},left,down,right,up)

upload_picture::FP.Ptr T.SDL_GPUDevice->FP.Ptr T.SDL_GPUTexture->FP.Ptr SDL_GPUTransferBuffer->Int->FP.Ptr DW.Word8->FCT.CInt->FCT.CInt->FCT.CInt->Atlas->IO (Atlas,Int,FCT.CFloat,FCT.CFloat)
upload_picture device texture picture_transfer_buffer picture_size pixel width height padding atlas=let new_width=fromIntegral width in let new_height=fromIntegral height in let (new_atlas,index,left,down,_,_,u,v)=atlas_insert width height padding atlas in do
    map_transfer_buffer<-F.sdl_mapgputransferbuffer device picture_transfer_buffer (FMU.fromBool True)
    catch_null map_transfer_buffer
    let size=fromIntegral (4*width*height)
    CM.when (picture_size<size) (error "upload_picture: error 1")
    FMU.copyBytes (FP.castPtr map_transfer_buffer) pixel size
    F.sdl_unmapgputransferbuffer device picture_transfer_buffer
    command_buffer<-F.sdl_acquiregpucommandbuffer device
    catch_null command_buffer
    copy_pass<-F.sdl_begingpucopypass command_buffer
    catch_null copy_pass
    FMU.with (C.SDL_GPUTextureTransferInfo {transfer_buffer=picture_transfer_buffer,offset=0,pixels_per_row=new_width,rows_per_layer=new_height}) $ \texture_transfer_info->FMU.with (C.SDL_GPUTextureRegion {texture=texture,mip_level=0,layer=0,x=fromIntegral left,y=fromIntegral down,z=0,w=new_width,h=new_height,d=1}) $ \texture_region->F.sdl_uploadtogputexture copy_pass texture_transfer_info texture_region (FMU.fromBool False)
    F.sdl_endgpucopypass copy_pass
    catch_false (F.sdl_submitgpucommandbuffer command_buffer)
    return (new_atlas,index,u,v)