{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Engine.Render where

import Engine.Container
import Engine.Projection
import Engine.Selector
import Engine.Type
import Engine.Underlying
import qualified SDL.Function as SDLF
import qualified SDL.Include as SDLI
import qualified SDL.Type as SDLT
import qualified Error.Function as EF
import qualified Error.Type as ET
import qualified Control.Monad as CM
import qualified Data.Foldable as DF
import qualified Data.IntMap as DIM
import qualified Data.Sequence as DS
import qualified Data.Word as DW
import qualified Foreign.C.Types as FCT
import qualified Foreign.Marshal.Alloc as FMA
import qualified Foreign.Marshal.Utils as FMU
import qualified Foreign.Ptr as FP
import qualified Foreign.Storable as FS

do_render_canvas::ET.Has_call_stack=>Custom a=>Engine a->FCT.CFloat->FCT.CFloat->FP.Ptr SDLT.SDL_GPUTexture->FP.Ptr SDLT.SDL_GPUCommandBuffer->Maybe Int->DIM.IntMap (DS.Seq (Submit a))->IO ()
do_render_canvas engine width height texture command_buffer maybe_sampler_id submit=do
    draw_call<-write_submit engine command_buffer submit
    FMU.with (SDLI.SDL_GPUColorTargetInfo {sdl_texture=texture,sdl_clear_color=SDLI.SDL_FColor {sdl_r=0,sdl_g=0,sdl_b=0,sdl_a=0},sdl_load_op=SDLI.sdl_gpu_loadop_clear,sdl_store_op=SDLI.sdl_gpu_storeop_store}) $ \color_target_info->do
        render_pass<-SDLF.sdl_begin_gpu_render_pass command_buffer color_target_info 1 FP.nullPtr
        sdl_catch_null render_pass
        CM.unless (DS.null draw_call) (do_render_a engine width height engine.canvas_graphics_pipeline command_buffer (maybe engine.default_sampler (\sampler_id->int_map_lookup sampler_id engine.sampler) maybe_sampler_id) render_pass draw_call)
        SDLF.sdl_end_gpu_render_pass render_pass

do_render::ET.Has_call_stack=>Custom a=>Engine a->Window->FP.Ptr SDLT.SDL_GPUCommandBuffer->Maybe Int->DIM.IntMap (DS.Seq (Submit a))->IO ()
do_render engine window command_buffer maybe_sampler_id submit=FMA.alloca $ \ptr_texture->FMA.alloca $ \width->FMA.alloca $ \height->do
    value<-SDLF.sdl_acquire_gpu_swapchain_texture command_buffer window.sdl_window ptr_texture width height
    if FMU.toBool value
        then do
            texture<-FS.peek ptr_texture
            if texture==FP.nullPtr then sdl_catch_false (SDLF.sdl_cancel_gpu_command_buffer command_buffer) else do
                draw_call<-write_submit engine command_buffer submit
                case window.color of
                    Color {red,green,blue,alpha}->FMU.with (SDLI.SDL_GPUColorTargetInfo {sdl_texture=texture,sdl_clear_color=SDLI.SDL_FColor {sdl_r=red,sdl_g=green,sdl_b=blue,sdl_a=alpha},sdl_load_op=SDLI.sdl_gpu_loadop_clear,sdl_store_op=SDLI.sdl_gpu_storeop_store}) $ \color_target_info->do
                        render_pass<-SDLF.sdl_begin_gpu_render_pass command_buffer color_target_info 1 FP.nullPtr
                        sdl_catch_null render_pass
                        CM.unless (DS.null draw_call) (do_render_a engine window.adaptive_width window.adaptive_height window.graphics_pipeline command_buffer (maybe engine.default_sampler (\sampler_id->int_map_lookup sampler_id engine.sampler) maybe_sampler_id) render_pass draw_call)
                        SDLF.sdl_end_gpu_render_pass render_pass
                sdl_catch_false (SDLF.sdl_submit_gpu_command_buffer command_buffer)
        else sdl_catch_false (SDLF.sdl_cancel_gpu_command_buffer command_buffer)

do_render_a::ET.Has_call_stack=>Engine a->FCT.CFloat->FCT.CFloat->FP.Ptr SDLT.SDL_GPUGraphicsPipeline->FP.Ptr SDLT.SDL_GPUCommandBuffer->FP.Ptr SDLT.SDL_GPUSampler->FP.Ptr SDLT.SDL_GPURenderPass->DS.Seq (Submit_mode,DW.Word32,DW.Word32)->IO ()
do_render_a engine width height pipeline command_buffer sampler render_pass draw_call=do
    SDLF.sdl_bind_gpu_graphics_pipeline render_pass pipeline
    FMU.with engine.parameter_buffer (\parameter_buffer->SDLF.sdl_bind_gpu_vertex_storage_buffers render_pass 0 parameter_buffer 1)
    FMU.with (SDLI.SDL_GPUBufferBinding {sdl_buffer=engine.vertex_buffer,sdl_offset=0}) (\buffer_binding->SDLF.sdl_bind_gpu_vertex_buffers render_pass 0 buffer_binding 1)
    FMU.with (SDLI.SDL_GPUBufferBinding {sdl_buffer=engine.index_buffer,sdl_offset=0}) (\buffer_binding->SDLF.sdl_bind_gpu_index_buffer render_pass buffer_binding SDLI.sdl_gpu_indexelementsize_32bit)
    FMA.allocaBytesAligned 16 16 $ \ptr->do
        FMU.fillBytes ptr 0 16
        FS.pokeByteOff ptr 0 width
        FS.pokeByteOff ptr 4 height
        DF.mapM_ (\(submit_mode,index_size,index_index)->do_render_b engine command_buffer sampler render_pass submit_mode index_size index_index ptr) draw_call

do_render_b::ET.Has_call_stack=>Engine a->FP.Ptr SDLT.SDL_GPUCommandBuffer->FP.Ptr SDLT.SDL_GPUSampler->FP.Ptr SDLT.SDL_GPURenderPass->Submit_mode->DW.Word32->DW.Word32->FP.Ptr FCT.CFloat->IO ()
do_render_b engine command_buffer sampler render_pass submit_mode index_size index_index ptr=do
    case submit_mode of
        Submit_default->do
            FS.pokeByteOff ptr 8 engine.font_size
            FS.pokeByteOff ptr 12 engine.pixel_range
            SDLF.sdl_push_gpu_vertex_uniform_data command_buffer 0 (FP.castPtr ptr) 16
            FMU.with (SDLI.SDL_GPUTextureSamplerBinding {sdl_texture=engine.texture,sdl_sampler=sampler}) (\texture_sampler_binding->SDLF.sdl_bind_gpu_fragment_samplers render_pass 0 texture_sampler_binding 1)
        Submit_canvas {canvas_id}->do
            FS.pokeByteOff ptr 8 engine.font_size
            FS.pokeByteOff ptr 12 engine.pixel_range
            SDLF.sdl_push_gpu_vertex_uniform_data command_buffer 0 (FP.castPtr ptr) 16
            FMU.with (SDLI.SDL_GPUTextureSamplerBinding {sdl_texture=do_render_c (int_map_lookup canvas_id engine.canvas),sdl_sampler=sampler}) (\texture_sampler_binding->SDLF.sdl_bind_gpu_fragment_samplers render_pass 0 texture_sampler_binding 1)
        Submit_album {album_id}->do
            FS.pokeByteOff ptr 8 engine.font_size
            FS.pokeByteOff ptr 12 engine.pixel_range
            SDLF.sdl_push_gpu_vertex_uniform_data command_buffer 0 (FP.castPtr ptr) 16
            FMU.with (SDLI.SDL_GPUTextureSamplerBinding {sdl_texture=(int_map_lookup album_id engine.album).texture,sdl_sampler=sampler}) (\texture_sampler_binding->SDLF.sdl_bind_gpu_fragment_samplers render_pass 0 texture_sampler_binding 1)
        Submit_atlas_font {atlas_font_id}->case int_map_lookup atlas_font_id engine.atlas_font of
            Atlas_font {texture,font_size,pixel_range}->do
                FS.pokeByteOff ptr 8 font_size
                FS.pokeByteOff ptr 12 pixel_range
                SDLF.sdl_push_gpu_vertex_uniform_data command_buffer 0 (FP.castPtr ptr) 16
                FMU.with (SDLI.SDL_GPUTextureSamplerBinding {sdl_texture=texture,sdl_sampler=sampler}) (\texture_sampler_binding->SDLF.sdl_bind_gpu_fragment_samplers render_pass 0 texture_sampler_binding 1)
    SDLF.sdl_draw_gpu_indexed_primitives render_pass index_size 1 index_index 0 0

do_render_c::ET.Has_call_stack=>Canvas->FP.Ptr SDLT.SDL_GPUTexture
do_render_c canvas=case canvas of
    Free_canvas {texture}->texture
    Bound_canvas {texture}->texture

do_canvas_widget_render::ET.Has_call_stack=>Custom b=>Maybe Int->Projection_path->Selector a->Widget b->Engine b->IO (Engine b)
do_canvas_widget_render maybe_sampler_id projection_path selector widget engine=case widget of
    Collector {submit}->selector_monad_action (const (\this_widget this_engine->any_visual_selector_monad_action True (const (\visual->do_canvas_widget_render_a submit maybe_sampler_id visual)) this_widget this_engine)) selector (lookup_projection_widget projection_path engine) engine
    _->EF.empty_error

do_canvas_widget_render_a::ET.Has_call_stack=>Custom a=>DIM.IntMap (DS.Seq (Submit a))->Maybe Int->Visual a->Engine a->IO (Engine a)
do_canvas_widget_render_a submit maybe_sampler_id visual engine=do
    command_buffer<-SDLF.sdl_acquire_gpu_command_buffer engine.device
    sdl_catch_null command_buffer
    case visual of
        Canvas {half_width,half_height,canvas_id}->case int_map_lookup canvas_id engine.canvas of
            Bound_canvas {texture}->do
                do_render_canvas engine (half_width*2) (half_height*2) texture command_buffer maybe_sampler_id submit
                sdl_catch_false (SDLF.sdl_submit_gpu_command_buffer command_buffer)
                return engine
            _->EF.empty_error
        _->EF.empty_error

get_submit_size::ET.Has_call_stack=>DIM.IntMap (DS.Seq (Submit a))->(DW.Word32,DW.Word32,DW.Word32)
get_submit_size=DIM.foldl' (\(vertex_number,index_number,parameter_number) submit->DF.foldl' (flip get_submit_size_a) (vertex_number,index_number,parameter_number) submit) (0,0,0)

get_submit_size_a::ET.Has_call_stack=>Submit a->(DW.Word32,DW.Word32,DW.Word32)->(DW.Word32,DW.Word32,DW.Word32)
get_submit_size_a submit (vertex_number,index_number,parameter_number)=case submit of
    Submit {vertex_size,index_size}->(vertex_number+vertex_size,index_number+index_size,parameter_number+1)

write_submit::ET.Has_call_stack=>Custom a=>Engine a->FP.Ptr SDLT.SDL_GPUCommandBuffer->DIM.IntMap (DS.Seq (Submit a))->IO (DS.Seq (Submit_mode,DW.Word32,DW.Word32))
write_submit engine command_buffer submit=let (vertex_number,index_number,parameter_number)=get_submit_size submit in if parameter_number==0 then return DS.empty else let vertex_size=vertex_number*size_of_vertex in let index_size=index_number*size_of_index in let parameter_size=parameter_number*size_of_parameter in do
    CM.when (engine.max_vertex_size<vertex_size||engine.max_index_size<index_size||engine.max_parameter_size<parameter_size) EF.empty_error
    map_transfer_buffer<-SDLF.sdl_map_gpu_transfer_buffer engine.device engine.transfer_buffer (FMU.fromBool True)
    sdl_catch_null map_transfer_buffer
    (_,_,_,draw_call)<-CM.foldM (DF.foldlM (flip (write_submit_a (FP.castPtr map_transfer_buffer) (FP.castPtr (FP.plusPtr map_transfer_buffer (fromIntegral engine.max_vertex_size))) (FP.castPtr (FP.plusPtr map_transfer_buffer (fromIntegral (engine.max_vertex_size+engine.max_index_size))))))) (0,0,0,DS.empty) submit
    SDLF.sdl_unmap_gpu_transfer_buffer engine.device engine.transfer_buffer
    copy_pass<-SDLF.sdl_begin_gpu_copy_pass command_buffer
    sdl_catch_null copy_pass
    FMU.with (SDLI.SDL_GPUTransferBufferLocation {sdl_transfer_buffer=engine.transfer_buffer,sdl_offset=0}) (\transfer_buffer_location->FMU.with (SDLI.SDL_GPUBufferRegion {sdl_buffer=engine.vertex_buffer,sdl_offset=0,sdl_size=vertex_size}) (\buffer_region->SDLF.sdl_upload_to_gpu_buffer copy_pass transfer_buffer_location buffer_region (FMU.fromBool True)))
    FMU.with (SDLI.SDL_GPUTransferBufferLocation {sdl_transfer_buffer=engine.transfer_buffer,sdl_offset=engine.max_vertex_size}) (\transfer_buffer_location->FMU.with (SDLI.SDL_GPUBufferRegion {sdl_buffer=engine.index_buffer,sdl_offset=0,sdl_size=index_size}) (\buffer_region->SDLF.sdl_upload_to_gpu_buffer copy_pass transfer_buffer_location buffer_region (FMU.fromBool True)))
    FMU.with (SDLI.SDL_GPUTransferBufferLocation {sdl_transfer_buffer=engine.transfer_buffer,sdl_offset=engine.max_vertex_size+engine.max_index_size}) (\transfer_buffer_location->FMU.with (SDLI.SDL_GPUBufferRegion {sdl_buffer=engine.parameter_buffer,sdl_offset=0,sdl_size=parameter_size}) (\buffer_region->SDLF.sdl_upload_to_gpu_buffer copy_pass transfer_buffer_location buffer_region (FMU.fromBool True)))
    SDLF.sdl_end_gpu_copy_pass copy_pass
    return draw_call

write_submit_a::ET.Has_call_stack=>Custom a=>FP.Ptr Vertex->FP.Ptr DW.Word32->FP.Ptr Parameter->Submit a->(DW.Word32,DW.Word32,DW.Word32,DS.Seq (Submit_mode,DW.Word32,DW.Word32))->IO (DW.Word32,DW.Word32,DW.Word32,DS.Seq (Submit_mode,DW.Word32,DW.Word32))
write_submit_a vertex_ptr index_ptr parameter_ptr submit (vertex_index,index_index,parameter_index,draw_call)=case submit of
    Submit {submit_mode,submit_data,parameter,vertex_size,index_size}->do
        FS.pokeByteOff parameter_ptr (fromIntegral parameter_index*size_of_parameter) parameter
        write_submit_data (FP.plusPtr vertex_ptr (fromIntegral vertex_index*size_of_vertex)) (FP.plusPtr index_ptr (fromIntegral index_index*size_of_index)) vertex_index parameter_index submit_data
        return (vertex_index+vertex_size,index_index+index_size,parameter_index+1,write_submit_b submit_mode index_size index_index draw_call)

write_submit_b::ET.Has_call_stack=>Submit_mode->DW.Word32->DW.Word32->DS.Seq (Submit_mode,DW.Word32,DW.Word32)->DS.Seq (Submit_mode,DW.Word32,DW.Word32)
write_submit_b submit_mode index_size index_index draw_call=case draw_call of
    DS.Empty->DS.singleton (submit_mode,index_size,index_index)
    other_draw_call DS.:|> (new_submit_mode,new_index_size,new_index_index)->if submit_mode==new_submit_mode then other_draw_call DS.|> (submit_mode,index_size+new_index_size,new_index_index) else draw_call DS.|> (submit_mode,index_size,index_index)

write_submit_data::ET.Has_call_stack=>Custom a=>FP.Ptr Vertex->FP.Ptr DW.Word32->DW.Word32->DW.Word32->Submit_data a->IO ()
write_submit_data vertex_ptr index_ptr vertex_index parameter_index submit_data=case submit_data of
    Submit_rectangle {red,green,blue,alpha,min_u,min_v,max_u,max_v,left,down,right,up}->write_submit_rectangle vertex_ptr index_ptr vertex_index parameter_index red green blue alpha min_u min_v max_u max_v left down right up
    Submit_triangle {red,green,blue,alpha,u,v,first_x,first_y,second_x,second_y,third_x,third_y}->write_submit_triangle vertex_ptr index_ptr vertex_index parameter_index red green blue alpha u v first_x first_y second_x second_y third_x third_y
    Submit_convex_polygon {red,green,blue,alpha,u,v,x,y,point_set}->write_submit_convex_polygon vertex_ptr index_ptr vertex_index parameter_index red green blue alpha u v x y point_set
    Submit_regular_polygon {red,green,blue,alpha,u,v,x,y,angle,radius,number}->write_submit_regular_polygon vertex_ptr index_ptr vertex_index parameter_index red green blue alpha u v x y angle radius number
    Submit_text {red,green,blue,alpha,x,y,current_y,ratio,article}->write_submit_text vertex_ptr index_ptr vertex_index parameter_index red green blue alpha x y current_y ratio article
    Custom_submit_data {custom}->custom_submit_data vertex_ptr index_ptr vertex_index parameter_index custom

write_submit_rectangle::ET.Has_call_stack=>FP.Ptr Vertex->FP.Ptr DW.Word32->DW.Word32->DW.Word32->FCT.CFloat->FCT.CFloat->FCT.CFloat->FCT.CFloat->FCT.CFloat->FCT.CFloat->FCT.CFloat->FCT.CFloat->FCT.CFloat->FCT.CFloat->FCT.CFloat->FCT.CFloat->IO ()
write_submit_rectangle vertex_ptr index_ptr vertex_index parameter_index red green blue alpha min_u min_v max_u max_v left down right up=do
    poke_vertex vertex_ptr 0 parameter_index 0 left down min_u max_v red green blue alpha
    poke_vertex vertex_ptr size_of_vertex parameter_index 0 right down max_u max_v red green blue alpha
    poke_vertex vertex_ptr (2*size_of_vertex) parameter_index 0 right up max_u min_v red green blue alpha
    poke_vertex vertex_ptr (3*size_of_vertex) parameter_index 0 left up min_u min_v red green blue alpha
    FS.pokeByteOff index_ptr 0 vertex_index
    FS.pokeByteOff index_ptr size_of_index (vertex_index+1)
    FS.pokeByteOff index_ptr (2*size_of_index) (vertex_index+2)
    FS.pokeByteOff index_ptr (3*size_of_index) vertex_index
    FS.pokeByteOff index_ptr (4*size_of_index) (vertex_index+2)
    FS.pokeByteOff index_ptr (5*size_of_index) (vertex_index+3)

write_submit_triangle::ET.Has_call_stack=>FP.Ptr Vertex->FP.Ptr DW.Word32->DW.Word32->DW.Word32->FCT.CFloat->FCT.CFloat->FCT.CFloat->FCT.CFloat->FCT.CFloat->FCT.CFloat->FCT.CFloat->FCT.CFloat->FCT.CFloat->FCT.CFloat->FCT.CFloat->FCT.CFloat->IO ()
write_submit_triangle vertex_ptr index_ptr vertex_index parameter_index red green blue alpha u v first_x first_y second_x second_y third_x third_y=do
    poke_vertex vertex_ptr 0 parameter_index 0 first_x first_y u v red green blue alpha
    poke_vertex vertex_ptr size_of_vertex parameter_index 0 second_x second_y u v red green blue alpha
    poke_vertex vertex_ptr (2*size_of_vertex) parameter_index 0 third_x third_y u v red green blue alpha
    FS.pokeByteOff index_ptr 0 vertex_index
    FS.pokeByteOff index_ptr size_of_index (vertex_index+1)
    FS.pokeByteOff index_ptr (2*size_of_index) (vertex_index+2)

write_submit_convex_polygon::ET.Has_call_stack=>FP.Ptr Vertex->FP.Ptr DW.Word32->DW.Word32->DW.Word32->FCT.CFloat->FCT.CFloat->FCT.CFloat->FCT.CFloat->FCT.CFloat->FCT.CFloat->FCT.CFloat->FCT.CFloat->DS.Seq Point->IO ()
write_submit_convex_polygon vertex_ptr index_ptr vertex_index parameter_index red green blue alpha u v x y point_set=do
    CM.void (DF.foldlM (\index point->integral_action (\this_index->poke_vertex vertex_ptr (this_index*size_of_vertex) parameter_index 0 (x+point.x) (y+point.y) u v red green blue alpha) index) 0 point_set)
    monad_for 0 (DS.length point_set-3) $ \index->let offset=3*index*size_of_index in let new_vertex_index=vertex_index+fromIntegral index in do
        FS.pokeByteOff index_ptr offset vertex_index
        FS.pokeByteOff index_ptr (offset+size_of_index) (new_vertex_index+1)
        FS.pokeByteOff index_ptr (offset+2*size_of_index) (new_vertex_index+2)

write_submit_regular_polygon::ET.Has_call_stack=>FP.Ptr Vertex->FP.Ptr DW.Word32->DW.Word32->DW.Word32->FCT.CFloat->FCT.CFloat->FCT.CFloat->FCT.CFloat->FCT.CFloat->FCT.CFloat->FCT.CFloat->FCT.CFloat->FCT.CFloat->FCT.CFloat->Int->IO ()
write_submit_regular_polygon vertex_ptr index_ptr vertex_index parameter_index red green blue alpha u v x y angle radius number=let new_angle=2*pi/fromIntegral number in do
    monad_for 0 (number-1) (\index->let direction=angle+fromIntegral index*new_angle in poke_vertex vertex_ptr (index*size_of_vertex) parameter_index 0 (x+radius*cos direction) (y+radius*sin direction) u v red green blue alpha)
    monad_for 0 (number-3) $ \index->let offset=3*index*size_of_index in let new_vertex_index=vertex_index+fromIntegral index in do
        FS.pokeByteOff index_ptr offset vertex_index
        FS.pokeByteOff index_ptr (offset+size_of_index) (new_vertex_index+1)
        FS.pokeByteOff index_ptr (offset+2*size_of_index) (new_vertex_index+2)

write_submit_text::ET.Has_call_stack=>FP.Ptr Vertex->FP.Ptr DW.Word32->DW.Word32->DW.Word32->FCT.CFloat->FCT.CFloat->FCT.CFloat->FCT.CFloat->FCT.CFloat->FCT.CFloat->FCT.CFloat->FCT.CFloat->DS.Seq (DS.Seq Row)->IO ()
write_submit_text vertex_ptr index_ptr vertex_index parameter_index red green blue alpha x y current_y ratio article=CM.void (DF.foldlM (DF.foldlM (\(character_vertex_index,character_index_index) row->write_submit_row vertex_ptr index_ptr vertex_index parameter_index red green blue alpha x y current_y ratio row character_vertex_index character_index_index)) (0,0) article)

write_submit_row::ET.Has_call_stack=>FP.Ptr Vertex->FP.Ptr DW.Word32->DW.Word32->DW.Word32->FCT.CFloat->FCT.CFloat->FCT.CFloat->FCT.CFloat->FCT.CFloat->FCT.CFloat->FCT.CFloat->FCT.CFloat->Row->Int->Int->IO (Int,Int)
write_submit_row vertex_ptr index_ptr vertex_index parameter_index red green blue alpha this_x this_y current_y ratio row character_vertex_index character_index_index=case row of
    Row {row_core,x,y,width}->DF.foldlM (write_submit_character vertex_ptr index_ptr vertex_index parameter_index red green blue alpha (this_x+x-ratio*width/2) (this_y+current_y-y)) (character_vertex_index,character_index_index) row_core

write_submit_character::ET.Has_call_stack=>FP.Ptr Vertex->FP.Ptr DW.Word32->DW.Word32->DW.Word32->FCT.CFloat->FCT.CFloat->FCT.CFloat->FCT.CFloat->FCT.CFloat->FCT.CFloat->(Int,Int)->Character->IO (Int,Int)
write_submit_character vertex_ptr index_ptr vertex_index parameter_index this_red this_green this_blue this_alpha x y (character_vertex_index,character_index_index) character=case character of
    Character {font_size,left,down,right,up,min_u,min_v,max_u,max_v,color}->case color of
        Color {red,green,blue,alpha}->let new_red=this_red*red in let new_green=this_green*green in let new_blue=this_blue*blue in let new_alpha=this_alpha*alpha in let new_left=x+left in let new_down=y+down in let new_right=x+right in let new_up=y+up in let vertex_offset=character_vertex_index*size_of_vertex in let index_offset=character_index_index*size_of_index in let new_vertex_index=vertex_index+fromIntegral character_vertex_index in do
            poke_vertex vertex_ptr vertex_offset parameter_index font_size new_left new_down min_u min_v new_red new_green new_blue new_alpha
            poke_vertex vertex_ptr (vertex_offset+size_of_vertex) parameter_index font_size new_right new_down max_u min_v new_red new_green new_blue new_alpha
            poke_vertex vertex_ptr (vertex_offset+2*size_of_vertex) parameter_index font_size new_right new_up max_u max_v new_red new_green new_blue new_alpha
            poke_vertex vertex_ptr (vertex_offset+3*size_of_vertex) parameter_index font_size new_left new_up min_u max_v new_red new_green new_blue new_alpha
            FS.pokeByteOff index_ptr index_offset new_vertex_index
            FS.pokeByteOff index_ptr (index_offset+size_of_index) (new_vertex_index+1)
            FS.pokeByteOff index_ptr (index_offset+2*size_of_index) (new_vertex_index+2)
            FS.pokeByteOff index_ptr (index_offset+3*size_of_index) new_vertex_index
            FS.pokeByteOff index_ptr (index_offset+4*size_of_index) (new_vertex_index+2)
            FS.pokeByteOff index_ptr (index_offset+5*size_of_index) (new_vertex_index+3)
            return (character_vertex_index+4,character_index_index+6)

poke_vertex::ET.Has_call_stack=>FP.Ptr Vertex->Int->DW.Word32->FCT.CFloat->FCT.CFloat->FCT.CFloat->FCT.CFloat->FCT.CFloat->FCT.CFloat->FCT.CFloat->FCT.CFloat->FCT.CFloat->IO ()
poke_vertex ptr offset parameter_id font_size x y u v red green blue alpha=do
    FS.pokeByteOff ptr offset parameter_id
    FS.pokeByteOff ptr (offset+4) font_size
    FS.pokeByteOff ptr (offset+8) x
    FS.pokeByteOff ptr (offset+12) y
    FS.pokeByteOff ptr (offset+16) u
    FS.pokeByteOff ptr (offset+20) v
    FS.pokeByteOff ptr (offset+24) red
    FS.pokeByteOff ptr (offset+28) green
    FS.pokeByteOff ptr (offset+32) blue
    FS.pokeByteOff ptr (offset+36) alpha

{-# INLINE do_render_c #-}
{-# INLINE get_submit_size #-}
{-# INLINE get_submit_size_a #-}
{-# INLINE write_submit_b #-}
{-# INLINE write_submit_data #-}
{-# INLINE write_submit_rectangle #-}
{-# INLINE write_submit_triangle #-}
{-# INLINE poke_vertex #-}