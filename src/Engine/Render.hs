{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Engine.Render where

import Engine.Collector
import Engine.Container
import Engine.Projection
import Engine.Selector
import Engine.Type
import Engine.Underlying
import qualified SDL.Function as SDLF
import qualified SDL.Include as SDLI
import qualified SDL.Type as SDLT
import qualified Error.Error as EE
import qualified Control.Monad as CM
import qualified Data.Foldable as DF
import qualified Data.Sequence as DS
import qualified Data.Vector as DV
import qualified Data.Word as DW
import qualified Foreign.C.Types as FCT
import qualified Foreign.Marshal.Alloc as FMA
import qualified Foreign.Marshal.Utils as FMU
import qualified Foreign.Ptr as FP
import qualified Foreign.Storable as FS

for_render::Window->FP.Ptr SDLT.SDL_GPUCommandBuffer->(FP.Ptr SDLT.SDL_GPUTexture->IO ())->IO ()
for_render window command_buffer action=FMA.alloca $ \ptr_texture->FMA.alloca $ \width->FMA.alloca $ \height->do
    value<-SDLF.sdl_acquire_gpu_swapchain_texture command_buffer window.sdl_window ptr_texture width height
    if FMU.toBool value
        then do
            texture<-FS.peek ptr_texture
            if texture==FP.nullPtr then catch_false (SDLF.sdl_cancel_gpu_command_buffer command_buffer) else do
                action texture
                catch_false (SDLF.sdl_submit_gpu_command_buffer command_buffer)
        else catch_false (SDLF.sdl_cancel_gpu_command_buffer command_buffer)

do_render::Engine a b c d e->Window->FP.Ptr SDLT.SDL_GPUCommandBuffer->FP.Ptr SDLT.SDL_GPUTexture->Maybe Int->DS.Seq (Maybe Int,Maybe Int,DW.Word32,DW.Word32)->DS.Seq Vertex->DS.Seq DW.Word32->DS.Seq Parameter->IO ()
do_render engine window command_buffer texture maybe_sampler_id draw_call vertex index parameter=do
    value<-update_buffer engine.device command_buffer engine.vertex_buffer engine.index_buffer engine.parameter_buffer engine.transfer_buffer engine.vertex_size engine.index_size engine.parameter_size vertex index parameter
    FMU.with (SDLI.SDL_GPUColorTargetInfo {sdl_texture=texture,sdl_clear_color=SDLI.SDL_FColor {sdl_r=window.red,sdl_g=window.green,sdl_b=window.blue,sdl_a=window.alpha},sdl_load_op=SDLI.sdl_gpu_loadop_clear,sdl_store_op=SDLI.sdl_gpu_storeop_store}) $ \color_target_info->do
        render_pass<-SDLF.sdl_begin_gpu_render_pass command_buffer color_target_info 1 FP.nullPtr
        catch_null render_pass
        CM.when value (do_render_a engine window command_buffer render_pass maybe_sampler_id draw_call)
        SDLF.sdl_end_gpu_render_pass render_pass

do_render_a::Engine a b c d e->Window->FP.Ptr SDLT.SDL_GPUCommandBuffer->FP.Ptr SDLT.SDL_GPURenderPass->Maybe Int->DS.Seq (Maybe Int,Maybe Int,DW.Word32,DW.Word32)->IO ()
do_render_a engine window command_buffer render_pass maybe_sampler_id draw_call=do
    SDLF.sdl_bind_gpu_graphics_pipeline render_pass window.graphics_pipeline
    FMU.with engine.parameter_buffer (\parameter_buffer->SDLF.sdl_bind_gpu_vertex_storage_buffers render_pass 0 parameter_buffer 1)
    let size=4*FS.sizeOf (undefined::FCT.CFloat) in FMA.allocaBytesAligned size 16 $ \ptr->do
        FMU.fillBytes ptr 0 size
        FS.pokeElemOff ptr 0 window.adaptive_width
        FS.pokeElemOff ptr 1 window.adaptive_height
        FS.pokeElemOff ptr 2 engine.font_size
        FS.pokeElemOff ptr 3 engine.pixel_range
        SDLF.sdl_push_gpu_vertex_uniform_data command_buffer 0 (FP.castPtr ptr) (fromIntegral size)
    FMU.with (SDLI.SDL_GPUBufferBinding {sdl_buffer=engine.vertex_buffer,sdl_offset=0}) (\buffer_binding->SDLF.sdl_bind_gpu_vertex_buffers render_pass 0 buffer_binding 1)
    FMU.with (SDLI.SDL_GPUBufferBinding {sdl_buffer=engine.index_buffer,sdl_offset=0}) (\buffer_binding->SDLF.sdl_bind_gpu_index_buffer render_pass buffer_binding SDLI.sdl_gpu_indexelementsize_32bit)
    DF.mapM_ (do_render_b render_pass (maybe engine.default_sampler (\sampler_id->intmap_lookup sampler_id engine.sampler) maybe_sampler_id) engine) draw_call

do_render_b::FP.Ptr SDLT.SDL_GPURenderPass->FP.Ptr SDLT.SDL_GPUSampler->Engine a b c d e->(Maybe Int,Maybe Int,DW.Word32,DW.Word32)->IO ()
do_render_b render_pass sampler engine (maybe_canvas_id,maybe_album_id,index_length,index_offset)=case maybe_canvas_id of
    Just canvas_id->do
        FMU.with (SDLI.SDL_GPUTextureSamplerBinding {sdl_texture=do_render_c (intmap_lookup canvas_id engine.canvas),sdl_sampler=sampler}) (\texture_sampler_binding->SDLF.sdl_bind_gpu_fragment_samplers render_pass 0 texture_sampler_binding 1)
        SDLF.sdl_draw_gpu_indexed_primitives render_pass index_length 1 index_offset 0 0
    Nothing->case maybe_album_id of
        Nothing->do
            FMU.with (SDLI.SDL_GPUTextureSamplerBinding {sdl_texture=engine.texture,sdl_sampler=sampler}) (\texture_sampler_binding->SDLF.sdl_bind_gpu_fragment_samplers render_pass 0 texture_sampler_binding 1)
            SDLF.sdl_draw_gpu_indexed_primitives render_pass index_length 1 index_offset 0 0
        Just album_id->do
            FMU.with (SDLI.SDL_GPUTextureSamplerBinding {sdl_texture=(intmap_lookup album_id engine.album).texture,sdl_sampler=sampler}) (\texture_sampler_binding->SDLF.sdl_bind_gpu_fragment_samplers render_pass 0 texture_sampler_binding 1)
            SDLF.sdl_draw_gpu_indexed_primitives render_pass index_length 1 index_offset 0 0

do_render_c::Canvas->FP.Ptr SDLT.SDL_GPUTexture
do_render_c canvas=case canvas of
    Free_canvas {texture}->texture
    Bound_canvas {texture}->texture

do_render_canvas::Engine a b c d e->FCT.CFloat->FCT.CFloat->FP.Ptr SDLT.SDL_GPUCommandBuffer->FP.Ptr SDLT.SDL_GPUTexture->Maybe Int->DS.Seq (Maybe Int,Maybe Int,DW.Word32,DW.Word32)->DS.Seq Vertex->DS.Seq DW.Word32->DS.Seq Parameter->IO ()
do_render_canvas engine width height command_buffer texture maybe_sampler_id draw_call vertex index parameter=do
    value<-update_buffer engine.device command_buffer engine.vertex_buffer engine.index_buffer engine.parameter_buffer engine.transfer_buffer engine.vertex_size engine.index_size engine.parameter_size vertex index parameter
    FMU.with (SDLI.SDL_GPUColorTargetInfo {sdl_texture=texture,sdl_clear_color=SDLI.SDL_FColor {sdl_r=0,sdl_g=0,sdl_b=0,sdl_a=0},sdl_load_op=SDLI.sdl_gpu_loadop_clear,sdl_store_op=SDLI.sdl_gpu_storeop_store}) $ \color_target_info->do
        render_pass<-SDLF.sdl_begin_gpu_render_pass command_buffer color_target_info 1 FP.nullPtr
        catch_null render_pass
        CM.when value (do_render_canvas_a engine width height command_buffer render_pass maybe_sampler_id draw_call)
        SDLF.sdl_end_gpu_render_pass render_pass

do_render_canvas_a::Engine a b c d e->FCT.CFloat->FCT.CFloat->FP.Ptr SDLT.SDL_GPUCommandBuffer->FP.Ptr SDLT.SDL_GPURenderPass->Maybe Int->DS.Seq (Maybe Int,Maybe Int,DW.Word32,DW.Word32)->IO ()
do_render_canvas_a engine width height command_buffer render_pass maybe_sampler_id draw_call=do
    SDLF.sdl_bind_gpu_graphics_pipeline render_pass engine.canvas_graphics_pipeline
    FMU.with engine.parameter_buffer (\parameter_buffer->SDLF.sdl_bind_gpu_vertex_storage_buffers render_pass 0 parameter_buffer 1)
    let size=4*FS.sizeOf (undefined::FCT.CFloat) in FMA.allocaBytesAligned size 16 $ \ptr->do
        FMU.fillBytes ptr 0 size
        FS.pokeElemOff ptr 0 width
        FS.pokeElemOff ptr 1 height
        FS.pokeElemOff ptr 2 engine.font_size
        FS.pokeElemOff ptr 3 engine.pixel_range
        SDLF.sdl_push_gpu_vertex_uniform_data command_buffer 0 (FP.castPtr ptr) (fromIntegral size)
    FMU.with (SDLI.SDL_GPUBufferBinding {sdl_buffer=engine.vertex_buffer,sdl_offset=0}) (\buffer_binding->SDLF.sdl_bind_gpu_vertex_buffers render_pass 0 buffer_binding 1)
    FMU.with (SDLI.SDL_GPUBufferBinding {sdl_buffer=engine.index_buffer,sdl_offset=0}) (\buffer_binding->SDLF.sdl_bind_gpu_index_buffer render_pass buffer_binding SDLI.sdl_gpu_indexelementsize_32bit)
    DF.mapM_ (do_render_b render_pass (maybe engine.default_sampler (\sampler_id->intmap_lookup sampler_id engine.sampler) maybe_sampler_id) engine) draw_call

for_canvas_widget_render::Maybe Int->Projection_path->Selector ()->Widget a b c d e->Engine a b c d e->IO (Engine a b c d e)
for_canvas_widget_render maybe_sampler_id projection_path canvas_widget_render_selector widget engine=case widget of
    Collector {submit}->let (vertex,index,parameter,draw_call)=for_submit submit in for_canvas_widget_render_a projection_path canvas_widget_render_selector engine $ \half_width half_height canvas_id this_engine->do
        command_buffer<-SDLF.sdl_acquire_gpu_command_buffer this_engine.device
        catch_null command_buffer
        case intmap_lookup canvas_id this_engine.canvas of
            Bound_canvas {texture}->do
                do_render_canvas this_engine (half_width*2) (half_height*2) command_buffer texture maybe_sampler_id draw_call vertex index parameter
                catch_false (SDLF.sdl_submit_gpu_command_buffer command_buffer)
                return this_engine
            _->EE.quick_error "for_canvas_widget_render" 0
    _->EE.quick_error "for_canvas_widget_render" 1

for_canvas_widget_render_a::Projection_path->Selector ()->Engine a b c d e->(FCT.CFloat->FCT.CFloat->Int->Engine a b c d e->IO (Engine a b c d e))->IO (Engine a b c d e)
for_canvas_widget_render_a projection_path selector engine action=selector_monad_action (\_ widget this_engine->for_canvas_widget_render_b widget action this_engine) selector (lookup_projection_widget projection_path engine) engine

for_canvas_widget_render_b::Widget a b c d e->(FCT.CFloat->FCT.CFloat->Int->Engine a b c d e->IO (Engine a b c d e))->Engine a b c d e->IO (Engine a b c d e)
for_canvas_widget_render_b widget action engine=case widget of
    Visual {visual}->for_canvas_widget_render_c visual action engine
    Group_visual {collect_order,group_visual}->DF.foldlM (\this_engine index->for_canvas_widget_render_c (intmap_lookup index group_visual) action this_engine) engine collect_order
    Vector_visual {collect_order,size,vector_visual}->DF.foldlM (\this_engine index->for_canvas_widget_render_c (vector_visual DV.! catch_out 0 size index) action this_engine) engine collect_order
    _->EE.quick_error "for_canvas_widget_render_b" 0

for_canvas_widget_render_c::Visual->(FCT.CFloat->FCT.CFloat->Int->Engine a b c d e->IO (Engine a b c d e))->Engine a b c d e->IO (Engine a b c d e)
for_canvas_widget_render_c visual action engine=case visual of
    Canvas {half_width,half_height,canvas_id,locked}->if locked then EE.quick_error "for_canvas_widget_render_c" 0 else action half_width half_height canvas_id engine
    _->return engine

update_buffer::FP.Ptr SDLT.SDL_GPUDevice->FP.Ptr SDLT.SDL_GPUCommandBuffer->FP.Ptr SDLT.SDL_GPUBuffer->FP.Ptr SDLT.SDL_GPUBuffer->FP.Ptr SDLT.SDL_GPUBuffer->FP.Ptr SDLT.SDL_GPUTransferBuffer->Int->Int->Int->DS.Seq Vertex->DS.Seq DW.Word32->DS.Seq Parameter->IO Bool
update_buffer device command_buffer vertex_buffer index_buffer parameter_buffer transfer_buffer vertex_size index_size parameter_size vertex index parameter=let vertex_length=DS.length vertex in let index_length=DS.length index in let parameter_length=DS.length parameter in if vertex_length==0||index_length==0||parameter_length==0 then return False else let unit_vertex=FS.sizeOf (undefined::Vertex) in let unit_index=FS.sizeOf (undefined::DW.Word32) in let unit_parameter=FS.sizeOf (undefined::Parameter) in let new_vertex_size=vertex_length*unit_vertex in let new_index_size=index_length*unit_index in let new_parameter_size=parameter_length*unit_parameter in if vertex_size<new_vertex_size||index_size<new_index_size||parameter_size<new_parameter_size then EE.quick_error "update_buffer" 0 else do
    map_transfer_buffer<-SDLF.sdl_map_gpu_transfer_buffer device transfer_buffer (FMU.fromBool True)
    catch_null map_transfer_buffer
    seq_poke_array unit_vertex vertex (FP.castPtr map_transfer_buffer)
    seq_poke_array unit_index index (FP.plusPtr map_transfer_buffer vertex_size)
    let size=vertex_size+index_size
    seq_poke_array unit_parameter parameter (FP.plusPtr map_transfer_buffer size)
    SDLF.sdl_unmap_gpu_transfer_buffer device transfer_buffer
    copy_pass<-SDLF.sdl_begin_gpu_copy_pass command_buffer
    catch_null copy_pass
    FMU.with (SDLI.SDL_GPUTransferBufferLocation {sdl_transfer_buffer=transfer_buffer,sdl_offset=0}) (\transfer_buffer_location->FMU.with (SDLI.SDL_GPUBufferRegion {sdl_buffer=vertex_buffer,sdl_offset=0,sdl_size=fromIntegral new_vertex_size}) (\buffer_region->SDLF.sdl_upload_to_gpu_buffer copy_pass transfer_buffer_location buffer_region (FMU.fromBool True)))
    FMU.with (SDLI.SDL_GPUTransferBufferLocation {sdl_transfer_buffer=transfer_buffer,sdl_offset=fromIntegral vertex_size}) (\transfer_buffer_location->FMU.with (SDLI.SDL_GPUBufferRegion {sdl_buffer=index_buffer,sdl_offset=0,sdl_size=fromIntegral new_index_size}) (\buffer_region->SDLF.sdl_upload_to_gpu_buffer copy_pass transfer_buffer_location buffer_region (FMU.fromBool True)))
    FMU.with (SDLI.SDL_GPUTransferBufferLocation {sdl_transfer_buffer=transfer_buffer,sdl_offset=fromIntegral size}) (\transfer_buffer_location->FMU.with (SDLI.SDL_GPUBufferRegion {sdl_buffer=parameter_buffer,sdl_offset=0,sdl_size=fromIntegral new_parameter_size}) (\buffer_region->SDLF.sdl_upload_to_gpu_buffer copy_pass transfer_buffer_location buffer_region (FMU.fromBool True)))
    SDLF.sdl_end_gpu_copy_pass copy_pass
    return True