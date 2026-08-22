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

do_render::Engine a b c d e->Window->FP.Ptr SDLT.SDL_GPUCommandBuffer->FP.Ptr SDLT.SDL_GPUTexture->Maybe Int->DS.Seq (Submit_mode,DW.Word32,DW.Word32)->DS.Seq Vertex->DS.Seq DW.Word32->DS.Seq Parameter->IO ()
do_render engine window command_buffer texture maybe_sampler_id draw_call vertex index parameter=do
    value<-update_buffer engine.device command_buffer engine.vertex_buffer engine.index_buffer engine.parameter_buffer engine.transfer_buffer engine.max_vertex_size engine.max_index_size engine.max_parameter_size vertex index parameter
    case window.color of
        Color {red,green,blue,alpha}->FMU.with (SDLI.SDL_GPUColorTargetInfo {sdl_texture=texture,sdl_clear_color=SDLI.SDL_FColor {sdl_r=red,sdl_g=green,sdl_b=blue,sdl_a=alpha},sdl_load_op=SDLI.sdl_gpu_loadop_clear,sdl_store_op=SDLI.sdl_gpu_storeop_store}) $ \color_target_info->do
            render_pass<-SDLF.sdl_begin_gpu_render_pass command_buffer color_target_info 1 FP.nullPtr
            catch_null render_pass
            CM.when value (do_render_a engine window command_buffer render_pass maybe_sampler_id draw_call)
            SDLF.sdl_end_gpu_render_pass render_pass

do_render_a::Engine a b c d e->Window->FP.Ptr SDLT.SDL_GPUCommandBuffer->FP.Ptr SDLT.SDL_GPURenderPass->Maybe Int->DS.Seq (Submit_mode,DW.Word32,DW.Word32)->IO ()
do_render_a engine window command_buffer render_pass maybe_sampler_id draw_call=do
    SDLF.sdl_bind_gpu_graphics_pipeline render_pass window.graphics_pipeline
    FMU.with engine.parameter_buffer (\parameter_buffer->SDLF.sdl_bind_gpu_vertex_storage_buffers render_pass 0 parameter_buffer 1)
    FMU.with (SDLI.SDL_GPUBufferBinding {sdl_buffer=engine.vertex_buffer,sdl_offset=0}) (\buffer_binding->SDLF.sdl_bind_gpu_vertex_buffers render_pass 0 buffer_binding 1)
    FMU.with (SDLI.SDL_GPUBufferBinding {sdl_buffer=engine.index_buffer,sdl_offset=0}) (\buffer_binding->SDLF.sdl_bind_gpu_index_buffer render_pass buffer_binding SDLI.sdl_gpu_indexelementsize_32bit)
    DF.mapM_ (do_render_b engine window.adaptive_width window.adaptive_height command_buffer render_pass (maybe engine.default_sampler (\sampler_id->int_map_lookup sampler_id engine.sampler) maybe_sampler_id)) draw_call

do_render_b::Engine a b c d e->FCT.CFloat->FCT.CFloat->FP.Ptr SDLT.SDL_GPUCommandBuffer->FP.Ptr SDLT.SDL_GPURenderPass->FP.Ptr SDLT.SDL_GPUSampler->(Submit_mode,DW.Word32,DW.Word32)->IO ()
do_render_b engine adaptive_width adaptive_height command_buffer render_pass sampler (submit_mode,index_size,index_offset)=do
    let size=4*FS.sizeOf (undefined::FCT.CFloat) in FMA.allocaBytesAligned size 16 $ \ptr->do
        FMU.fillBytes ptr 0 size
        FS.pokeElemOff ptr 0 adaptive_width
        FS.pokeElemOff ptr 1 adaptive_height
        case submit_mode of
            Submit_default->do
                FS.pokeElemOff ptr 2 engine.font_size
                FS.pokeElemOff ptr 3 engine.pixel_range
                SDLF.sdl_push_gpu_vertex_uniform_data command_buffer 0 (FP.castPtr ptr) (fromIntegral size)
                FMU.with (SDLI.SDL_GPUTextureSamplerBinding {sdl_texture=engine.texture,sdl_sampler=sampler}) (\texture_sampler_binding->SDLF.sdl_bind_gpu_fragment_samplers render_pass 0 texture_sampler_binding 1)
            Submit_canvas {canvas_id}->do
                FS.pokeElemOff ptr 2 engine.font_size
                FS.pokeElemOff ptr 3 engine.pixel_range
                SDLF.sdl_push_gpu_vertex_uniform_data command_buffer 0 (FP.castPtr ptr) (fromIntegral size)
                FMU.with (SDLI.SDL_GPUTextureSamplerBinding {sdl_texture=do_render_c (int_map_lookup canvas_id engine.canvas),sdl_sampler=sampler}) (\texture_sampler_binding->SDLF.sdl_bind_gpu_fragment_samplers render_pass 0 texture_sampler_binding 1)
            Submit_album {album_id}->do
                FS.pokeElemOff ptr 2 engine.font_size
                FS.pokeElemOff ptr 3 engine.pixel_range
                SDLF.sdl_push_gpu_vertex_uniform_data command_buffer 0 (FP.castPtr ptr) (fromIntegral size)
                FMU.with (SDLI.SDL_GPUTextureSamplerBinding {sdl_texture=(int_map_lookup album_id engine.album).texture,sdl_sampler=sampler}) (\texture_sampler_binding->SDLF.sdl_bind_gpu_fragment_samplers render_pass 0 texture_sampler_binding 1)
            Submit_atlas_font {atlas_font_id}->case int_map_lookup atlas_font_id engine.atlas_font of
                Atlas_font {texture,font_size,pixel_range}->do
                    FS.pokeElemOff ptr 2 font_size
                    FS.pokeElemOff ptr 3 pixel_range
                    SDLF.sdl_push_gpu_vertex_uniform_data command_buffer 0 (FP.castPtr ptr) (fromIntegral size)
                    FMU.with (SDLI.SDL_GPUTextureSamplerBinding {sdl_texture=texture,sdl_sampler=sampler}) (\texture_sampler_binding->SDLF.sdl_bind_gpu_fragment_samplers render_pass 0 texture_sampler_binding 1)
        SDLF.sdl_draw_gpu_indexed_primitives render_pass index_size 1 index_offset 0 0

do_render_c::Canvas->FP.Ptr SDLT.SDL_GPUTexture
do_render_c canvas=case canvas of
    Free_canvas {texture}->texture
    Bound_canvas {texture}->texture

do_render_canvas::Engine a b c d e->FCT.CFloat->FCT.CFloat->FP.Ptr SDLT.SDL_GPUCommandBuffer->FP.Ptr SDLT.SDL_GPUTexture->Maybe Int->DS.Seq (Submit_mode,DW.Word32,DW.Word32)->DS.Seq Vertex->DS.Seq DW.Word32->DS.Seq Parameter->IO ()
do_render_canvas engine width height command_buffer texture maybe_sampler_id draw_call vertex index parameter=do
    value<-update_buffer engine.device command_buffer engine.vertex_buffer engine.index_buffer engine.parameter_buffer engine.transfer_buffer engine.max_vertex_size engine.max_index_size engine.max_parameter_size vertex index parameter
    FMU.with (SDLI.SDL_GPUColorTargetInfo {sdl_texture=texture,sdl_clear_color=SDLI.SDL_FColor {sdl_r=0,sdl_g=0,sdl_b=0,sdl_a=0},sdl_load_op=SDLI.sdl_gpu_loadop_clear,sdl_store_op=SDLI.sdl_gpu_storeop_store}) $ \color_target_info->do
        render_pass<-SDLF.sdl_begin_gpu_render_pass command_buffer color_target_info 1 FP.nullPtr
        catch_null render_pass
        CM.when value (do_render_canvas_a engine width height command_buffer render_pass maybe_sampler_id draw_call)
        SDLF.sdl_end_gpu_render_pass render_pass

do_render_canvas_a::Engine a b c d e->FCT.CFloat->FCT.CFloat->FP.Ptr SDLT.SDL_GPUCommandBuffer->FP.Ptr SDLT.SDL_GPURenderPass->Maybe Int->DS.Seq (Submit_mode,DW.Word32,DW.Word32)->IO ()
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
    DF.mapM_ (do_render_b engine width height command_buffer render_pass (maybe engine.default_sampler (\sampler_id->int_map_lookup sampler_id engine.sampler) maybe_sampler_id)) draw_call

for_canvas_widget_render::Maybe Int->Projection_path->Selector ()->Widget a b c d e->Engine a b c d e->IO (Engine a b c d e)
for_canvas_widget_render maybe_sampler_id projection_path canvas_widget_render_selector widget engine=case widget of
    Collector {submit}->let (vertex,index,parameter,draw_call)=for_submit submit in for_canvas_widget_render_a projection_path canvas_widget_render_selector engine $ \half_width half_height canvas_id this_engine->do
        command_buffer<-SDLF.sdl_acquire_gpu_command_buffer this_engine.device
        catch_null command_buffer
        case int_map_lookup canvas_id this_engine.canvas of
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
    Group_visual {collect_order,group_visual}->DF.foldlM (\this_engine index->for_canvas_widget_render_c (int_map_lookup index group_visual) action this_engine) engine collect_order
    Vector_visual {collect_order,size,vector_visual}->DF.foldlM (\this_engine index->for_canvas_widget_render_c (vector_visual DV.! catch_out 0 size index) action this_engine) engine collect_order
    _->EE.quick_error "for_canvas_widget_render_b" 0

for_canvas_widget_render_c::Visual->(FCT.CFloat->FCT.CFloat->Int->Engine a b c d e->IO (Engine a b c d e))->Engine a b c d e->IO (Engine a b c d e)
for_canvas_widget_render_c visual action engine=case visual of
    Canvas {half_width,half_height,canvas_id}->action half_width half_height canvas_id engine
    _->return engine

update_buffer::FP.Ptr SDLT.SDL_GPUDevice->FP.Ptr SDLT.SDL_GPUCommandBuffer->FP.Ptr SDLT.SDL_GPUBuffer->FP.Ptr SDLT.SDL_GPUBuffer->FP.Ptr SDLT.SDL_GPUBuffer->FP.Ptr SDLT.SDL_GPUTransferBuffer->Int->Int->Int->DS.Seq Vertex->DS.Seq DW.Word32->DS.Seq Parameter->IO Bool
update_buffer device command_buffer vertex_buffer index_buffer parameter_buffer transfer_buffer max_vertex_size max_index_size max_parameter_size vertex index parameter=let vertex_length=DS.length vertex in let index_length=DS.length index in let parameter_length=DS.length parameter in if vertex_length==0||index_length==0||parameter_length==0 then return False else let single_vertex_size=FS.sizeOf (undefined::Vertex) in let single_index_size=FS.sizeOf (undefined::DW.Word32) in let single_parameter_size=FS.sizeOf (undefined::Parameter) in let vertex_size=vertex_length*single_vertex_size in let index_size=index_length*single_index_size in let parameter_size=parameter_length*single_parameter_size in if max_vertex_size<vertex_size||max_index_size<index_size||max_parameter_size<parameter_size then EE.quick_error "update_buffer" 0 else do
    map_transfer_buffer<-SDLF.sdl_map_gpu_transfer_buffer device transfer_buffer (FMU.fromBool True)
    catch_null map_transfer_buffer
    seq_poke_array single_vertex_size vertex (FP.castPtr map_transfer_buffer)
    seq_poke_array single_index_size index (FP.plusPtr map_transfer_buffer max_vertex_size)
    let size=max_vertex_size+max_index_size
    seq_poke_array single_parameter_size parameter (FP.plusPtr map_transfer_buffer size)
    SDLF.sdl_unmap_gpu_transfer_buffer device transfer_buffer
    copy_pass<-SDLF.sdl_begin_gpu_copy_pass command_buffer
    catch_null copy_pass
    FMU.with (SDLI.SDL_GPUTransferBufferLocation {sdl_transfer_buffer=transfer_buffer,sdl_offset=0}) (\transfer_buffer_location->FMU.with (SDLI.SDL_GPUBufferRegion {sdl_buffer=vertex_buffer,sdl_offset=0,sdl_size=fromIntegral vertex_size}) (\buffer_region->SDLF.sdl_upload_to_gpu_buffer copy_pass transfer_buffer_location buffer_region (FMU.fromBool True)))
    FMU.with (SDLI.SDL_GPUTransferBufferLocation {sdl_transfer_buffer=transfer_buffer,sdl_offset=fromIntegral max_vertex_size}) (\transfer_buffer_location->FMU.with (SDLI.SDL_GPUBufferRegion {sdl_buffer=index_buffer,sdl_offset=0,sdl_size=fromIntegral index_size}) (\buffer_region->SDLF.sdl_upload_to_gpu_buffer copy_pass transfer_buffer_location buffer_region (FMU.fromBool True)))
    FMU.with (SDLI.SDL_GPUTransferBufferLocation {sdl_transfer_buffer=transfer_buffer,sdl_offset=fromIntegral size}) (\transfer_buffer_location->FMU.with (SDLI.SDL_GPUBufferRegion {sdl_buffer=parameter_buffer,sdl_offset=0,sdl_size=fromIntegral parameter_size}) (\buffer_region->SDLF.sdl_upload_to_gpu_buffer copy_pass transfer_buffer_location buffer_region (FMU.fromBool True)))
    SDLF.sdl_end_gpu_copy_pass copy_pass
    return True

{-# INLINE for_render #-}
{-# INLINE do_render_c #-}
{-# INLINE for_canvas_widget_render #-}
{-# INLINE for_canvas_widget_render_a #-}
{-# INLINE for_canvas_widget_render_b #-}
{-# INLINE for_canvas_widget_render_c #-}