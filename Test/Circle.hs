{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Main where

import Engine.Collector
import Engine.Engine
import Engine.Helper
import Engine.Operation
import Engine.Projection
import Engine.Request
import Engine.Text
import Engine.Type
import Engine.Window
import Extension.Page
import Extension.Type
import Data.Maybe
import qualified Data.Foldable as DF
import qualified Data.IntSet as DIS
import qualified Data.Sequence as DSeq
import qualified Data.Set as DSet
import qualified Data.Text as DT
import qualified Data.Vector as DV
import qualified Foreign.C.Types as FCT
import qualified Foreign.Ptr as FP
import qualified Foreign.Storable as FS
import qualified SDL.Include as SDLI

instance Custom_request () where
    custom_request _ _=error "none"

instance Custom_widget () where
    custom_widget_run _ _ _=error "none"
    custom_widget_collect _ _ _ _=error "none"
    custom_widget_remove _ _=error "none"
    custom_widget_lock _=error "none"
    custom_widget_unlock _ _=error "none"

instance Custom_widget_request () where
    custom_widget_request _ _=error "none"

standard_sampler::Sampler_create_info
standard_sampler=Sampler_create_info {min_filter=Filter_linear,mag_filter=Filter_linear,mipmap_mode=Sampler_mipmap_mode_linear,address_mode_u=Sampler_address_mode_clamp_to_edge,address_mode_v=Sampler_address_mode_clamp_to_edge,address_mode_w=Sampler_address_mode_clamp_to_edge}

canvas_blend_state::Blend_state
canvas_blend_state=Blend_state {src_color_blend_factor=Blend_factor_src_alpha,dst_color_blend_factor=Blend_factor_one_minus_src_alpha,color_blend_op=Blend_op_add,src_alpha_blend_factor=Blend_factor_one,dst_alpha_blend_factor=Blend_factor_one_minus_src_alpha,alpha_blend_op=Blend_op_add,color_write_mask=DSet.fromList [Color_component_r,Color_component_g,Color_component_b,Color_component_a],enable_blend=True,enable_color_write_mask=True}

window_blend_state::Blend_state
window_blend_state=Blend_state {src_color_blend_factor=Blend_factor_src_alpha,dst_color_blend_factor=Blend_factor_one_minus_src_alpha,color_blend_op=Blend_op_add,src_alpha_blend_factor=Blend_factor_src_alpha,dst_alpha_blend_factor=Blend_factor_one_minus_src_alpha,alpha_blend_op=Blend_op_add,color_write_mask=DSet.fromList [Color_component_r,Color_component_g,Color_component_b,Color_component_a],enable_blend=True,enable_color_write_mask=True}

test_time::Int
test_time=1000

window_id::Int
window_id=1

adaptive_trigger_id::Int
adaptive_trigger_id=100

main_widget_id::Int
main_widget_id=2

render_trigger_id::Int
render_trigger_id=10

window_collector_id::Int
window_collector_id=4

text_collector_id::Int
text_collector_id=5

canvas_id::Int
canvas_id=1

pipeline_id::Int
pipeline_id=1

vertex_shader_id::Int
vertex_shader_id=101

fragment_shader_id::Int
fragment_shader_id=102

path_main_widget::Projection_path
path_main_widget=Object_path {leaf_id=main_widget_id}

move_text_collector::Projection_move
move_text_collector=Object_move {consume=True,leaf_id=text_collector_id}

move_window_collector::Projection_move
move_window_collector=Object_move {consume=True,leaf_id=window_collector_id}

type My_engine=Engine (FCT.CFloat,FCT.CFloat) () () () ()

type My_widget=Widget (FCT.CFloat,FCT.CFloat) () () () ()

main_router::Event ()->My_engine->Maybe Int
main_router _ _=Just adaptive_trigger_id

adaptive_next::Event ()->My_engine->Maybe Int
adaptive_next _ _=Just main_widget_id

main_widget_next::Event ()->My_engine->Maybe Int
main_widget_next _ _=Just render_trigger_id

calculate_width::FCT.CFloat->Int->DSeq.Seq Row->DSeq.Seq (DSeq.Seq Row)->FCT.CFloat
calculate_width base_width row_number _ _=let base_h=45 in let r_factor=8 in let center_row=9 in if row_number<=17 then let dy=fromIntegral (row_number-center_row)*base_h in let r=r_factor*base_h in let rad=r*r-dy*dy in if rad>0 then max 20 (2*sqrt rad) else 20 else base_width

calculate_typesetting::Int->DSeq.Seq (DSeq.Seq Row)->(FCT.CFloat,FCT.CFloat)
calculate_typesetting row_number article=let total_rows=DF.sum (fmap DSeq.length article) in if row_number>total_rows then let min_down=DF.foldl' (\acc r->case r of {Row {min_down=d}->d; _->acc}) 0 (DF.foldl' (DSeq.><) DSeq.empty article) in let bottom_padding=15 in (0,bottom_padding-min_down) else let base_h=45 in if row_number<20 then (0,base_h) else (0,base_h+fromIntegral (row_number-20)*8)

main_widget_transform::Event ()->My_engine->My_widget->My_widget
main_widget_transform event engine widget=let clicked_widget=case event of
        At {action=Click {press=Press_down,x,y}}->fromMaybe widget (click_page x y widget)
        _->widget
    in case clicked_widget of
        Vector {vector_widget}->let selected=view_page_bool clicked_widget False in let widget_synced=update_page_bool (const selected) True clicked_widget in if selected then
            case event of
                Time {}->let scroll_speed=5 in let rot_speed=0.05 in case vector_widget DV.! 0 of
                    Vector_visual {arrange,collect_order,size,vector_visual}->let text_visual=vector_visual DV.! 0 in let (tw1,_)=if DSet.member Key_w engine.key then update_text (scroll_text (-scroll_speed)) text_visual else (text_visual,False) in let (tw2,_)=if DSet.member Key_s engine.key then update_text (scroll_text scroll_speed) tw1 else (tw1,False) in let (tw3,_)=if DSet.member Key_a engine.key then update_text scroll_top_text tw2 else (tw2,False) in let (tw4,_)=if DSet.member Key_d engine.key then update_text scroll_bottom_text tw3 else (tw3,False) in let new_vector_visual=vector_visual DV.// [(0,tw4)] in let rotate_theta=(if DSet.member Key_q engine.key then rot_speed else 0)+(if DSet.member Key_e engine.key then -rot_speed else 0) in let new_arrange=if rotate_theta/=0 then let c=cos rotate_theta in let s=sin rotate_theta in let matrix=arrange.matrix in let new_matrix=Matrix {x=matrix.x,y=matrix.y,x_x=matrix.x_x*c+matrix.x_y*s,x_y=matrix.x_y*c-matrix.x_x*s,y_x=matrix.y_x*c+matrix.y_y*s,y_y=matrix.y_y*c-matrix.y_x*s} in arrange {matrix=new_matrix} else arrange in update_vector_widget 0 (const (Vector_visual {arrange=new_arrange,collect_order=collect_order,size=size,vector_visual=new_vector_visual})) widget_synced
                    _->widget_synced
                _->widget_synced
            else widget_synced
        _->widget

force_redraw::My_widget->My_widget
force_redraw=id

render_trigger::Event ()->My_engine->My_engine
render_trigger event engine=case event of
    Time {tick}->if tick>0&&mod tick test_time==0
        then let engineWithRequests=create_request Clean_atlas (create_request (Unlock {leaf_id=main_widget_id}) engine) in let (new_engine,_)=update_lookup_projection_widget path_main_widget force_redraw engineWithRequests in new_engine
        else let (bx,by)=engine.custom in let speed=0.01 in let dx=(if DSet.member Key_l engine.key then speed else 0)-(if DSet.member Key_j engine.key then speed else 0) in let dy=(if DSet.member Key_k engine.key then speed else 0)-(if DSet.member Key_i engine.key then speed else 0) in let nx=bx+dx in let ny=by+dy in let engine_moved=engine {custom=(nx,ny),main_id=engine.main_id} in let e1=collect_page Nothing path_main_widget text_collector_id (Self_selector ()) (Index_strategy {seat=0}) engine_moved in let e2=collect_canvas (Point {x=0,y=0}) identity_matrix 1 1 1 1 Nothing canvas_id window_collector_id (Self_selector ()) (Index_strategy {seat=0}) e1 in create_request (Render {window_id=window_id,render_selector=Self_selector (),projection_move=move_window_collector,maybe_sampler_id=Nothing}) (create_request (Shader_canvas {canvas_id=canvas_id,pipeline_id=pipeline_id,uniform=Uniform {size=16,alignment=16,write= \ptr->let p=FP.castPtr ptr::FP.Ptr FCT.CFloat in FS.pokeElemOff p 0 (fromIntegral tick*0.05)>>FS.pokeElemOff p 1 nx>>FS.pokeElemOff p 2 ny>>FS.pokeElemOff p 3 (1200.0/800.0)},maybe_sampler_id=Nothing}) (create_request (Canvas_render {canvas_id=canvas_id,canvas_render_selector=Self_selector (),projection_move=move_text_collector,maybe_sampler_id=Nothing}) e2))
    _->engine

main::IO ()
main=do
    init_engine
    engine<-quick_create_engine (0.5,0.5) main_router (\_ _->Image_safe_strategy) 16 16 16 16 (Just 100) 2 1200 800 40 4 standard_sampler canvas_blend_state
    let phrase=Phrase {phrase_core=DT.pack "Haskell SDL3 GPU Typography Engine Dynamic Layout Demo. ",size=40,red=0.9,green=0.8,blue=0.3,alpha=1}
    let sentence=Sentence {sentence_core=DSeq.singleton phrase,path="arial"}
    let paragraph=DSeq.replicate 100 sentence
    let article=DSeq.singleton paragraph
    let text_request=Visual_request {visual_request=Text_request {arrange=Arrange {point=Point {x=0,y=0},matrix=identity_matrix,red=1,green=1,blue=1,alpha=1},text_width=960,text_height=640,article=article,calculate_width=calculate_width 960,calculate_typesetting=calculate_typesetting,load=True}}
    let page_request=Page {text_request=text_request,inner_thickness=10,outer_thickness=5,inner_color=Color {red=0.2,green=0.2,blue=0.2,alpha=1},outer_color=Color {red=0.4,green=0.4,blue=0.4,alpha=1},inner_selected_color=Color {red=0.3,green=0.1,blue=0.1,alpha=1},outer_selected_color=Color {red=0.8,green=0.2,blue=0.2,alpha=1}}
    let main_widget_request=create_page_request main_widget_next main_widget_transform page_request
    let seq_request=DSeq.fromList [Create_window {window_id=window_id,title=DT.pack "Black Hole Shader Demo",window_width=1200,window_height=800,red=0.1,green=0.12,blue=0.15,alpha=1,window_flag=DSet.singleton Window_resizable,blend_state=window_blend_state},Create_canvas {canvas_width=1200,canvas_height=800,maybe_canvas_id=Just canvas_id},Create_shader {shader_id=vertex_shader_id,stage=SDLI.sdl_gpu_shaderstage_vertex,num_sampler=0,num_uniform_buffer=0,path="Default"},Create_shader {shader_id=fragment_shader_id,stage=SDLI.sdl_gpu_shaderstage_fragment,num_sampler=1,num_uniform_buffer=1,path="BlackHole"},Create_pipeline {pipeline_id=pipeline_id,maybe_vertex_shader_id=Just vertex_shader_id,fragment_shader_id=fragment_shader_id,blend_state=window_blend_state},Create_widget {leaf_id=adaptive_trigger_id,maybe_father_id=Nothing,widget_request=create_adaptive_window_trigger_request adaptive_next (DIS.singleton window_id)},Create_widget {leaf_id=main_widget_id,maybe_father_id=Nothing,widget_request=main_widget_request},Create_widget {leaf_id=render_trigger_id,maybe_father_id=Nothing,widget_request=Trigger_request {next=const (const Nothing),trigger=render_trigger}},Create_widget {leaf_id=window_collector_id,maybe_father_id=Nothing,widget_request=Collector_request {initial_min_index=0,initial_max_index=0}},Create_widget {leaf_id=text_collector_id,maybe_father_id=Nothing,widget_request=Collector_request {initial_min_index=0,initial_max_index=0}}]
    let final_engine=DF.foldl' (flip create_request) engine seq_request
    run_engine final_engine
    clean_engine final_engine
    quit_engine