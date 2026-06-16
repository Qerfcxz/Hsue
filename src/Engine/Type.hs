{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

module Engine.Type where

import qualified SDL.Type as T
import qualified Data.Aeson as DA
import qualified Data.Aeson.Types as DAT
import qualified Data.IntMap as DIM
import qualified Data.IntSet as DIS
import qualified Data.Map as DM
import qualified Data.Sequence as DSeq
import qualified Data.Set as DSet
import qualified Data.Text as DT
import qualified Data.Word as DW
import qualified Foreign.C.Types as FCT
import qualified Foreign.Ptr as FP
import qualified Foreign.Storable as FS

data Engine a=Engine {state::a,main_id::Engine a->Event->Maybe Int,projection_strategy::Engine a->Event->Projection_strategy,callback::FP.FunPtr (FP.Ptr ()->DW.Word32->DW.Word64->IO DW.Word64),font::DIM.IntMap Font,atlas::Atlas,album::DIM.IntMap Album,active::DIM.IntMap (Active a),inactive::DIM.IntMap (Inactive a),node::DIM.IntMap (Node a),window::DIM.IntMap Window,window_map::DM.Map DW.Word32 Int,request::DSeq.Seq (Request a),key::DSet.Set Key,device::FP.Ptr T.SDL_GPUDevice,texture::FP.Ptr T.SDL_GPUTexture,sampler::FP.Ptr T.SDL_GPUSampler,vertex_shader::FP.Ptr T.SDL_GPUShader,fragment_shader::FP.Ptr T.SDL_GPUShader,vertex_buffer::FP.Ptr T.SDL_GPUBuffer,index_buffer::FP.Ptr T.SDL_GPUBuffer,parameter_buffer::FP.Ptr T.SDL_GPUBuffer,transfer_buffer::FP.Ptr T.SDL_GPUTransferBuffer,picture_transfer_buffer::FP.Ptr T.SDL_GPUTransferBuffer,picture_size::FCT.CInt,vertex_size::Int,index_size::Int,parameter_size::Int,initial_album_id::Int,album_id::Int,count::Int,timer::Timer,time::DW.Word64,event_number::DW.Word32,padding::DW.Word32,width::DW.Word32,height::DW.Word32,reciprocal_width::FCT.CFloat,reciprocal_height::FCT.CFloat,u::FCT.CFloat,v::FCT.CFloat,font_size::FCT.CFloat,pixel_range::FCT.CFloat}

data Active a=Active {ancestry::DSeq.Seq Int,projection::Projection (Widget a),next::Engine a->Event->Maybe Int}

data Inactive a=Inactive {ancestry::DSeq.Seq Int,projection::Projection (Widget a)}

data Node a=Node {ancestry::DSeq.Seq Int,active_child::DIS.IntSet,inactive_child::DIS.IntSet,node_child::DIS.IntSet,event_transform::Engine a->Event->Event,widget_transform::Engine a->Widget a->Widget a}

data Widget a=Trigger {trigger::Event->Engine a->Engine a}|Io_trigger {io_trigger::Event->Engine a->IO (Engine a)}|Int_trigger {int_trigger::Int->Event->(Engine a->Engine a,Int),int::Int}|Int_io_trigger {int_io_trigger::Int->Event->(Engine a->IO (Engine a),Int),int::Int}|Collector {initial_min_index::Int,initial_max_index::Int,min_index::Int,max_index::Int,submit::DIM.IntMap (DSeq.Seq Submit)}|Visual {origin::Point,matrix::Matrix,maybe_clip::Maybe Clip,red::FCT.CFloat,green::FCT.CFloat,blue::FCT.CFloat,alpha::FCT.CFloat,visual::Visual}|Text {origin::Point,matrix::Matrix,width::FCT.CFloat,height::FCT.CFloat,y::FCT.CFloat,article::DSeq.Seq (DSeq.Seq Row)}

data Request a=Reset_timer {interval::DW.Word64}|Stop_timer|Stop_timer_safe|Create_widget {widget_id::Int,father::Maybe Int,widget_request::Widget_request a}|Remove_widget {widget_id::Int,widget_type::Bool}|Create_node {node_id::Int,father::Maybe Int,event_transform::Engine a->Event->Event,widget_transform::Engine a->Widget a->Widget a}|Remove_node {node_id::Int}|Create_window {window_id::Int,title::DT.Text,width::FCT.CInt,height::FCT.CInt,red::FCT.CFloat,green::FCT.CFloat,blue::FCT.CFloat,alpha::FCT.CFloat,window_flag::DSet.Set Window_flag}|Remove_window {window_id::Int}|Clean_atlas|Reload_inactive {inactive_id::Int}|Load_font {font_id::Int,path::String,char::DSet.Set Char}|Render {window_id::Int,projection_move::Projection_move}|Io {io::Engine a->IO (Engine a)}

data Widget_request a=Trigger_request {trigger::Event->Engine a->Engine a,next::Engine a->Event->Maybe Int}|Io_trigger_request {io_trigger::Event->Engine a->IO (Engine a),next::Engine a->Event->Maybe Int}|Int_trigger_request {int_trigger::Int->Event->(Engine a->Engine a,Int),int::Int,next::Engine a->Event->Maybe Int}|Int_io_trigger_request {int_io_trigger::Int->Event->(Engine a->IO (Engine a),Int),int::Int,next::Engine a->Event->Maybe Int}|Collector_request {initial_min_index::Int,initial_max_index::Int}|Visual_request {origin::Point,matrix::Matrix,maybe_clip::Maybe Clip,red::FCT.CFloat,green::FCT.CFloat,blue::FCT.CFloat,alpha::FCT.CFloat,visual_request::Visual_request}|Text_request {origin::Point,matrix::Matrix,width::FCT.CFloat,height::FCT.CFloat,article::DSeq.Seq (DSeq.Seq Sentence),calculate_width::Int->DSeq.Seq Row->DSeq.Seq (DSeq.Seq Row)->FCT.CFloat,calculate_typesetting::Int->DSeq.Seq (DSeq.Seq Row)->(FCT.CFloat,FCT.CFloat)}

data Projection a=Without {object::a}|With {object::a,image::a}

data Album=Album {width::DW.Word32,height::DW.Word32,texture::FP.Ptr T.SDL_GPUTexture}

data Timer=Off|On {timer_id::DW.Word32,interval::DW.Word64}

data Window=Window {window_id::Int,sdl_window_id::DW.Word32,sdl_window::FP.Ptr T.SDL_Window,graphics_pipeline::FP.Ptr T.SDL_GPUGraphicsPipeline,design_width::FCT.CFloat,design_height::FCT.CFloat,adaptive_width::FCT.CFloat,adaptive_height::FCT.CFloat,red::FCT.CFloat,green::FCT.CFloat,blue::FCT.CFloat,alpha::FCT.CFloat}

data Row=Blank|Row {row_core::DSeq.Seq Character,x::FCT.CFloat,y::FCT.CFloat,width::FCT.CFloat,min_down::FCT.CFloat,max_up::FCT.CFloat,min_descent::FCT.CFloat,max_ascent::FCT.CFloat}

data Character=Character {unicode::Int,font_id::Int,size::FCT.CFloat,left::FCT.CFloat,down::FCT.CFloat,right::FCT.CFloat,up::FCT.CFloat,min_u::FCT.CFloat,min_v::FCT.CFloat,max_u::FCT.CFloat,max_v::FCT.CFloat,red::FCT.CFloat,green::FCT.CFloat,blue::FCT.CFloat,alpha::FCT.CFloat}

data Sentence=Sentence {sentence_core::DSeq.Seq Phrase,font_id::Int}

data Phrase=Phrase {phrase_core::DT.Text,size::FCT.CFloat,red::FCT.CFloat,green::FCT.CFloat,blue::FCT.CFloat,alpha::FCT.CFloat}

data Visual=Triangle {first_point::Point,second_point::Point,third_point::Point}|Convex_polygon {point::DSeq.Seq Point}|Regular_polygon {number::Int,radius::FCT.CFloat,angle::FCT.CFloat}|Picture {width::FCT.CFloat,height::FCT.CFloat,album_id::Int,min_u::FCT.CFloat,min_v::FCT.CFloat,max_u::FCT.CFloat,max_v::FCT.CFloat,locked::Bool}|Large_picture {width::FCT.CFloat,height::FCT.CFloat,album_id::Int}

data Visual_request=Triangle_request {first_point::Point,second_point::Point,third_point::Point}|Convex_polygon_request {point::DSeq.Seq Point}|Regular_polygon_request {number::Int,radius::FCT.CFloat,angle::FCT.CFloat}|Picture_request {path::String}|Large_picture_request {path::String}

data Clip=Clip {left::FCT.CFloat,down::FCT.CFloat,right::FCT.CFloat,up::FCT.CFloat}

data Matrix=Matrix {x::FCT.CFloat,y::FCT.CFloat,x_x::FCT.CFloat,x_y::FCT.CFloat,y_x::FCT.CFloat,y_y::FCT.CFloat}

data Point=Point {x::FCT.CFloat,y::FCT.CFloat}

data Rectangle=Rectangle {left::DW.Word32,down::DW.Word32,right::DW.Word32,up::DW.Word32}

data Atlas=Leaf_atlas {rectangle::Rectangle,used::Bool}|Node_atlas {rectangle::Rectangle,left_atlas::Atlas,right_atlas::Atlas}

data Glyph=Glyph {advance::FCT.CFloat,left::FCT.CFloat,down::FCT.CFloat,right::FCT.CFloat,up::FCT.CFloat,min_u::FCT.CFloat,min_v::FCT.CFloat,max_u::FCT.CFloat,max_v::FCT.CFloat}

data Font=Font {descent::FCT.CFloat,ascent::FCT.CFloat,glyph::DIM.IntMap Glyph}

data Window_flag=Window_fullscreen|Window_hidden|Window_borderless|Window_resizable

data Projection_strategy=Object_strategy|Image_strategy|Image_safe_strategy

data Projection_path=Object_path {projection_id::Int}|Image_path {projection_id::Int}|Image_safe_path {projection_id::Int}

data Projection_move=Object_move {consume::Bool,projection_id::Int}|Image_move {projection_id::Int}|Image_safe_move {projection_id::Int}

data Collect_strategy=Min_collect_strategy|Max_collect_strategy|Index_collect_strategy {seat::Int}

data Event=Quit|Time {tick::Int,time::DW.Word64,interval::DW.Word64}|At {window_id::Int,action::Action}

data Action=Close|Resize {width::FCT.CFloat,height::FCT.CFloat}|Press {press::Press,change::Key,maintain::DSet.Set Key}

data Press=Press_up|Press_down

data Key=Key_unknown|Key_a|Key_b|Key_c|Key_d|Key_e|Key_f|Key_g|Key_h|Key_i|Key_j|Key_k|Key_l|Key_m|Key_n|Key_o|Key_p|Key_q|Key_r|Key_s|Key_t|Key_u|Key_v|Key_w|Key_x|Key_y|Key_z deriving (Eq,Ord)

data Submit=Submit {maybe_album_id::Maybe Int,vertex::DSeq.Seq Vertex,index::DSeq.Seq DW.Word32,parameter::Parameter,vertex_length::DW.Word32,index_length::DW.Word32}

data Extended a=Negative_infinity|Finite {number::a}|Positive_infinity deriving (Eq,Ord)

data Vertex=Vertex {red::FCT.CFloat,green::FCT.CFloat,blue::FCT.CFloat,alpha::FCT.CFloat,x::FCT.CFloat,y::FCT.CFloat,u::FCT.CFloat,v::FCT.CFloat,parameter_id::FCT.CFloat,size::FCT.CFloat}

instance FS.Storable Vertex where
    sizeOf _=40
    alignment _=4
    peek _=error "peek: error 1"
    poke ptr vertex=case vertex of
        (Vertex {red,green,blue,alpha,x,y,u,v,parameter_id,size})->let new_ptr=FP.castPtr ptr in do
            FS.pokeElemOff new_ptr 0 red
            FS.pokeElemOff new_ptr 1 green
            FS.pokeElemOff new_ptr 2 blue
            FS.pokeElemOff new_ptr 3 alpha
            FS.pokeElemOff new_ptr 4 x
            FS.pokeElemOff new_ptr 5 y
            FS.pokeElemOff new_ptr 6 u
            FS.pokeElemOff new_ptr 7 v
            FS.pokeElemOff new_ptr 8 parameter_id
            FS.pokeElemOff new_ptr 9 size

data Parameter=Parameter {x::FCT.CFloat,y::FCT.CFloat,x_x::FCT.CFloat,x_y::FCT.CFloat,y_x::FCT.CFloat,y_y::FCT.CFloat,clip_flag::FCT.CFloat,clip_left::FCT.CFloat,clip_down::FCT.CFloat,clip_right::FCT.CFloat,clip_up::FCT.CFloat}

instance FS.Storable Parameter where
    sizeOf _=48
    alignment _=4
    peek _=error "peek: error 1"
    poke ptr parameter=case parameter of
        (Parameter {x,y,x_x,x_y,y_x,y_y,clip_flag,clip_left,clip_down,clip_right,clip_up})->let new_ptr=FP.castPtr ptr::FP.Ptr FCT.CFloat in do
            FS.pokeElemOff new_ptr 0 x
            FS.pokeElemOff new_ptr 1 y
            FS.pokeElemOff new_ptr 2 x_x
            FS.pokeElemOff new_ptr 3 x_y
            FS.pokeElemOff new_ptr 4 y_x
            FS.pokeElemOff new_ptr 5 y_y
            FS.pokeElemOff new_ptr 6 0
            FS.pokeElemOff new_ptr 7 clip_flag
            FS.pokeElemOff new_ptr 8 clip_left
            FS.pokeElemOff new_ptr 9 clip_down
            FS.pokeElemOff new_ptr 10 clip_right
            FS.pokeElemOff new_ptr 11 clip_up

data MSDF_Metrics=MSDF_Metrics {msdf_ascender::FCT.CFloat,msdf_descender::FCT.CFloat}

instance DA.FromJSON MSDF_Metrics where
    parseJSON=DA.withObject "MSDF_Metrics" (\object->fmap (MSDF_Metrics . realToFrac) (object DA..: "ascender"::DAT.Parser Double)<*>fmap realToFrac (object DA..: "descender"::DAT.Parser Double))

data MSDF_Bounds=MSDF_Bounds {msdf_left::FCT.CFloat,msdf_bottom::FCT.CFloat,msdf_right::FCT.CFloat,msdf_top::FCT.CFloat}

instance DA.FromJSON MSDF_Bounds where
    parseJSON=DA.withObject "MSDF_Bounds" (\object->fmap (MSDF_Bounds . realToFrac) (object DA..: "left"::DAT.Parser Double)<*>fmap realToFrac (object DA..: "bottom"::DAT.Parser Double)<*>fmap realToFrac (object DA..: "right"::DAT.Parser Double)<*>fmap realToFrac (object DA..: "top"::DAT.Parser Double))

data MSDF_Glyph=MSDF_Glyph {msdf_unicode::Int,msdf_advance::FCT.CFloat,msdf_planeBounds::MSDF_Bounds,msdf_atlasBounds::MSDF_Bounds}

instance DA.FromJSON MSDF_Glyph where
    parseJSON=DA.withObject "MSDF_Glyph" (\object->fmap MSDF_Glyph (object DA..: "unicode")<*>fmap realToFrac (object DA..: "advance"::DAT.Parser Double)<*>((object DA..:? "planeBounds") DA..!= MSDF_Bounds {msdf_left=0,msdf_bottom=0,msdf_right=0,msdf_top=0})<*>((object DA..:? "atlasBounds") DA..!= MSDF_Bounds {msdf_left=0,msdf_bottom=0,msdf_right=0,msdf_top=0}))

data MSDF_Output=MSDF_Output {msdf_metrics::MSDF_Metrics,msdf_glyphs::DSeq.Seq MSDF_Glyph}

instance DA.FromJSON MSDF_Output where
    parseJSON=DA.withObject "MSDF_Output" (\object->fmap MSDF_Output (object DA..: "metrics")<*>(object DA..: "glyphs"))