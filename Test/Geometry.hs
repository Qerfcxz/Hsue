{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Main where

import Engine.Collector
import Engine.Engine
import Engine.Other
import Engine.Projection
import Engine.Request
import Engine.Type
import Engine.Window
import qualified Data.Foldable as DF
import qualified Data.IntSet as DIS
import qualified Data.Sequence as DSeq
import qualified Data.Set as DSet
import qualified Data.Text as DT
import qualified Foreign.C.Types as FCT

window_id::Int
window_id=1

adaptive_trigger_id::Int
adaptive_trigger_id=10

trigger_id::Int
trigger_id=1

geom_id::Int
geom_id=2

geom_tri_id::Int
geom_tri_id=4

geom_poly_id::Int
geom_poly_id=5

node_geom_id::Int
node_geom_id=6

node_tri_id::Int
node_tri_id=7

node_poly_id::Int
node_poly_id=8

collector_id::Int
collector_id=3

path_geom::Projection_path
path_geom=Image_safe_path {leaf_id=geom_id}

path_tri::Projection_path
path_tri=Image_safe_path {leaf_id=geom_tri_id}

path_poly::Projection_path
path_poly=Image_safe_path {leaf_id=geom_poly_id}

move_collector::Projection_move
move_collector=Object_move {consume=True,leaf_id=collector_id}

main_router::Engine ()->Event->Maybe Int
main_router _ _=Just adaptive_trigger_id

adaptive_next::Engine ()->Event->Maybe Int
adaptive_next _ _=Just trigger_id

geom_widget_transform::Engine ()->Widget ()->Widget ()
geom_widget_transform engine widget=case widget of
    Visual {origin,maybe_clip,alpha,visual}->let time_sec=fromIntegral engine.time/1000000000::FCT.CFloat in let sx=1+0.5*sin time_sec in let sy=1+0.5*cos time_sec in Visual {origin=origin,matrix=Matrix {x=0,y=0,x_x=sx,x_y=0,y_x=0,y_y=sy},maybe_clip=maybe_clip,red=0.5+0.5*sin time_sec,green=0.5+0.5*cos time_sec,blue=0.5+0.5*sin (time_sec*1.5),alpha=alpha,visual=visual}
    _->widget

tri_widget_transform::Engine ()->Widget ()->Widget ()
tri_widget_transform engine widget=case widget of
    Visual {origin,maybe_clip,alpha,visual}->let time_sec=fromIntegral engine.time/1000000000::FCT.CFloat in let theta_tri=time_sec*0.8 in let cos_theta=cos theta_tri in let sin_theta=sin theta_tri in Visual {origin=origin,matrix=Matrix {x=0,y=0,x_x=cos_theta,x_y= -sin_theta,y_x=sin_theta,y_y=cos_theta},maybe_clip=maybe_clip,red=0.2,green=0.8,blue=0.6,alpha=alpha,visual=visual}
    _->widget

poly_widget_transform::Engine ()->Widget ()->Widget ()
poly_widget_transform engine widget=case widget of
    Visual {origin,maybe_clip,alpha,visual}->let time_sec=fromIntegral engine.time/1000000000::FCT.CFloat in let theta_poly=(-(time_sec*0.5)) in let cos_theta=cos theta_poly in let sin_theta=sin theta_poly in Visual {origin=origin,matrix=Matrix {x=0,y=0,x_x=cos_theta,x_y= -sin_theta,y_x=sin_theta,y_y=cos_theta},maybe_clip=maybe_clip,red=0.9,green=0.3,blue=0.2,alpha=alpha,visual=visual}
    _->widget

trigger::Event->Engine ()->Engine ()
trigger event engine=case event of
    Time {}->let new_engine=create_image_safe geom_id (create_image_safe geom_tri_id (create_image_safe geom_poly_id engine)) in let new_new_engine=collect path_poly collector_id (Index_strategy {seat= -2}) (collect path_tri collector_id (Index_strategy {seat= -1}) (collect path_geom collector_id (Index_strategy {seat=0}) new_engine)) in create_request (Render {window_id=window_id,projection_move=move_collector}) new_new_engine
    _->engine

main::IO ()
main=do
    init_engine
    engine<-create_engine () main_router (\_ _->Image_safe_strategy) (16*mebibyte) (16*mebibyte) (16*mebibyte) (16*mebibyte) 0 0 0 (Just (div nanosecond 100)) 0 2 1200 800 40 4
    let seq_request=DSeq.singleton (Create_window {window_id=window_id,title=DT.pack "Hsue Adaptive Demo",width=800,height=600,red=0,green=0,blue=0,alpha=1,window_flag=DSet.singleton Window_resizable}) DSeq.|> Create_widget {leaf_id=adaptive_trigger_id,maybe_father_id=Nothing,widget_request=create_adaptive_window_trigger_request adaptive_next (DIS.singleton window_id)} DSeq.|> Create_widget {leaf_id=trigger_id,maybe_father_id=Nothing,widget_request=Trigger_request {trigger=trigger,next= \_ _->Nothing}} DSeq.|> Create_node {node_id=node_tri_id,maybe_father_id=Nothing,event_transform= \_ this_event->this_event,widget_transform=tri_widget_transform} DSeq.|> Create_node {node_id=node_poly_id,maybe_father_id=Nothing,event_transform= \_ this_event->this_event,widget_transform=poly_widget_transform} DSeq.|> Create_node {node_id=node_geom_id,maybe_father_id=Nothing,event_transform= \_ this_event->this_event,widget_transform=geom_widget_transform} DSeq.|> Create_widget {leaf_id=geom_tri_id,maybe_father_id=Just node_tri_id,widget_request=Visual_request {origin=Point {x=0,y=0},matrix=identity_matrix 0 0,maybe_clip=Just (Clip (-50) (-200) 50 200),red=0.2,green=0.8,blue=0.6,alpha=1,visual_request=Triangle_request {first_point=Point {x= -150,y= -150},second_point=Point {x=150,y= -150},third_point=Point {x=0,y=200}}}} DSeq.|> Create_widget {leaf_id=geom_poly_id,maybe_father_id=Just node_poly_id,widget_request=Visual_request {origin=Point {x=0,y=0},matrix=identity_matrix 0 0,maybe_clip=Nothing,red=0.9,green=0.3,blue=0.2,alpha=1,visual_request=Regular_polygon_request {number=6,radius=220,angle=0}}} DSeq.|> Create_widget {leaf_id=geom_id,maybe_father_id=Just node_geom_id,widget_request=Visual_request {origin=Point {x=0,y=0},matrix=identity_matrix 0 0,maybe_clip=Nothing,red=1,green=1,blue=1,alpha=1,visual_request=Large_picture_request {path="test"}}} DSeq.|> Create_widget {leaf_id=collector_id,maybe_father_id=Nothing,widget_request=Collector_request {initial_min_index=0,initial_max_index=0}}
    let final_engine=DF.foldl' (flip create_request) engine seq_request
    run_engine final_engine
    clean_engine final_engine
    quit_engine