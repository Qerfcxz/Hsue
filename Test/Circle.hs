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
adaptive_trigger_id=100

main_widget_id::Int
main_widget_id=2

render_trigger_id::Int
render_trigger_id=10

collector_id::Int
collector_id=4

font_id::Int
font_id=1

path_main_widget::Projection_path
path_main_widget=Object_path {leaf_id=main_widget_id}

move_collector::Projection_move
move_collector=Object_move {consume=True,leaf_id=collector_id}

main_router::Engine ()->Event->Maybe Int
main_router _ _=Just adaptive_trigger_id

adaptive_next::Engine ()->Event->Maybe Int
adaptive_next _ _=Just main_widget_id

main_widget_next::Engine ()->Event->Maybe Int
main_widget_next _ _=Just render_trigger_id

calculate_width::FCT.CFloat->Int->DSeq.Seq Row->DSeq.Seq (DSeq.Seq Row)->FCT.CFloat
calculate_width base_width row_number _ _=let base_h=45::FCT.CFloat in let r_factor=8::FCT.CFloat in let center_row=9 in if row_number<=17 then let dy=fromIntegral (row_number-center_row)*base_h in let r=r_factor*base_h in let rad=r*r-dy*dy in if rad>0 then max 20 (2*sqrt rad) else 20 else base_width

calculate_typesetting::Int->DSeq.Seq (DSeq.Seq Row)->(FCT.CFloat,FCT.CFloat)
calculate_typesetting row_number _=let base_h=45::FCT.CFloat in if row_number<20 then (0,base_h) else (0,base_h+fromIntegral (row_number-20)*8)

main_widget_transform::Widget ()->Event->Engine ()->(Engine ()->Engine (),Widget ())
main_widget_transform widget event engine=case widget of
    Double {which,first_widget}->case first_widget of
        Text {origin,matrix,width,height,y,max_y,article}->case event of
            Time {}->let scroll_speed=5 in let new_y=max 0 (min (max 0 (max_y-height)) (y+if DSet.member Key_w engine.key then -scroll_speed else 0+if DSet.member Key_s engine.key then scroll_speed else 0)) in if new_y/=y then (id,Double {which=which,first_widget=Text {origin=origin,matrix=matrix,width=width,height=height,y=new_y,max_y=max_y,article=article},second_widget=Data_int {int=1}}) else (id,widget)
            At {window_id=this_window_id,action}->if this_window_id==window_id
                then case action of
                    Resize {}->(id,Double {which=which,first_widget=first_widget,second_widget=Data_int {int=1}})
                    _->(id,widget)
                else (id,widget)
            _->(id,widget)
        _->(id,widget)
    _->(id,widget)

check_and_reset_data_int::Widget ()->Maybe (Widget ())
check_and_reset_data_int widget=case widget of
    Widget_trigger {next,widget_trigger,widget=inner_widget}->case inner_widget of
        Double {which,first_widget,second_widget}->case second_widget of
            Data_int {int}->if int==1 then Just (Widget_trigger {next=next,widget_trigger=widget_trigger,widget=Double {which=which,first_widget=first_widget,second_widget=Data_int {int=0}}}) else Nothing
            _->Nothing
        _->Nothing
    _->Nothing

render_trigger::Event->Engine ()->Engine ()
render_trigger event engine=case event of
    Time {}->let widget=lookup_projection_widget path_main_widget engine in case check_and_reset_data_int widget of
        Just _->let new_engine=second_maybe_update_collect check_and_reset_data_int path_main_widget collector_id (Index_strategy {seat=0}) engine in create_request (Render {window_id=window_id,projection_move=move_collector}) new_engine
        Nothing->engine
    _->engine

main::IO ()
main=do
    init_engine
    engine<-create_engine () main_router (\_ _->Image_safe_strategy) (16*mebibyte) (16*mebibyte) (16*mebibyte) (16*mebibyte) 0 0 (Just (div nanosecond 100)) 0 2 1200 800 40 4
    let set_char=DSet.fromList "Haskell SDL3 GPU Typography Engine Dynamic Layout Demo. "
    let phrase=Phrase {phrase_core=DT.pack "Haskell SDL3 GPU Typography Engine Dynamic Layout Demo. ",size=40,red=0.9,green=0.8,blue=0.3,alpha=1}
    let sentence=Sentence {sentence_core=DSeq.singleton phrase,font_id=font_id}
    let paragraph=DSeq.replicate 100 sentence
    let article=DSeq.singleton paragraph
    let text_request=Text_request {origin=Point {x=0,y=0},matrix=identity_matrix 0 0,width=960,height=640,article=article,calculate_width=calculate_width 960,calculate_typesetting=calculate_typesetting}
    let main_widget_request=Widget_trigger_request {next=main_widget_next,widget_trigger=main_widget_transform,widget_request=Double_request {which=True,first_widget_request=text_request,second_widget_request=Data_int_request {int=1}}}
    let seq_request=DSeq.singleton (Create_window {window_id=window_id,title=DT.pack "Perfect Circle Dynamic Typesetting",width=1200,height=800,red=0.1,green=0.12,blue=0.15,alpha=1,window_flag=DSet.singleton Window_resizable}) 
          DSeq.|> Load_font {font_id=font_id,path="arial",char=set_char} 
          DSeq.|> Create_widget {leaf_id=adaptive_trigger_id,maybe_father_id=Nothing,widget_request=create_adaptive_window_trigger_request adaptive_next (DIS.singleton window_id)} 
          DSeq.|> Create_widget {leaf_id=main_widget_id,maybe_father_id=Nothing,widget_request=main_widget_request} 
          DSeq.|> Create_widget {leaf_id=render_trigger_id,maybe_father_id=Nothing,widget_request=Trigger_request {next= \_ _->Nothing,trigger=render_trigger}} 
          DSeq.|> Create_widget {leaf_id=collector_id,maybe_father_id=Nothing,widget_request=Collector_request {initial_min_index=0,initial_max_index=0}}
    let final_engine=DF.foldl' (flip create_request) engine seq_request
    run_engine final_engine
    clean_engine final_engine
    quit_engine