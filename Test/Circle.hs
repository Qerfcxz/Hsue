{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Main where

import Engine.Collector
import Engine.Engine
import Engine.Helper
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

collector_id::Int
collector_id=4

path_main_widget::Projection_path
path_main_widget=Object_path {leaf_id=main_widget_id}

move_collector::Projection_move
move_collector=Object_move {consume=True,leaf_id=collector_id}

main_router::Event->Engine ()->Maybe Int
main_router _ _=Just adaptive_trigger_id

adaptive_next::Event->Engine ()->Maybe Int
adaptive_next _ _=Just main_widget_id

main_widget_next::Event->Engine ()->Maybe Int
main_widget_next _ _=Just render_trigger_id

calculate_width::FCT.CFloat->Int->DSeq.Seq Row->DSeq.Seq (DSeq.Seq Row)->FCT.CFloat
calculate_width base_width row_number _ _=let base_h=45 in let r_factor=8 in let center_row=9 in if row_number<=17 then let dy=fromIntegral (row_number-center_row)*base_h in let r=r_factor*base_h in let rad=r*r-dy*dy in if rad>0 then max 20 (2*sqrt rad) else 20 else base_width

calculate_typesetting::Int->DSeq.Seq (DSeq.Seq Row)->(FCT.CFloat,FCT.CFloat)
calculate_typesetting row_number _=let base_h=45 in if row_number<20 then (0,base_h) else (0,base_h+fromIntegral (row_number-20)*8)

main_widget_transform::Event->Engine ()->Widget ()->(Widget (),Engine ()->Engine ())
main_widget_transform event engine widget=case widget of
    Double {which,first_widget}->case first_widget of
        Text {origin,matrix,width,height,y,max_y,article,charset,locked}->case event of
            Time {}->let scroll_speed=5 in let new_y=max 0 (min (max 0 (max_y-height)) (y+if DSet.member Key_w engine.key then -scroll_speed else 0+if DSet.member Key_s engine.key then scroll_speed else 0)) in if new_y/=y then (Double {which=which,first_widget=Text {origin=origin,matrix=matrix,width=width,height=height,y=new_y,max_y=max_y,article=article,charset=charset,locked=locked},second_widget=Store {store=Data_bool {bool=True}}},id) else (widget,id)
            At {window_id=this_window_id,action}->if this_window_id==window_id
                then case action of
                    Resize {}->(Double {which=which,first_widget=first_widget,second_widget=Store {store=Data_bool {bool=True}}},id)
                    _->(widget,id)
                else (widget,id)
            _->(widget,id)
        _->(widget,id)
    _->(widget,id)

check_and_reset_data_int::Widget ()->Maybe (Widget ())
check_and_reset_data_int widget=case widget of
    Widget_trigger {next,widget_trigger,widget=inner_widget}->case inner_widget of
        Double {which,first_widget,second_widget}->case second_widget of
            Store {store}->case store of
                Data_bool {bool}->if bool then Just (Widget_trigger {next=next,widget_trigger=widget_trigger,widget=Double {which=which,first_widget=first_widget,second_widget=Store {store=Data_bool {bool=False}}}}) else Nothing
                _->Nothing
            _->Nothing
        _->Nothing
    _->Nothing

force_redraw::Widget ()->Widget ()
force_redraw widget=case widget of
    Widget_trigger {next,widget_trigger,widget=inner_widget}->
        Widget_trigger {next=next,widget_trigger=widget_trigger,widget=force_redraw inner_widget}
    Double {which,first_widget}->
        Double {which=which,first_widget=first_widget,second_widget=Store {store=Data_bool {bool=True}}}
    _->widget

render_trigger::Event->Engine ()->Engine ()
render_trigger event engine=case event of
    Time {tick}->if tick>0&&mod tick test_time==0 then let engineWithRequests=create_request Clean_atlas (create_request (Unlock {leaf_id=main_widget_id}) engine) in let (new_engine,_)=update_lookup_projection_widget path_main_widget force_redraw engineWithRequests in new_engine else let widget=lookup_projection_widget path_main_widget engine in case check_and_reset_data_int widget of
        Just _->let new_engine=maybe_collect_limited_update check_and_reset_data_int id path_main_widget 0 collector_id (Index_strategy {seat=0}) engine in create_request (Render {window_id=window_id,projection_move=move_collector}) new_engine
        Nothing->engine
    _->engine

main::IO ()
main=do
    init_engine
    engine<-create_engine () main_router (\_ _->Image_safe_strategy) (16*mebibyte) (16*mebibyte) (16*mebibyte) (16*mebibyte) 0 0 0 (Just (div nanosecond 100)) 0 2 1200 800 40 4
    let phrase=Phrase {phrase_core=DT.pack "Haskell SDL3 GPU Typography Engine Dynamic Layout Demo. ",size=40,red=0.9,green=0.8,blue=0.3,alpha=1}
    let sentence=Sentence {sentence_core=DSeq.singleton phrase,path="arial"}
    let paragraph=DSeq.replicate 100 sentence
    let article=DSeq.singleton paragraph
    let text_request=Text_request {origin=Point {x=0,y=0},matrix=identity_matrix,width=960,height=640,article=article,calculate_width=calculate_width 960,calculate_typesetting=calculate_typesetting,load=True}
    let main_widget_request=Widget_trigger_request {next=main_widget_next,widget_trigger=main_widget_transform,widget_request=Double_request {which=True,first_widget_request=text_request,second_widget_request=Store_request {store=Data_bool {bool=True}}}}
    let seq_request=DSeq.singleton (Create_window {window_id=window_id,title=DT.pack "Perfect Circle Dynamic Typesetting",width=1200,height=800,red=0.1,green=0.12,blue=0.15,alpha=1,window_flag=DSet.singleton Window_resizable}) DSeq.|> Create_widget {leaf_id=adaptive_trigger_id,maybe_father_id=Nothing,widget_request=create_adaptive_window_trigger_request adaptive_next (DIS.singleton window_id)} DSeq.|> Create_widget {leaf_id=main_widget_id,maybe_father_id=Nothing,widget_request=main_widget_request} DSeq.|> Create_widget {leaf_id=render_trigger_id,maybe_father_id=Nothing,widget_request=Trigger_request {next= \_ _->Nothing,trigger=render_trigger}} DSeq.|> Create_widget {leaf_id=collector_id,maybe_father_id=Nothing,widget_request=Collector_request {initial_min_index=0,initial_max_index=0}}
    let final_engine=DF.foldl' (flip create_request) engine seq_request
    run_engine final_engine
    clean_engine final_engine
    quit_engine