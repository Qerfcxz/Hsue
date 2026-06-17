{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Main where

import Engine.Collector
import Engine.Engine
import Engine.Other
import Engine.Request
import Engine.Type
import Engine.Widget
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

text_id::Int
text_id=2

collector_id::Int
collector_id=4

font_id::Int
font_id=1

path_text::Projection_path
path_text=Object_path {projection_id=text_id}

move_collector::Projection_move
move_collector=Object_move {consume=True,projection_id=collector_id}

main_router::Engine ()->Event->Maybe Int
main_router _ _=Just adaptive_trigger_id

adaptive_next::Engine ()->Event->Maybe Int
adaptive_next _ _=Just trigger_id

calculate_width::FCT.CFloat->Int->DSeq.Seq Row->DSeq.Seq (DSeq.Seq Row)->FCT.CFloat
calculate_width base_width row_number _ _=let base_h=45::FCT.CFloat in let r_factor=8::FCT.CFloat in let center_row=9 in if row_number<=17 then let dy=fromIntegral (row_number-center_row)*base_h in let r=r_factor*base_h in let rad=r*r-dy*dy in if rad>0 then max 20 (2*sqrt rad) else 20 else base_width

calculate_typesetting::Int->DSeq.Seq (DSeq.Seq Row)->(FCT.CFloat,FCT.CFloat)
calculate_typesetting row_number _=let base_h=45::FCT.CFloat in if row_number<20 then (0,base_h) else (0,base_h+fromIntegral (row_number-20)*8)

text_trigger_transform::Widget ()->Int->Event->Engine ()->(Engine ()->Engine (),Int,Widget ())
text_trigger_transform widget int event engine=case widget of
    Text {origin,matrix,width,height,y,max_y,article}->case event of
        Time {}->
            let is_w=DSet.member Key_w engine.key 
                is_s=DSet.member Key_s engine.key 
                scroll_speed=5::FCT.CFloat 
                dy=(if is_w then -scroll_speed else 0)+(if is_s then scroll_speed else 0) 
                new_y=max 0 (min (max 0 (max_y-height)) (y+dy)) 
                y_changed=new_y/=y 
                current_int=if y_changed then 1 else int 
            in 
                if current_int==1 then 
                    let update_eng eng=create_request (Render {window_id=window_id,projection_move=move_collector}) (collect path_text collector_id (Index_collect_strategy {seat=0}) eng) 
                    in (update_eng,0,Text {origin=origin,matrix=matrix,width=width,height=height,y=new_y,max_y=max_y,article=article}) 
                else 
                    (id,0,widget)
        At {window_id=this_window_id,action}->if this_window_id==window_id
            then case action of
                Resize {}->(id,1,widget)
                _->(id,0,widget)
            else (id,0,widget)
        _->(id,int,widget)
    _->(id,int,widget)

main::IO ()
main=do
    init_engine
    engine<-create_engine () main_router (\_ _->Image_safe_strategy) (16*mebibyte) (16*mebibyte) (16*mebibyte) (16*mebibyte) 0 0 (Just (div nanosecond 100)) 0 2 1200 800 40 4
    let set_char=DSet.fromList "Haskell SDL3 GPU Typography Engine Dynamic Layout Demo. "
    let phrase=Phrase {phrase_core=DT.pack "Haskell SDL3 GPU Typography Engine Dynamic Layout Demo. ",size=40,red=0.9,green=0.8,blue=0.3,alpha=1}
    let sentence=Sentence {sentence_core=DSeq.singleton phrase,font_id=font_id}
    let paragraph=DSeq.replicate 100 sentence
    let article=DSeq.singleton paragraph
    let seq_request=DSeq.singleton (Create_window {window_id=window_id,title=DT.pack "Perfect Circle Dynamic Typesetting",width=1200,height=800,red=0.1,green=0.12,blue=0.15,alpha=1,window_flag=DSet.singleton Window_resizable}) 
          DSeq.|> Load_font {font_id=font_id,path="arial",char=set_char} 
          DSeq.|> Create_widget {widget_id=adaptive_trigger_id,father=Nothing,widget_request=create_adaptive_window_trigger_request adaptive_next (DIS.singleton window_id)} 
          DSeq.|> Create_widget {widget_id=trigger_id,father=Nothing,widget_request=create_inactive_int_trigger_request text_trigger_transform text_id 1 (\_ _->Nothing)} 
          DSeq.|> Create_widget {widget_id=text_id,father=Nothing,widget_request=Text_request {origin=Point {x=0,y=0},matrix=identity_matrix 0 0,width=960,height=640,article=article,calculate_width=calculate_width 960,calculate_typesetting=calculate_typesetting}} 
          DSeq.|> Create_widget {widget_id=collector_id,father=Nothing,widget_request=Collector_request {initial_min_index=0,initial_max_index=0}}
    let final_engine=DF.foldl' (flip create_request) engine seq_request
    run_engine final_engine
    clean_engine final_engine
    quit_engine