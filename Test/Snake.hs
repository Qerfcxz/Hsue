{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE StrictData #-}

module Main where

import Engine.Engine
import Engine.Type
import Engine.Request
import Engine.Other
import Engine.Projection
import Engine.Collector
import qualified Data.IntMap as DIM
import qualified Data.Set as DSet
import qualified Data.Sequence as DS
import qualified Data.Text as DT
import qualified Foreign.C.Types as FCT

data Direction=DUp|DDown|DLeft|DRight deriving (Eq)

data Position=Position {x::FCT.CFloat,y::FCT.CFloat} deriving (Eq)

data GameState=GameState{snake::[Position],direction::Direction,next_direction::Direction,food::Position,segment_count::Int}

main::IO ()
main=do
    init_engine
    engine<-create_engine (GameState {snake=[Position {x=0,y=0},Position {x=0,y=40},Position {x=0,y=80}],direction=DUp,next_direction=DUp,food=Position {x=120,y= -120},segment_count=3}) (\_ _->Just 101) (\_ _->Object_strategy) mebibyte mebibyte mebibyte 1 1 0 (Just 100000000) 0 2 1024 1024
    let engine_ready=foldl (flip create_request) engine ([Create_window 1 (DT.pack "Snake Engine Demo") 800 600 0.1 0.1 0.1 1.0 (DSet.singleton Window_resizable),Create_widget 200 Nothing (Collector_request 0 1000),Create_node 100 Nothing (\_ e->e) (\_ w->w),Create_widget 101 (Just 100) (Trigger_request logic_trigger (\_ _->Nothing)),Create_node 300 (Just 100) (\_ e->e) transform_food,Create_widget 301 (Just 300) (Visual_request 1.0 0.2 0.2 1.0 (identity_matrix 0 0) (Convex_polygon_request (DS.fromList [Point {x= -15,y= -15},Point {x=15,y= -15},Point {x=15,y=15},Point {x= -15,y=15}])))]++concat [create_segment_requests i|i<-[0..2]])
    run_engine engine_ready
    clean_engine engine_ready
    quit_engine

create_segment_requests::Int->[Request GameState]
create_segment_requests i=let (r,g,b)=if i==0 then (0.2,0.5,0.9) else (0.2,0.8,0.2) in [Create_node (1000+i) (Just 100) (\_ e->e) (transform_segment i),Create_widget (2000+i) (Just (1000+i)) (Visual_request r g b 1.0 (identity_matrix 0 0) (Convex_polygon_request (DS.fromList [Point {x= -18,y= -18},Point {x=18,y= -18},Point {x=18,y=18},Point {x= -18,y=18}])))]

transform_segment::Int->Engine GameState->Widget GameState->Widget GameState
transform_segment i engine widget=case widget of
    Visual {red,green,blue,alpha,matrix,visual}->let st=engine.state in if i<length st.snake
        then let Position {x=px,y=py}=st.snake!!i in case visual of
            Convex_polygon {}->Visual {red=red,green=green,blue=blue,alpha=alpha,matrix=matrix,visual=Convex_polygon {point=DS.fromList [Point {x=px-18,y=py-18},Point {x=px+18,y=py-18},Point {x=px+18,y=py+18},Point {x=px-18,y=py+18}]}}
            _->Visual {red=red,green=green,blue=blue,alpha=alpha,matrix=matrix,visual=visual}
        else widget
    _->widget

transform_food::Engine GameState->Widget GameState->Widget GameState
transform_food engine widget=case widget of
    Visual {red,green,blue,alpha,matrix,visual}->let Position {x=px,y=py}=engine.state.food in case visual of
        Convex_polygon {}->Visual {red=red,green=green,blue=blue,alpha=alpha,matrix=matrix,visual=Convex_polygon {point=DS.fromList [Point {x=px-15,y=py-15},Point {x=px+15,y=py-15},Point {x=px+15,y=py+15},Point {x=px-15,y=py+15}]}}
        _->Visual {red=red,green=green,blue=blue,alpha=alpha,matrix=matrix,visual=visual}
    _->widget

logic_trigger::Event->Engine GameState->Engine GameState
logic_trigger event engine=case event of
    At {action}->case action of
        Press {press=Press_down,change}->let st=engine.state in case change of
            Key_w->engine {state=st {next_direction=if st.direction/=DDown then DUp else st.next_direction}}
            Key_s->engine {state=st {next_direction=if st.direction/=DUp then DDown else st.next_direction}}
            Key_a->engine {state=st {next_direction=if st.direction/=DRight then DLeft else st.next_direction}}
            Key_d->engine {state=st {next_direction=if st.direction/=DLeft then DRight else st.next_direction}}
            _->engine {state=st {next_direction=st.next_direction}}
        _->engine
    Time {}->let st=engine.state in let h=head st.snake in let dir=st.next_direction in let dx=case dir of DDown->0;DUp->0;DLeft->(-40);DRight->40 in let dy=case dir of DDown->(-40);DUp->40;DLeft->0;DRight->0 in let new_h=Position {x=h.x+dx,y=h.y+dy} in let new_h_wrap=Position {x=if new_h.x>360 then -360 else if new_h.x< -360 then 360 else new_h.x,y=if new_h.y>280 then -280 else if new_h.y< -280 then 280 else new_h.y} in let ate_food=any (\pos->abs (pos.x-st.food.x)<1&&abs (pos.y-st.food.y)<1) (new_h_wrap:st.snake) in if any (\pos->abs (new_h_wrap.x-pos.x)<1&&abs (new_h_wrap.y-pos.y)<1) (if ate_food then st.snake else init st.snake) then render_frame (foldl (flip create_request) (engine {state=GameState {snake=[Position {x=0,y=0},Position {x=0,y=40},Position {x=0,y=80}],direction=DUp,next_direction=DUp,food=Position {x=120,y= -120},segment_count=3}}) (concat [[Remove_widget (2000+i) False,Remove_node (1000+i)]|i<-[3..st.segment_count-1]])) else let engine_moved=engine {state=st {snake=if ate_food then new_h_wrap:st.snake else new_h_wrap:init st.snake,direction=dir,food=if ate_food then Position {x=fromIntegral ((engine.count*17`mod`18)*40-360),y=fromIntegral ((engine.count*23`mod`14)*40-280)} else st.food,segment_count=if ate_food then st.segment_count+1 else st.segment_count}} in render_frame (if ate_food then foldl (flip create_request) (engine {state=st {snake=if ate_food then new_h_wrap:st.snake else new_h_wrap:init st.snake,direction=dir,food=if ate_food then Position {x=fromIntegral ((engine.count*17`mod`18)*40-360),y=fromIntegral ((engine.count*23`mod`14)*40-280)} else st.food,segment_count=if ate_food then st.segment_count+1 else st.segment_count}}) (create_segment_requests st.segment_count) else engine_moved)
    _->engine

render_frame::Engine GameState->Engine GameState
render_frame engine=let st=engine.state in let ids_to_update=filter (`DIM.member` engine.inactive) (301:[2000..2000+st.segment_count-1]) in let e1=foldl (flip (create_image_safe False)) engine ids_to_update in foldl (flip (create_image_safe False)) (create_request (Render 1 (Object_move False 200)) (foldl (\e (idx,vid)->collect (Image_safe_path vid) 200 (Index_collect_strategy idx) e) (e1 {inactive=intmap_update 200 (update_inactive_projection (update_object consume_widget)) e1.inactive}) (zip [1..] ids_to_update))) ids_to_update