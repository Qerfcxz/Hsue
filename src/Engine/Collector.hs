{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Engine.Collector where

import Engine.Container
import Engine.Projection
import Engine.Selector
import Engine.Type
import Engine.Underlying
import qualified Error.Function as EF
import qualified Error.Type as ET
import qualified Data.Foldable as DF
import qualified Data.Functor.Compose as DFC
import qualified Data.IntMap as DIM
import qualified Data.Sequence as DS
import qualified Data.Tuple as DT
import qualified Data.Vector.Storable as DVS
import qualified Foreign.C.Types as FCT

clean_collect::ET.Has_call_stack=>Int->Selector a->Engine b->Engine b
clean_collect leaf_id selector engine=engine {leaf=int_map_update leaf_id (update_projection_object (selector_update (const clean_collect_a) selector)) engine.leaf}

clean_collect_a::ET.Has_call_stack=>Widget a->Widget a
clean_collect_a widget=case widget of
    Collector {initial_min_index,initial_max_index}->Collector {initial_min_index=initial_min_index,min_index=initial_min_index,initial_max_index=initial_max_index,max_index=initial_max_index,submit=DIM.empty}
    _->widget

collect_canvas::ET.Has_call_stack=>Arrange->Maybe (Border FCT.CFloat)->Int->Int->Selector a->Insert_strategy->Engine b->Engine b
collect_canvas arrange maybe_border canvas_id leaf_id selector collect_strategy engine=case int_map_lookup canvas_id engine.canvas of
    Free_canvas {half_width,half_height}->engine {leaf=int_map_update leaf_id (update_projection_object (selector_update (const (collect_a (DS.singleton (create_submit_rectangle (Submit_canvas {canvas_id=canvas_id}) maybe_border arrange half_width half_height 0 0 1 1)) collect_strategy)) selector)) engine.leaf}
    _->EF.empty_error

maybe_update_collect::ET.Has_call_stack=>Custom a=>(Widget a->Maybe (Widget a))->(Widget a->Widget a)->Maybe (Border FCT.CFloat)->Projection_path->Int->Selector b->Visual_selector c->Insert_strategy->Engine a->Engine a
maybe_update_collect update view maybe_border projection_path leaf_id selector visual_selector collect_strategy engine=case DFC.getCompose (functor_lookup_projection_widget projection_path (\widget->DFC.Compose {getCompose=fmap (\this_widget->(to_collect visual_selector engine.u engine.v maybe_border (view this_widget),this_widget)) (selector_monad_update (const update) selector widget)}) engine) of
    Nothing->engine
    Just (submit,new_engine)->new_engine {leaf=int_map_update leaf_id (update_projection_object (collect_a submit collect_strategy)) new_engine.leaf}

maybe_collect_update::ET.Has_call_stack=>Custom a=>(Widget a->Maybe (Widget a))->(Widget a->Widget a)->Maybe (Border FCT.CFloat)->Projection_path->Int->Selector b->Visual_selector c->Insert_strategy->Engine a->Engine a
maybe_collect_update update view maybe_border projection_path leaf_id selector visual_selector collect_strategy engine=let (new_update,maybe_engine)=DFC.getCompose (functor_lookup_projection_widget projection_path (\widget->DFC.Compose {getCompose=(int_map_update leaf_id (update_projection_object (collect_a (to_collect visual_selector engine.u engine.v maybe_border (view widget)) collect_strategy)),selector_monad_update (const update) selector widget)}) engine) in case maybe_engine of
    Nothing->engine
    Just new_engine->new_engine {leaf=new_update new_engine.leaf}

collect::ET.Has_call_stack=>Custom a=>(Widget a->Widget a)->Maybe (Border FCT.CFloat)->Projection_path->Int->Selector b->Visual_selector c->Insert_strategy->Engine a->Engine a
collect view maybe_border projection_path leaf_id selector visual_selector collect_strategy engine=engine {leaf=int_map_update leaf_id (update_projection_object (selector_update (const (collect_a (to_collect visual_selector engine.u engine.v maybe_border (view (lookup_projection_widget projection_path engine))) collect_strategy)) selector)) engine.leaf}

collect_a::ET.Has_call_stack=>DS.Seq (Submit a)->Insert_strategy->Widget a->Widget a
collect_a this_submit collect_strategy widget=case widget of
    Collector {initial_min_index,min_index,initial_max_index,max_index,submit}->case collect_strategy of
        Min_strategy->Collector {initial_min_index=initial_min_index,min_index=min_index-1,initial_max_index=initial_max_index,max_index=max_index,submit=int_map_insert min_index this_submit submit}
        Max_strategy->Collector {initial_min_index=initial_min_index,min_index=min_index,initial_max_index=initial_max_index,max_index=max_index+1,submit=int_map_insert max_index this_submit submit}
        Index_strategy {seat}->if seat<=min_index then Collector {initial_min_index=initial_min_index,min_index=seat-1,initial_max_index=initial_max_index,max_index=max_index,submit=int_map_insert seat this_submit submit} else if max_index<=seat then Collector {initial_min_index=initial_min_index,min_index=min_index,initial_max_index=initial_max_index,max_index=seat+1,submit=int_map_insert seat this_submit submit} else Collector {initial_min_index=initial_min_index,min_index=min_index,initial_max_index=initial_max_index,max_index=max_index,submit=int_map_insert seat this_submit submit}
    _->EF.empty_error

to_collect::ET.Has_call_stack=>Custom b=>Visual_selector a->FCT.CFloat->FCT.CFloat->Maybe (Border FCT.CFloat)->Widget b->DS.Seq (Submit b)
to_collect visual_selector u v maybe_border widget=visual_selector_action (const (\arrange visual submit->submit DS.|> to_collect_visual arrange u v maybe_border visual)) visual_selector widget DS.empty

to_collect_visual::ET.Has_call_stack=>Custom a=>Arrange->FCT.CFloat->FCT.CFloat->Maybe (Border FCT.CFloat)->Visual a->Submit a
to_collect_visual this_arrange u v maybe_border visual=case visual of
    Rectangle {arrange,half_width,half_height}->create_submit_rectangle Submit_default maybe_border (combine_arrange this_arrange arrange) half_width half_height u v u v
    Triangle {arrange,first_point,second_point,third_point}->create_submit_triangle Submit_default maybe_border (combine_arrange this_arrange arrange) first_point second_point third_point u v
    Convex_polygon {arrange,point_set}->create_submit_convex_polygon Submit_default maybe_border (combine_arrange this_arrange arrange) point_set u v
    Regular_polygon {arrange,number,radius,angle}->create_submit_regular_polygon Submit_default maybe_border (combine_arrange this_arrange arrange) number radius angle u v
    Picture {arrange,half_width,half_height,min_u,min_v,max_u,max_v,locked}->if locked then EF.empty_error else create_submit_rectangle Submit_default maybe_border (combine_arrange this_arrange arrange) half_width half_height min_u min_v max_u max_v
    Large_picture {arrange,half_width,half_height,album_id}->create_submit_rectangle (Submit_album {album_id=album_id}) maybe_border (combine_arrange this_arrange arrange) half_width half_height 0 0 1 1
    Atlas {arrange,clip,index,locked}->if locked then EF.empty_error else create_submit_rectangle_clip Submit_default maybe_border (combine_arrange this_arrange arrange) (clip DVS.! index)
    Large_atlas {arrange,clip,index,album_id}->create_submit_rectangle_clip (Submit_album {album_id=album_id}) maybe_border (combine_arrange this_arrange arrange) (clip DVS.! index)
    Animation {arrange,half_width,half_height,padding,exponent_width,exponent_height,width_number,height_number,album_number,index,album_id}->let (quotient,remainder)=divMod index (width_number*height_number) in if album_number<=quotient then EF.empty_error else let (new_quotient,new_remainder)=divMod remainder width_number in let frame_x=2*fromIntegral new_remainder*(half_width+padding)+padding in let frame_y=2*fromIntegral new_quotient*(half_height+padding)+padding in create_submit_rectangle (Submit_album {album_id=album_id+quotient}) maybe_border (combine_arrange this_arrange arrange) half_width half_height (scaleFloat (negate exponent_width) frame_x) (scaleFloat (negate exponent_height) frame_y) (scaleFloat (negate exponent_width) (frame_x+2*half_width)) (scaleFloat (negate exponent_height) (frame_y+2*half_height))
    Text {arrange,half_width,half_height,current_y,anchor,article,locked}->if locked then EF.empty_error else create_submit_text Submit_default maybe_border (combine_arrange this_arrange arrange) half_width half_height current_y anchor article
    Editor {}->error "未完待续"
    Canvas {arrange,half_width,half_height,canvas_id}->create_submit_rectangle (Submit_canvas {canvas_id=canvas_id}) maybe_border (combine_arrange this_arrange arrange) half_width half_height 0 0 1 1
    Custom_visual {custom}->custom_visual_collect this_arrange u v maybe_border custom

create_submit_rectangle::ET.Has_call_stack=>Submit_mode->Maybe (Border FCT.CFloat)->Arrange->FCT.CFloat->FCT.CFloat->FCT.CFloat->FCT.CFloat->FCT.CFloat->FCT.CFloat->Submit a
create_submit_rectangle submit_mode maybe_border arrange half_width half_height min_u min_v max_u max_v=case arrange of
    Arrange {point,matrix,color}->case point of
        Point {x,y}->case color of
            Color {red,green,blue,alpha}->Submit {submit_mode=submit_mode,submit_data=Submit_rectangle {red=red,green=green,blue=blue,alpha=alpha,left=x-half_width,down=y-half_height,right=x+half_width,up=y+half_height,min_u=min_u,min_v=min_v,max_u=max_u,max_v=max_v},parameter=to_parameter x y matrix maybe_border,vertex_size=4,index_size=6}

create_submit_rectangle_clip::ET.Has_call_stack=>Submit_mode->Maybe (Border FCT.CFloat)->Arrange->Clip->Submit a
create_submit_rectangle_clip submit_mode maybe_border arrange clip=case arrange of
    Arrange {point,matrix,color}->case move_clip point clip of
        Clip {x,y,half_width,half_height,min_u,min_v,max_u,max_v}->case color of
            Color {red,green,blue,alpha}->Submit {submit_mode=submit_mode,submit_data=Submit_rectangle {red=red,green=green,blue=blue,alpha=alpha,left=x-half_width,down=y-half_height,right=x+half_width,up=y+half_height,min_u=min_u,min_v=min_v,max_u=max_u,max_v=max_v},parameter=to_parameter point.x point.y matrix maybe_border,vertex_size=4,index_size=6}

create_submit_triangle::ET.Has_call_stack=>Submit_mode->Maybe (Border FCT.CFloat)->Arrange->Point->Point->Point->FCT.CFloat->FCT.CFloat->Submit a
create_submit_triangle submit_mode maybe_border arrange first_point second_point third_point u v=case arrange of
    Arrange {point,matrix,color}->case point of
        Point {x,y}->case color of
            Color {red,green,blue,alpha}->case first_point of
                Point {x=first_x,y=first_y}->case second_point of
                    Point {x=second_x,y=second_y}->case third_point of
                        Point {x=third_x,y=third_y}->Submit {submit_mode=submit_mode,submit_data=Submit_triangle {red=red,green=green,blue=blue,alpha=alpha,first_x=x+first_x,first_y=y+first_y,second_x=x+second_x,second_y=y+second_y,third_x=x+third_x,third_y=y+third_y,u=u,v=v},parameter=to_parameter x y matrix maybe_border,vertex_size=3,index_size=3}

create_submit_convex_polygon::ET.Has_call_stack=>Submit_mode->Maybe (Border FCT.CFloat)->Arrange->DS.Seq Point->FCT.CFloat->FCT.CFloat->Submit a
create_submit_convex_polygon submit_mode maybe_border arrange point_set u v=case arrange of
    Arrange {point,matrix,color}->case point of
        Point {x,y}->case color of
            Color {red,green,blue,alpha}->let number=DS.length point_set in if number<3 then EF.empty_error else Submit {submit_mode=submit_mode,submit_data=Submit_convex_polygon {red=red,green=green,blue=blue,alpha=alpha,x=x,y=y,u=u,v=v,point_set=point_set},parameter=to_parameter x y matrix maybe_border,vertex_size=fromIntegral number,index_size=3*(fromIntegral number-2)}

create_submit_regular_polygon::ET.Has_call_stack=>Submit_mode->Maybe (Border FCT.CFloat)->Arrange->Int->FCT.CFloat->FCT.CFloat->FCT.CFloat->FCT.CFloat->Submit a
create_submit_regular_polygon submit_mode maybe_border arrange number radius angle u v=case arrange of
    Arrange {point,matrix,color}->case point of
        Point {x,y}->case color of
            Color {red,green,blue,alpha}->if number<3 then EF.empty_error else Submit {submit_mode=submit_mode,submit_data=Submit_regular_polygon {red=red,green=green,blue=blue,alpha=alpha,x=x,y=y,u=u,v=v,angle=angle,radius=radius,number=number},parameter=to_parameter x y matrix maybe_border,vertex_size=fromIntegral number,index_size=3*(fromIntegral number-2)}

create_submit_text::ET.Has_call_stack=>Submit_mode->Maybe (Border FCT.CFloat)->Arrange->FCT.CFloat->FCT.CFloat->FCT.CFloat->Anchor->DS.Seq (DS.Seq Row)->Submit a
create_submit_text submit_mode maybe_border arrange half_width half_height current_y anchor article=case arrange of
    Arrange {point,matrix,color}->case point of
        Point {x,y}->case color of
            Color {red,green,blue,alpha}->case anchor of
                Anchor {ratio,offset}->let (new_article,parameter)=to_parameter_text half_width half_height current_y x y matrix maybe_border article in let count=fromIntegral (DF.foldl' (DF.foldl' (\this_count row->this_count+DS.length row.row_core)) 0 new_article) in Submit {submit_mode=submit_mode,submit_data=Submit_text {red=red,green=green,blue=blue,alpha=alpha,x=x+offset,y=y,current_y=current_y,ratio=ratio,article=new_article},parameter=parameter,vertex_size=4*count,index_size=6*count}

to_parameter::ET.Has_call_stack=>FCT.CFloat->FCT.CFloat->Matrix->Maybe (Border FCT.CFloat)->Parameter
to_parameter this_x this_y matrix maybe_border=case matrix of
    Matrix {x,y,x_x,x_y,y_x,y_y}->case maybe_border of
        Nothing->Parameter {x=this_x+x,y=this_y+y,x_x=x_x,x_y=x_y,y_x=y_x,y_y=y_y,border_flag=0,border_left=0,border_down=0,border_right=0,border_up=0}
        Just border->case border of
            Border {left,down,right,up}->Parameter {x=this_x+x,y=this_y+y,x_x=x_x,x_y=x_y,y_x=y_x,y_y=y_y,border_flag=1,border_left=this_x+left,border_down=this_y+down,border_right=this_x+right,border_up=this_y+up}

to_parameter_text::ET.Has_call_stack=>FCT.CFloat->FCT.CFloat->FCT.CFloat->FCT.CFloat->FCT.CFloat->Matrix->Maybe (Border FCT.CFloat)->DS.Seq (DS.Seq Row)->(DS.Seq (DS.Seq Row),Parameter)
to_parameter_text half_width half_height current_y this_x this_y matrix maybe_border article=case matrix of
    Matrix {x,y,x_x,x_y,y_x,y_y}->case maybe_border of
        Nothing->(fmap (DS.takeWhileL (end_text current_y half_height) . DS.dropWhileL (start_text current_y half_height)) article,Parameter {x=this_x+x,y=this_y+y,x_x=x_x,x_y=x_y,y_x=y_x,y_y=y_y,border_flag=1,border_left=this_x-half_width,border_down=this_y-half_height,border_right=this_x+half_width,border_up=this_y+half_height})
        Just border->case border of
            Border {left,down,right,up}->let new_down=max (negate half_height) down in let new_up=min half_height up in (fmap (DS.takeWhileL (end_text current_y (negate new_down)) . DS.dropWhileL (start_text current_y new_up)) article,Parameter {x=this_x+x,y=this_y+y,x_x=x_x,x_y=x_y,y_x=y_x,y_y=y_y,border_flag=1,border_left=this_x+max (negate half_width) left,border_down=this_y+new_down,border_right=this_x+min half_width right,border_up=this_y+new_up})

start_text::ET.Has_call_stack=>FCT.CFloat->FCT.CFloat->Row->Bool
start_text this_y height row=case row of
    Row {y,min_down}->y+height<this_y+min_down

end_text::ET.Has_call_stack=>FCT.CFloat->FCT.CFloat->Row->Bool
end_text this_y height row=case row of
    Row {y,max_up}->y<=this_y+max_up+height

move::ET.Has_call_stack=>Projection_move->Int->Selector (Selector Insert_strategy)->Engine a->Engine a
move projection_move leaf_id selector engine=let (new_engine,widget)=move_lookup projection_move engine in selector_action (move_a leaf_id) selector widget new_engine

move_a::ET.Has_call_stack=>Int->Selector Insert_strategy->Widget a->Engine a->Engine a
move_a leaf_id selector widget engine=case widget of
    Collector {submit}->engine {leaf=int_map_update leaf_id (update_projection_object (selector_update (collect_a (DF.foldl' (DS.><) DS.empty submit)) selector)) engine.leaf}
    _->EF.empty_error

move_lookup::ET.Has_call_stack=>Projection_move->Engine a->(Engine a,Widget a)
move_lookup projection_move engine=case projection_move of
    Object_move {leaf_id,consume}->if consume then let (widget,leaf)=int_map_functor_update leaf_id (DT.swap . update_lookup_projection_widget_a (default_selector_update True consume_widget)) engine.leaf in (engine {leaf=leaf},widget) else (engine,lookup_projection_object (int_map_lookup leaf_id engine.leaf))
    Image_move {leaf_id}->(engine,lookup_projection_image (int_map_lookup leaf_id engine.leaf))
    Image_safe_move {leaf_id}->(engine,lookup_projection_image_safe (int_map_lookup leaf_id engine.leaf))

consume_widget::ET.Has_call_stack=>Widget a->Widget a
consume_widget widget=case widget of
    Collector {initial_min_index,initial_max_index}->Collector {initial_min_index=initial_min_index,min_index=initial_min_index,initial_max_index=initial_max_index,max_index=initial_max_index,submit=DIM.empty}
    _->EF.empty_error

{-# INLINE clean_collect #-}
{-# INLINE clean_collect_a #-}
{-# INLINE collect_canvas #-}
{-# INLINE maybe_update_collect #-}
{-# INLINE maybe_collect_update #-}
{-# INLINE collect #-}
{-# INLINE collect_a #-}
{-# INLINE to_collect #-}
{-# INLINE to_collect_visual #-}
{-# INLINE create_submit_rectangle #-}
{-# INLINE create_submit_rectangle_clip #-}
{-# INLINE create_submit_triangle #-}
{-# INLINE create_submit_convex_polygon #-}
{-# INLINE create_submit_regular_polygon #-}
{-# INLINE create_submit_text #-}
{-# INLINE to_parameter #-}
{-# INLINE to_parameter_text #-}
{-# INLINE start_text #-}
{-# INLINE end_text #-}
{-# INLINE move #-}
{-# INLINE move_a #-}
{-# INLINE move_lookup #-}
{-# INLINE consume_widget #-}