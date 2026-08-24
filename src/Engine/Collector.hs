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
import qualified Data.Vector as DV
import qualified Data.Vector.Storable as DVS
import qualified Data.Word as DW
import qualified Foreign.C.Types as FCT

clean_collect::ET.Has_call_stack=>Int->Selector a->Engine b c d e f->Engine b c d e f
clean_collect leaf_id selector engine=engine {leaf=int_map_update leaf_id (update_projection_object (selector_update (const clean_collect_a) selector)) engine.leaf}

clean_collect_a::ET.Has_call_stack=>Widget a b c d e->Widget a b c d e
clean_collect_a widget=case widget of
    Collector {initial_min_index,initial_max_index}->Collector {initial_min_index=initial_min_index,min_index=initial_min_index,initial_max_index=initial_max_index,max_index=initial_max_index,submit=DIM.empty}
    _->widget

collect_canvas::ET.Has_call_stack=>Arrange->Maybe (Border FCT.CFloat)->Int->Int->Selector a->Insert_strategy->Engine b c d e f->Engine b c d e f
collect_canvas arrange maybe_border canvas_id leaf_id selector collect_strategy engine=case int_map_lookup canvas_id engine.canvas of
    Free_canvas {half_width,half_height}->case arrange of
        Arrange {point,matrix,color}->case point of
            Point {x,y}->engine {leaf=int_map_update leaf_id (update_projection_object (selector_update (const (collect_a (DS.singleton (Submit {submit_mode=Submit_canvas {canvas_id=canvas_id},vertex=quick_create_rectangle_vertex color (x-half_width) (y-half_height) (x+half_width) (y+half_height) 0 0 1 1,index=quick_create_rectangle_index,parameter=to_Parameter x y matrix maybe_border,vertex_size=4,index_size=6})) collect_strategy)) selector)) engine.leaf}
    _->EF.empty_error

maybe_update_collect::ET.Has_call_stack=>Custom_widget d=>(Widget a b c d e->Maybe (Widget a b c d e))->(Widget a b c d e->Widget a b c d e)->Maybe (Border FCT.CFloat)->Projection_path->Int->Selector f->Insert_strategy->Engine a b c d e->Engine a b c d e
maybe_update_collect update view maybe_border projection_path leaf_id selector collect_strategy engine=case DFC.getCompose (functor_lookup_projection_widget projection_path (\widget->DFC.Compose {getCompose=fmap (\this_widget->(to_collect engine.u engine.v maybe_border (view this_widget),this_widget)) (selector_monad_update (const update) selector widget)}) engine) of
    Nothing->engine
    Just (submit,new_engine)->new_engine {leaf=int_map_update leaf_id (update_projection_object (collect_a submit collect_strategy)) new_engine.leaf}

maybe_collect_update::ET.Has_call_stack=>Custom_widget d=>(Widget a b c d e->Maybe (Widget a b c d e))->(Widget a b c d e->Widget a b c d e)->Maybe (Border FCT.CFloat)->Projection_path->Int->Selector f->Insert_strategy->Engine a b c d e->Engine a b c d e
maybe_collect_update update view maybe_border projection_path leaf_id selector collect_strategy engine=let (new_update,maybe_engine)=DFC.getCompose (functor_lookup_projection_widget projection_path (\widget->DFC.Compose {getCompose=(int_map_update leaf_id (update_projection_object (collect_a (to_collect engine.u engine.v maybe_border (view widget)) collect_strategy)),selector_monad_update (const update) selector widget)}) engine) in case maybe_engine of
    Nothing->engine
    Just new_engine->new_engine {leaf=new_update new_engine.leaf}

collect::ET.Has_call_stack=>Custom_widget d=>(Widget a b c d e->Widget a b c d e)->Maybe (Border FCT.CFloat)->Projection_path->Int->Selector f->Insert_strategy->Engine a b c d e->Engine a b c d e
collect view maybe_border projection_path leaf_id selector collect_strategy engine=engine {leaf=int_map_update leaf_id (update_projection_object (selector_update (const (collect_a (to_collect engine.u engine.v maybe_border (view (lookup_projection_widget projection_path engine))) collect_strategy)) selector)) engine.leaf}

collect_a::ET.Has_call_stack=>DS.Seq Submit->Insert_strategy->Widget a b c d e->Widget a b c d e
collect_a this_submit collect_strategy widget=case widget of
    Collector {initial_min_index,min_index,initial_max_index,max_index,submit}->case collect_strategy of
        Min_strategy->Collector {initial_min_index=initial_min_index,min_index=min_index-1,initial_max_index=initial_max_index,max_index=max_index,submit=int_map_insert min_index this_submit submit}
        Max_strategy->Collector {initial_min_index=initial_min_index,min_index=min_index,initial_max_index=initial_max_index,max_index=max_index+1,submit=int_map_insert max_index this_submit submit}
        Index_strategy {seat}->if seat<=min_index then Collector {initial_min_index=initial_min_index,min_index=seat-1,initial_max_index=initial_max_index,max_index=max_index,submit=int_map_insert seat this_submit submit} else if max_index<=seat then Collector {initial_min_index=initial_min_index,min_index=min_index,initial_max_index=initial_max_index,max_index=seat+1,submit=int_map_insert seat this_submit submit} else Collector {initial_min_index=initial_min_index,min_index=min_index,initial_max_index=initial_max_index,max_index=max_index,submit=int_map_insert seat this_submit submit}
    _->EF.empty_error

to_collect::ET.Has_call_stack=>Custom_widget d=>FCT.CFloat->FCT.CFloat->Maybe (Border FCT.CFloat)->Widget a b c d e->DS.Seq Submit
to_collect u v maybe_border widget=case widget of
    Visual {visual}->DS.singleton (to_collect_visual id u v maybe_border visual)
    Group_visual {arrange,collect_order,group_visual}->fmap (\index->to_collect_visual (combine_arrange arrange) u v maybe_border (int_map_lookup index group_visual)) collect_order
    Vector_visual {arrange,collect_order,vector_visual}->fmap (\index->to_collect_visual (combine_arrange arrange) u v maybe_border (vector_visual DV.! index)) collect_order
    Custom_widget {custom}->custom_widget_collect u v maybe_border custom
    _->EF.empty_error

to_collect_visual::ET.Has_call_stack=>(Arrange->Arrange)->FCT.CFloat->FCT.CFloat->Maybe (Border FCT.CFloat)->Visual->Submit
to_collect_visual arrange_transform u v maybe_border visual=case visual of
    Rectangle {arrange,half_width,half_height}->case arrange_transform arrange of
        Arrange {point,matrix,color}->case point of
            Point {x,y}->Submit {submit_mode=Submit_default,vertex=quick_create_rectangle_vertex color (x-half_width) (y-half_height) (x+half_width) (y+half_height) u v u v,index=quick_create_rectangle_index,parameter=to_Parameter x y matrix maybe_border,vertex_size=4,index_size=6}
    Triangle {arrange,first_point,second_point,third_point}->case arrange_transform arrange of
        Arrange {point,matrix,color}->case point of
            Point {x,y}->Submit {submit_mode=Submit_default,vertex=DS.singleton (quick_create_vertex color (x+first_point.x) (y+first_point.y) u v) DS.|> quick_create_vertex color (x+second_point.x) (y+second_point.y) u v DS.|> quick_create_vertex color (x+third_point.x) (y+third_point.y) u v,index=DS.singleton 0 DS.|> 1 DS.|> 2,parameter=to_Parameter x y matrix maybe_border,vertex_size=3,index_size=3}
    Convex_polygon {arrange,point_set}->case arrange_transform arrange of
        Arrange {point,matrix,color}->case point of
            Point {x,y}->let number=DS.length point_set in if number<3 then EF.empty_error else let new_number=3*(number-2) in Submit {submit_mode=Submit_default,vertex=fmap (\this_point->quick_create_vertex color (x+this_point.x) (y+this_point.y) u v) point_set,index=DS.fromFunction new_number collect_convex_polygon,parameter=to_Parameter x y matrix maybe_border,vertex_size=fromIntegral number,index_size=fromIntegral new_number}
    Regular_polygon {arrange,number,radius,angle}->case arrange_transform arrange of
        Arrange {point,matrix,color}->case point of
            Point {x,y}->if number<3 then EF.empty_error else let new_number=3*(number-2) in let new_angle=2*pi/fromIntegral number in Submit {submit_mode=Submit_default,vertex=DS.fromFunction number (\index->let direction=angle+fromIntegral index*new_angle in quick_create_vertex color (x+radius*cos direction) (y+radius*sin direction) u v),index=DS.fromFunction new_number collect_convex_polygon,parameter=to_Parameter x y matrix maybe_border,vertex_size=fromIntegral number,index_size=fromIntegral new_number}
    Picture {arrange,half_width,half_height,min_u,min_v,max_u,max_v,locked}->if locked then EF.empty_error else case arrange_transform arrange of
        Arrange {point,matrix,color}->case point of
            Point {x,y}->Submit {submit_mode=Submit_default,vertex=quick_create_rectangle_vertex color (x-half_width) (y-half_height) (x+half_width) (y+half_height) min_u min_v max_u max_v,index=quick_create_rectangle_index,parameter=to_Parameter x y matrix maybe_border,vertex_size=4,index_size=6}
    Large_picture {arrange,half_width,half_height,album_id}->case arrange_transform arrange of
        Arrange {point,matrix,color}->case point of
            Point {x,y}->Submit {submit_mode=Submit_album {album_id=album_id},vertex=quick_create_rectangle_vertex color (x-half_width) (y-half_height) (x+half_width) (y+half_height) 0 0 1 1,index=quick_create_rectangle_index,parameter=to_Parameter x y matrix maybe_border,vertex_size=4,index_size=6}
    Atlas {arrange,clip,index,locked}->if locked then EF.empty_error else case arrange_transform arrange of
        Arrange {point,matrix,color}->case move_clip point (clip DVS.! index) of
            Clip {x,y,half_width,half_height,min_u,min_v,max_u,max_v}->Submit {submit_mode=Submit_default,vertex=quick_create_rectangle_vertex color (x-half_width) (y-half_height) (x+half_width) (y+half_height) min_u min_v max_u max_v,index=quick_create_rectangle_index,parameter=to_Parameter point.x point.y matrix maybe_border,vertex_size=4,index_size=6}
    Large_atlas {arrange,clip,album_id,index}->case arrange_transform arrange of
        Arrange {point,matrix,color}->case move_clip point (clip DVS.! index) of
            Clip {x,y,half_width,half_height,min_u,min_v,max_u,max_v}->Submit {submit_mode=Submit_album {album_id=album_id},vertex=quick_create_rectangle_vertex color (x-half_width) (y-half_height) (x+half_width) (y+half_height) min_u min_v max_u max_v,index=quick_create_rectangle_index,parameter=to_Parameter point.x point.y matrix maybe_border,vertex_size=4,index_size=6}
    Animation {arrange,half_width,half_height,padding,exponent_width,exponent_height,width_number,height_number,album_number,album_id,index}->case arrange_transform arrange of
        Arrange {point,matrix,color}->case point of
            Point {x,y}->let (quotient,remainder)=divMod index (width_number*height_number) in let new_album_id=album_id+quotient in if album_number<=quotient then EF.empty_error else let (new_quotient,new_remainder)=divMod remainder width_number in let frame_x=2*fromIntegral new_remainder*(half_width+padding)+padding in let frame_y=2*fromIntegral new_quotient*(half_height+padding)+padding in Submit {submit_mode=Submit_album {album_id=new_album_id},vertex=quick_create_rectangle_vertex color (x-half_width) (y-half_height) (x+half_width) (y+half_height) (scaleFloat (-exponent_width) frame_x) (scaleFloat (-exponent_height) frame_y) (scaleFloat (-exponent_width) (frame_x+2*half_width)) (scaleFloat (-exponent_height) (frame_y+2*half_height)),index=quick_create_rectangle_index,parameter=to_Parameter x y matrix maybe_border,vertex_size=4,index_size=6}
    Text {arrange,half_width,half_height,current_y,article,locked}->if locked then EF.empty_error else case arrange_transform arrange of
        Arrange {point,matrix,color}->case point of
            Point {x,y}->case maybe_border of
                Nothing->let (vertex,index,_)=DF.foldl' (DF.foldl' (flip (collect_text color x y current_y))) (DS.empty,DS.empty,0) (fmap (DS.takeWhileL (end_text current_y half_height) . DS.dropWhileL (begin_text current_y half_height)) article) in Submit {submit_mode=Submit_default,vertex=vertex,index=index,parameter=Parameter {x=x+matrix.x,y=y+matrix.y,x_x=matrix.x_x,x_y=matrix.x_y,y_x=matrix.y_x,y_y=matrix.y_y,border_flag=1,border_left=x-half_width,border_down=y-half_height,border_right=x+half_width,border_up=y+half_height},vertex_size=fromIntegral (DS.length vertex),index_size=fromIntegral (DS.length index)}
                Just border->case border of
                    Border {left,down,right,up}->let border_down=max (-half_height) down in let border_up=min half_height up in let (vertex,index,_)=DF.foldl' (DF.foldl' (flip (collect_text color x y current_y))) (DS.empty,DS.empty,0) (fmap (DS.takeWhileL (end_text current_y (-border_down)) . DS.dropWhileL (begin_text current_y border_up)) article) in Submit {submit_mode=Submit_default,vertex=vertex,index=index,parameter=Parameter {x=x+matrix.x,y=y+matrix.y,x_x=matrix.x_x,x_y=matrix.x_y,y_x=matrix.y_x,y_y=matrix.y_y,border_flag=1,border_left=x+max (-half_width) left,border_down=y+border_down,border_right=x+min half_width right,border_up=y+border_up},vertex_size=fromIntegral (DS.length vertex),index_size=fromIntegral (DS.length index)}
    Canvas {arrange,half_width,half_height,canvas_id}->case arrange_transform arrange of
        Arrange {point,matrix,color}->case point of
            Point {x,y}->Submit {submit_mode=Submit_canvas {canvas_id=canvas_id},vertex=quick_create_rectangle_vertex color (x-half_width) (y-half_height) (x+half_width) (y+half_height) 0 0 1 1,index=quick_create_rectangle_index,parameter=to_Parameter x y matrix maybe_border,vertex_size=4,index_size=6}

to_Parameter::ET.Has_call_stack=>FCT.CFloat->FCT.CFloat->Matrix->Maybe (Border FCT.CFloat)->Parameter
to_Parameter x y matrix maybe_border=case maybe_border of
    Nothing->Parameter {x=x+matrix.x,y=y+matrix.y,x_x=matrix.x_x,x_y=matrix.x_y,y_x=matrix.y_x,y_y=matrix.y_y,border_flag=0,border_left=0,border_down=0,border_right=0,border_up=0}
    Just border->case border of
        Border {left,down,right,up}->Parameter {x=x+matrix.x,y=y+matrix.y,x_x=matrix.x_x,x_y=matrix.x_y,y_x=matrix.y_x,y_y=matrix.y_y,border_flag=1,border_left=x+left,border_down=y+down,border_right=x+right,border_up=y+up}

begin_text::ET.Has_call_stack=>FCT.CFloat->FCT.CFloat->Row->Bool
begin_text this_y height row=case row of
    Blank->True
    Row {y,min_down}->y+height<this_y+min_down

end_text::ET.Has_call_stack=>FCT.CFloat->FCT.CFloat->Row->Bool
end_text this_y height row=case row of
    Blank->True
    Row {y,max_up}->y<=this_y+max_up+height

collect_convex_polygon::ET.Has_call_stack=>Int->DW.Word32
collect_convex_polygon index=let (quotient,remainder)=divMod index 3 in let new_quotient=fromIntegral quotient+1 in case remainder of
    0->0
    1->new_quotient
    2->new_quotient+1
    _->EF.empty_error

collect_text::ET.Has_call_stack=>Color->FCT.CFloat->FCT.CFloat->FCT.CFloat->Row->(DS.Seq Vertex,DS.Seq DW.Word32,DW.Word32)->(DS.Seq Vertex,DS.Seq DW.Word32,DW.Word32)
collect_text color origin_x origin_y this_y row primitive=case row of
    Blank->primitive
    Row {row_core,x,y}->case color of
        Color {red,green,blue,alpha}->DF.foldl' (flip (collect_character red green blue alpha (origin_x+x) (origin_y+this_y-y))) primitive row_core

collect_character::ET.Has_call_stack=>FCT.CFloat->FCT.CFloat->FCT.CFloat->FCT.CFloat->FCT.CFloat->FCT.CFloat->Character->(DS.Seq Vertex,DS.Seq DW.Word32,DW.Word32)->(DS.Seq Vertex,DS.Seq DW.Word32,DW.Word32)
collect_character this_red this_green this_blue this_alpha x y character (vertex,index,index_offset)=case character of
    Character {font_size,left,down,right,up,min_u,min_v,max_u,max_v,color}->case color of
        Color {red,green,blue,alpha}->(quick_create_rectangle_text_vertex (this_red*red) (this_green*green) (this_blue*blue) (this_alpha*alpha) (x+left) (y+down) (x+right) (y+up) min_u min_v max_u max_v font_size vertex,index DS.|> index_offset DS.|> (index_offset+1) DS.|> (index_offset+2) DS.|> index_offset DS.|> (index_offset+2) DS.|> (index_offset+3),index_offset+4)

move::ET.Has_call_stack=>Projection_move->Int->Selector (Selector Insert_strategy)->Engine a b c d e->Engine a b c d e
move projection_move leaf_id selector engine=let (new_engine,widget)=move_lookup projection_move engine in selector_action (move_a leaf_id) selector widget new_engine

move_a::ET.Has_call_stack=>Int->Selector Insert_strategy->Widget a b c d e->Engine a b c d e->Engine a b c d e
move_a leaf_id selector widget engine=case widget of
    Collector {submit}->engine {leaf=int_map_update leaf_id (update_projection_object (selector_update (collect_a (DF.foldl' (DS.><) DS.empty submit)) selector)) engine.leaf}
    _->EF.empty_error

move_lookup::ET.Has_call_stack=>Projection_move->Engine a b c d e->(Engine a b c d e,Widget a b c d e)
move_lookup projection_move engine=case projection_move of
    Object_move {leaf_id,consume}->if consume then let (widget,leaf)=int_map_functor_update leaf_id (DT.swap . update_lookup_projection_widget_a (default_selector_update True consume_widget)) engine.leaf in (engine {leaf=leaf},widget) else (engine,lookup_projection_object (int_map_lookup leaf_id engine.leaf))
    Image_move {leaf_id}->(engine,lookup_projection_image (int_map_lookup leaf_id engine.leaf))
    Image_safe_move {leaf_id}->(engine,lookup_projection_image_safe (int_map_lookup leaf_id engine.leaf))

consume_widget::ET.Has_call_stack=>Widget a b c d e->Widget a b c d e
consume_widget widget=case widget of
    Collector {initial_min_index,initial_max_index}->Collector {initial_min_index=initial_min_index,min_index=initial_min_index,initial_max_index=initial_max_index,max_index=initial_max_index,submit=DIM.empty}
    _->EF.empty_error

for_submit::ET.Has_call_stack=>DIM.IntMap (DS.Seq Submit)->(DS.Seq Vertex,DS.Seq DW.Word32,DS.Seq Parameter,DS.Seq (Submit_mode,DW.Word32,DW.Word32))
for_submit submit=let (vertex,index,parameter,draw_call,_,_,_)=DIM.foldl' (DF.foldl' (flip for_submit_a)) (DS.empty,DS.empty,DS.empty,DS.empty,0,0,0) submit in (vertex,index,parameter,draw_call)

for_submit_a::ET.Has_call_stack=>Submit->(DS.Seq Vertex,DS.Seq DW.Word32,DS.Seq Parameter,DS.Seq (Submit_mode,DW.Word32,DW.Word32),DW.Word32,DW.Word32,FCT.CFloat)->(DS.Seq Vertex,DS.Seq DW.Word32,DS.Seq Parameter,DS.Seq (Submit_mode,DW.Word32,DW.Word32),DW.Word32,DW.Word32,FCT.CFloat)
for_submit_a submit (this_vertex,this_index,this_parameter,draw_call,vertex_offset,index_offset,parameter_id)=case submit of
    Submit {submit_mode,vertex,index,parameter,vertex_size,index_size}->(this_vertex DS.>< fmap (\single_vertex->single_vertex {parameter_id=parameter_id}) vertex,this_index DS.>< fmap (vertex_offset+) index,this_parameter DS.|> parameter,for_submit_b submit_mode index_size index_offset draw_call,vertex_offset+vertex_size,index_offset+index_size,parameter_id+1)

for_submit_b::ET.Has_call_stack=>Submit_mode->DW.Word32->DW.Word32->DS.Seq (Submit_mode,DW.Word32,DW.Word32)->DS.Seq (Submit_mode,DW.Word32,DW.Word32)
for_submit_b submit_mode index_size index_offset draw_call=case draw_call of
    DS.Empty->DS.singleton (submit_mode,index_size,index_offset)
    other_draw_call DS.:|> (new_submit_mode,new_index_size,new_index_offset)->if submit_mode==new_submit_mode then other_draw_call DS.|> (submit_mode,index_size+new_index_size,new_index_offset) else draw_call DS.|> (submit_mode,index_size,index_offset)

get_submit::ET.Has_call_stack=>Selector ()->Widget a b c d e->DIM.IntMap (DS.Seq Submit)
get_submit selector widget=selector_action (\_ this_widget submit->get_submit_a this_widget submit) selector widget DIM.empty

get_submit_a::ET.Has_call_stack=>Widget a b c d e->DIM.IntMap (DS.Seq Submit)->DIM.IntMap (DS.Seq Submit)
get_submit_a widget this_submit=case widget of
    Collector {submit}->DIM.unionWith (DS.><) this_submit submit
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
{-# INLINE to_Parameter #-}
{-# INLINE begin_text #-}
{-# INLINE end_text #-}
{-# INLINE collect_convex_polygon #-}
{-# INLINE collect_text #-}
{-# INLINE collect_character #-}
{-# INLINE move #-}
{-# INLINE move_a #-}
{-# INLINE move_lookup #-}
{-# INLINE consume_widget #-}
{-# INLINE for_submit #-}
{-# INLINE for_submit_a #-}
{-# INLINE for_submit_b #-}
{-# INLINE get_submit #-}
{-# INLINE get_submit_a #-}