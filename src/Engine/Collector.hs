{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Engine.Collector where

import Engine.Container
import Engine.Helper
import Engine.Projection
import Engine.Type
import qualified Data.Foldable as DF
import qualified Data.Functor.Compose as DFC
import qualified Data.IntMap as DIM
import qualified Data.Sequence as DS
import qualified Data.Tuple as DT
import qualified Data.Word as DW
import qualified Foreign.C.Types as FCT

clean_collect::Int->Engine a->Engine a
clean_collect leaf_id engine=engine {leaf=intmap_update leaf_id (update_projection_object (update_all_widget clean_collect_a)) engine.leaf}

clean_collect_a::Widget a->Widget a
clean_collect_a widget=case widget of
    Collector {initial_min_index,initial_max_index}->Collector {initial_min_index=initial_min_index,min_index=initial_min_index,initial_max_index=initial_max_index,max_index=initial_max_index,submit=DIM.empty}
    _->widget

maybe_update_collect::(Widget a->Maybe (Widget a))->(Widget a->Widget a)->Projection_path->Int->Insert_strategy->Engine a->Engine a
maybe_update_collect update view projection_path leaf_id collect_strategy engine=let maybe_tuple=DFC.getCompose (functor_lookup_projection_widget projection_path (\widget->DFC.Compose {getCompose=fmap (\this_widget->(to_collect engine.u engine.v (view this_widget),this_widget)) (functor_update_widget update widget)}) engine) in case maybe_tuple of
    Nothing->engine
    Just (submit,new_engine)->new_engine {leaf=intmap_update leaf_id (update_projection_object (collect_a (DS.singleton submit) collect_strategy)) new_engine.leaf}

maybe_update_limited_collect::(Widget a->Maybe (Widget a))->(Widget a->Widget a)->Projection_path->Int->Int->Insert_strategy->Engine a->Engine a
maybe_update_limited_collect update view projection_path depth leaf_id collect_strategy engine=let maybe_tuple=DFC.getCompose (functor_lookup_projection_widget projection_path (\widget->DFC.Compose {getCompose=fmap (\this_widget->(to_collect engine.u engine.v (view this_widget),this_widget)) (functor_limited_update_widget depth update widget)}) engine) in case maybe_tuple of
    Nothing->engine
    Just (submit,new_engine)->new_engine {leaf=intmap_update leaf_id (update_projection_object (collect_a (DS.singleton submit) collect_strategy)) new_engine.leaf}

maybe_collect_update::(Widget a->Maybe (Widget a))->(Widget a->Widget a)->Projection_path->Int->Insert_strategy->Engine a->Engine a
maybe_collect_update update view projection_path leaf_id collect_strategy engine=let (new_update,maybe_engine)=DFC.getCompose (functor_lookup_projection_widget projection_path (\widget->DFC.Compose {getCompose=(intmap_update leaf_id (update_projection_object (collect_a (DS.singleton (to_collect engine.u engine.v (view widget))) collect_strategy)),functor_update_widget update widget)}) engine) in case maybe_engine of
    Nothing->engine
    Just new_engine->new_engine {leaf=new_update new_engine.leaf}

maybe_collect_limited_update::(Widget a->Maybe (Widget a))->(Widget a->Widget a)->Projection_path->Int->Int->Insert_strategy->Engine a->Engine a
maybe_collect_limited_update update view projection_path depth leaf_id collect_strategy engine=let (new_update,maybe_engine)=DFC.getCompose (functor_lookup_projection_widget projection_path (\widget->DFC.Compose {getCompose=(intmap_update leaf_id (update_projection_object (collect_a (DS.singleton (to_collect engine.u engine.v (view widget))) collect_strategy)),functor_limited_update_widget depth update widget)}) engine) in case maybe_engine of
    Nothing->engine
    Just new_engine->new_engine {leaf=new_update new_engine.leaf}

collect::(Widget a->Widget a)->Projection_path->Int->Insert_strategy->Engine a->Engine a
collect view projection_path leaf_id collect_strategy engine=engine {leaf=intmap_update leaf_id (update_projection_object (update_widget (collect_a (DS.singleton (to_collect engine.u engine.v (view (lookup_projection_widget projection_path engine)))) collect_strategy))) engine.leaf}

limited_collect::(Widget a->Widget a)->Projection_path->Int->Int->Insert_strategy->Engine a->Engine a
limited_collect view projection_path depth leaf_id collect_strategy engine=engine {leaf=intmap_update leaf_id (update_projection_object (limited_update_widget depth (collect_a (DS.singleton (to_collect engine.u engine.v (view (lookup_projection_widget projection_path engine)))) collect_strategy))) engine.leaf}

collect_a::DS.Seq Submit->Insert_strategy->Widget a->Widget a
collect_a this_submit collect_strategy widget=case widget of
    Collector {initial_min_index,min_index,initial_max_index,max_index,submit}->case collect_strategy of
        Min_strategy->Collector {initial_min_index=initial_min_index,min_index=min_index-1,initial_max_index=initial_max_index,max_index=max_index,submit=intmap_insert min_index this_submit submit}
        Max_strategy->Collector {initial_min_index=initial_min_index,min_index=min_index,initial_max_index=initial_max_index,max_index=max_index+1,submit=intmap_insert max_index this_submit submit}
        Index_strategy {seat}->if seat<=min_index then Collector {initial_min_index=initial_min_index,min_index=seat-1,initial_max_index=initial_max_index,max_index=max_index,submit=intmap_insert seat this_submit submit} else if max_index<=seat then Collector {initial_min_index=initial_min_index,min_index=min_index,initial_max_index=initial_max_index,max_index=seat+1,submit=intmap_insert seat this_submit submit} else Collector {initial_min_index=initial_min_index,min_index=min_index,initial_max_index=initial_max_index,max_index=max_index,submit=intmap_insert seat this_submit submit}
    _->error "collect_a: error 1"

to_collect::FCT.CFloat->FCT.CFloat->Widget a->Submit
to_collect u v widget=case lookup_widget widget of
    Visual {origin,matrix,maybe_clip,red,green,blue,alpha,visual}->let parameter=to_Parameter origin (move_matrix origin matrix) maybe_clip in case visual of
        Triangle {first_point,second_point,third_point}->let new_first_point=point_addition origin first_point in let new_second_point=point_addition origin second_point in let new_third_point=point_addition origin third_point in Submit {maybe_album_id=Nothing,vertex=DS.singleton (Vertex {red=red,green=green,blue=blue,alpha=alpha,x=new_first_point.x,y=new_first_point.y,u=u,v=v,parameter_id=0,size=0}) DS.|> Vertex {red=red,green=green,blue=blue,alpha=alpha,x=new_second_point.x,y=new_second_point.y,u=u,v=v,parameter_id=0,size=0} DS.|> Vertex {red=red,green=green,blue=blue,alpha=alpha,x=new_third_point.x,y=new_third_point.y,u=u,v=v,parameter_id=0,size=0},index=DS.singleton 0 DS.|> 1 DS.|> 2,parameter=parameter,vertex_length=3,index_length=3}
        Convex_polygon {point}->let number=DS.length point in if number<3 then error "to_collect: error 1" else let new_number=3*(number-2) in Submit {maybe_album_id=Nothing,vertex=fmap ((\this_point->Vertex {red=red,green=green,blue=blue,alpha=alpha,x=this_point.x,y=this_point.y,u=u,v=v,parameter_id=0,size=0}) . point_addition origin) point,index=DS.fromFunction new_number collect_convex_polygon,parameter=parameter,vertex_length=fromIntegral number,index_length=fromIntegral new_number}
        Regular_polygon {number,radius,angle}->if number<3 then error "to_collect: error 2" else let new_number=3*(number-2) in let new_angle=2*pi/fromIntegral number in Submit {maybe_album_id=Nothing,vertex=fmap (\point->Vertex {red=red,green=green,blue=blue,alpha=alpha,x=point.x,y=point.y,u=u,v=v,parameter_id=0,size=0}) (DS.fromFunction number (\index->let direction=angle+fromIntegral index*new_angle in Point {x=origin.x+radius*cos direction,y=origin.y+radius*sin direction})),index=DS.fromFunction new_number collect_convex_polygon,parameter=parameter,vertex_length=fromIntegral number,index_length=fromIntegral new_number}
        Picture {width,height,min_u,min_v,max_u,max_v,locked}->if locked then error "to_collect: error 3" else let new_width=width/2 in let new_height=height/2 in let first_point=Point {x=origin.x-new_width,y=origin.y-new_height} in let second_point=Point {x=origin.x+new_width,y=origin.y-new_height} in let third_point=Point {x=origin.x+new_width,y=origin.y+new_height} in let fourth_point=Point {x=origin.x-new_width,y=origin.y+new_height} in Submit {maybe_album_id=Nothing,vertex=DS.singleton (Vertex {red=red,green=green,blue=blue,alpha=alpha,x=first_point.x,y=first_point.y,u=min_u,v=min_v,parameter_id=0,size=0}) DS.|> Vertex {red=red,green=green,blue=blue,alpha=alpha,x=second_point.x,y=second_point.y,u=max_u,v=min_v,parameter_id=0,size=0} DS.|> Vertex {red=red,green=green,blue=blue,alpha=alpha,x=third_point.x,y=third_point.y,u=max_u,v=max_v,parameter_id=0,size=0} DS.|> Vertex {red=red,green=green,blue=blue,alpha=alpha,x=fourth_point.x,y=fourth_point.y,u=min_u,v=max_v,parameter_id=0,size=0},index=DS.singleton 0 DS.|> 1 DS.|> 2 DS.|> 0 DS.|> 2 DS.|> 3,parameter=parameter,vertex_length=4,index_length=6}
        Large_picture {width,height,album_id}->let new_width=width/2 in let new_height=height/2 in let first_point=Point {x=origin.x-new_width,y=origin.y-new_height} in let second_point=Point {x=origin.x+new_width,y=origin.y-new_height} in let third_point=Point {x=origin.x+new_width,y=origin.y+new_height} in let fourth_point=Point {x=origin.x-new_width,y=origin.y+new_height} in Submit {maybe_album_id=Just album_id,vertex=DS.singleton (Vertex {red=red,green=green,blue=blue,alpha=alpha,x=first_point.x,y=first_point.y,u=0,v=1,parameter_id=0,size=0}) DS.|> Vertex {red=red,green=green,blue=blue,alpha=alpha,x=second_point.x,y=second_point.y,u=1,v=1,parameter_id=0,size=0} DS.|> Vertex {red=red,green=green,blue=blue,alpha=alpha,x=third_point.x,y=third_point.y,u=1,v=0,parameter_id=0,size=0} DS.|> Vertex {red=red,green=green,blue=blue,alpha=alpha,x=fourth_point.x,y=fourth_point.y,u=0,v=0,parameter_id=0,size=0},index=DS.singleton 0 DS.|> 1 DS.|> 2 DS.|> 0 DS.|> 2 DS.|> 3,parameter=parameter,vertex_length=4,index_length=6}
    Text {origin,matrix,width,height,y,article}->let new_width=width/2 in let new_height=height/2 in let (vertex,index,_)=DF.foldl' (DF.foldl' (flip (collect_text origin y))) (DS.empty,DS.empty,0) (fmap (DS.takeWhileL (end_text y new_height) . DS.dropWhileL (begin_text y new_height)) article) in Submit {maybe_album_id=Nothing,vertex=vertex,index=index,parameter=Parameter {x=origin.x+matrix.x,y=origin.y+matrix.y,x_x=matrix.x_x,x_y=matrix.x_y,y_x=matrix.y_x,y_y=matrix.y_y,clip_flag=1,clip_left=origin.x-new_width,clip_down=origin.y-new_height,clip_right=origin.x+new_width,clip_up=origin.y+new_height},vertex_length=fromIntegral (DS.length vertex),index_length=fromIntegral (DS.length index)}
    _->error "to_collect: error 4"

to_Parameter::Point->Matrix->Maybe Clip->Parameter
to_Parameter origin matrix maybe_clip=case maybe_clip of
    Nothing->Parameter {x=matrix.x,y=matrix.y,x_x=matrix.x_x,x_y=matrix.x_y,y_x=matrix.y_x,y_y=matrix.y_y,clip_flag=0,clip_left=0,clip_down=0,clip_right=0,clip_up=0}
    Just clip->case clip of
        Clip {left,down,right,up}->Parameter {x=matrix.x,y=matrix.y,x_x=matrix.x_x,x_y=matrix.x_y,y_x=matrix.y_x,y_y=matrix.y_y,clip_flag=1,clip_left=origin.x+left,clip_down=origin.y+down,clip_right=origin.x+right,clip_up=origin.y+up}

begin_text::FCT.CFloat->FCT.CFloat->Row->Bool
begin_text this_y height row=case row of
    Blank->True
    Row {y,min_down}->y+height<this_y+min_down

end_text::FCT.CFloat->FCT.CFloat->Row->Bool
end_text this_y height row=case row of
    Blank->True
    Row {y,max_up}->y<=this_y+max_up+height

collect_convex_polygon::Int->DW.Word32
collect_convex_polygon index=let (quotient,remainder)=divMod index 3 in let new_quotient=fromIntegral quotient+1 in case remainder of
    0->0
    1->new_quotient
    2->new_quotient+1
    _->error "collect_convex_polygon: error 1"

collect_text::Point->FCT.CFloat->Row->(DS.Seq Vertex,DS.Seq DW.Word32,DW.Word32)->(DS.Seq Vertex,DS.Seq DW.Word32,DW.Word32)
collect_text origin this_y row primitive=case row of
    Blank->primitive
    Row {row_core,x,y}->DF.foldl' (flip (collect_character origin x (this_y-y))) primitive row_core

collect_character::Point->FCT.CFloat->FCT.CFloat->Character->(DS.Seq Vertex,DS.Seq DW.Word32,DW.Word32)->(DS.Seq Vertex,DS.Seq DW.Word32,DW.Word32)
collect_character origin x y character (vertex,index,index_offset)=case character of
    Character {size,left,down,right,up,min_u,min_v,max_u,max_v,red,green,blue,alpha}->let new_left=origin.x+x+left in let new_down=origin.y+y+down in let new_right=origin.x+x+right in let new_up=origin.y+y+up in (vertex DS.|> Vertex {red=red,green=green,blue=blue,alpha=alpha,x=new_left,y=new_down,u=min_u,v=min_v,parameter_id=0,size=size} DS.|> Vertex {red=red,green=green,blue=blue,alpha=alpha,x=new_right,y=new_down,u=max_u,v=min_v,parameter_id=0,size=size} DS.|> Vertex {red=red,green=green,blue=blue,alpha=alpha,x=new_right,y=new_up,u=max_u,v=max_v,parameter_id=0,size=size} DS.|> Vertex {red=red,green=green,blue=blue,alpha=alpha,x=new_left,y=new_up,u=min_u,v=max_v,parameter_id=0,size=size},index DS.|> index_offset DS.|> (index_offset+1) DS.|> (index_offset+2) DS.|> index_offset DS.|> (index_offset+2) DS.|> (index_offset+3),index_offset+4)

move::Projection_move->Int->Insert_strategy->Engine a->Engine a
move projection_move leaf_id collect_strategy engine=let (new_engine,widget)=move_lookup projection_move engine in new_engine {leaf=intmap_update leaf_id (update_projection_object (collect_a (move_a widget) collect_strategy)) new_engine.leaf}

move_a::Widget a->DS.Seq Submit
move_a widget=let new_widget=lookup_widget widget in case new_widget of
    Collector {submit}->DF.foldl' (DS.><) DS.empty submit
    _->error "move_a: error 1"

move_lookup::Projection_move->Engine a->(Engine a,Widget a)
move_lookup projection_move engine=case projection_move of
    Object_move {leaf_id,consume}->if consume then let (widget,leaf)=intmap_functor_update leaf_id (DT.swap . update_lookup_projection_widget_a (update_widget consume_widget)) engine.leaf in (engine {leaf=leaf},widget) else (engine,lookup_projection_object (intmap_lookup leaf_id engine.leaf))
    Image_move {leaf_id}->(engine,lookup_projection_image (intmap_lookup leaf_id engine.leaf))
    Image_safe_move {leaf_id}->(engine,lookup_projection_image_safe (intmap_lookup leaf_id engine.leaf))

consume_widget::Widget a->Widget a
consume_widget widget=case widget of
    Collector {initial_min_index,initial_max_index}->Collector {initial_min_index=initial_min_index,min_index=initial_min_index,initial_max_index=initial_max_index,max_index=initial_max_index,submit=DIM.empty}
    _->error "consume_widget: error 1"

for_submit::DIM.IntMap (DS.Seq Submit)->(DS.Seq Vertex,DS.Seq DW.Word32,DS.Seq Parameter,DS.Seq (Maybe Int,DW.Word32,DW.Word32))
for_submit submit=let (vertex,index,parameter,draw_call,_,_,_)=DIM.foldl' (DF.foldl' (flip for_submit_a)) (DS.empty,DS.empty,DS.empty,DS.empty,0,0,0) submit in (vertex,index,parameter,draw_call)

for_submit_a::Submit->(DS.Seq Vertex,DS.Seq DW.Word32,DS.Seq Parameter,DS.Seq (Maybe Int,DW.Word32,DW.Word32),DW.Word32,DW.Word32,FCT.CFloat)->(DS.Seq Vertex,DS.Seq DW.Word32,DS.Seq Parameter,DS.Seq (Maybe Int,DW.Word32,DW.Word32),DW.Word32,DW.Word32,FCT.CFloat)
for_submit_a submit (this_vertex,this_index,this_parameter,draw_call,vertex_offset,index_offset,parameter_id)=case submit of
    Submit {maybe_album_id,vertex,index,parameter,vertex_length,index_length}->(this_vertex DS.>< fmap (\this_this_vertex->this_this_vertex {parameter_id=parameter_id}) vertex,this_index DS.>< fmap (vertex_offset+) index,this_parameter DS.|> parameter,for_submit_b maybe_album_id index_length index_offset draw_call,vertex_offset+vertex_length,index_offset+index_length,parameter_id+1)

for_submit_b::Maybe Int->DW.Word32->DW.Word32->DS.Seq (Maybe Int,DW.Word32,DW.Word32)->DS.Seq (Maybe Int,DW.Word32,DW.Word32)
for_submit_b maybe_album_id index_length index_offset draw_call=case draw_call of
    DS.Empty->DS.singleton (maybe_album_id,index_length,index_offset)
    other_draw_call DS.:|> (new_maybe_album_id,new_index_length,new_index_offset)->if maybe_album_id==new_maybe_album_id then other_draw_call DS.|> (maybe_album_id,index_length+new_index_length,new_index_offset) else draw_call DS.|> (maybe_album_id,index_length,index_offset)