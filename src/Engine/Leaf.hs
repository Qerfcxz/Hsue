{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Engine.Leaf where

import Engine.Atlas
import Engine.Container
import Engine.Coroutine
import Engine.Projection
import Engine.Text
import Engine.Type
import qualified SDL.Function as F
import qualified Control.Monad as CM
import qualified Data.IntMap as DIM
import qualified Data.Sequence as DS

insert_same_multiple_insert::Int->DS.Seq Insert_strategy->Widget a->Engine a->Engine a
insert_same_multiple_insert leaf_id multiple_insert_strategy widget engine=engine {leaf=intmap_update leaf_id (update_projection_object (insert_same_multiple_insert_a multiple_insert_strategy widget)) engine.leaf}

insert_same_multiple_insert_a::DS.Seq Insert_strategy->Widget a->Widget a->Widget a
insert_same_multiple_insert_a multiple_insert_strategy widget this_widget=case this_widget of
    Group {initial_min_index,min_index,initial_max_index,max_index,index,group_widget}->let (new_group_widget,new_max_index,new_min_index)=insert_same_multiple_insert_b min_index max_index multiple_insert_strategy widget group_widget in Group {initial_min_index=initial_min_index,min_index=new_min_index,initial_max_index=initial_max_index,max_index=new_max_index,index=index,group_widget=new_group_widget}
    Coroutine {initial_min_index,min_index,initial_max_index,max_index,index,coroutine_state,linear_coroutine}->let (new_coroutine_state,new_max_index,new_min_index)=insert_same_multiple_insert_b min_index max_index multiple_insert_strategy (init_coroutine_state widget) coroutine_state in Coroutine {initial_min_index=initial_min_index,min_index=new_min_index,initial_max_index=initial_max_index,max_index=new_max_index,index=index,coroutine_state=new_coroutine_state,linear_coroutine=linear_coroutine}
    _->error "insert_same_multiple_insert_a: error 1"

insert_same_multiple_insert_b::Int->Int->DS.Seq Insert_strategy->a->DIM.IntMap a->(DIM.IntMap a,Int,Int)
insert_same_multiple_insert_b min_index max_index multiple_insert_strategy value intmap=case multiple_insert_strategy of
    DS.Empty->(intmap,max_index,min_index)
    insert_strategy DS.:<| other_insert_strategy->case insert_strategy of
        Min_strategy->insert_same_multiple_insert_b (min_index-1) max_index other_insert_strategy value (intmap_insert min_index value intmap)
        Max_strategy->insert_same_multiple_insert_b min_index (max_index+1) other_insert_strategy value (intmap_insert max_index value intmap)
        Index_strategy {seat}->if seat<=min_index then insert_same_multiple_insert_b (seat-1) max_index other_insert_strategy value (intmap_insert seat value intmap) else if max_index<=seat then insert_same_multiple_insert_b min_index (seat+1) other_insert_strategy value (intmap_insert seat value intmap) else insert_same_multiple_insert_b min_index max_index other_insert_strategy value (intmap_insert seat value intmap)

insert_multiple_insert::Int->DS.Seq (Insert a (Widget a))->Engine a->Engine a
insert_multiple_insert leaf_id multiple_insert engine=engine {leaf=intmap_update leaf_id (update_projection_object (insert_multiple_insert_a multiple_insert)) engine.leaf}

insert_multiple_insert_a::DS.Seq (Insert a (Widget a))->Widget a->Widget a
insert_multiple_insert_a multiple_insert widget=case widget of
    Group {initial_min_index,min_index,initial_max_index,max_index,index,group_widget}->let (new_group_widget,new_max_index,new_min_index)=insert_multiple_insert_b min_index max_index id multiple_insert group_widget in Group {initial_min_index=initial_min_index,min_index=new_min_index,initial_max_index=initial_max_index,max_index=new_max_index,index=index,group_widget=new_group_widget}
    Coroutine {initial_min_index,min_index,initial_max_index,max_index,index,coroutine_state,linear_coroutine}->let (new_coroutine_state,new_max_index,new_min_index)=insert_multiple_insert_b min_index max_index init_coroutine_state multiple_insert coroutine_state in Coroutine {initial_min_index=initial_min_index,min_index=new_min_index,initial_max_index=initial_max_index,max_index=new_max_index,index=index,coroutine_state=new_coroutine_state,linear_coroutine=linear_coroutine}
    _->error "insert_multiple_insert_a: error 1"

insert_multiple_insert_b::Int->Int->(Widget a->b)->DS.Seq (Insert a (Widget a))->DIM.IntMap b->(DIM.IntMap b,Int,Int)
insert_multiple_insert_b min_index max_index transform multiple_insert intmap=case multiple_insert of
    DS.Empty->(intmap,max_index,min_index)
    insert DS.:<| other_insert->case insert of
        Insert {insert_strategy,value}->case insert_strategy of
            Min_strategy->insert_multiple_insert_b (min_index-1) max_index transform other_insert (intmap_insert min_index (transform value) intmap)
            Max_strategy->insert_multiple_insert_b min_index (max_index+1) transform other_insert (intmap_insert max_index (transform value) intmap)
            Index_strategy {seat}->if seat<=min_index then insert_multiple_insert_b (seat-1) max_index transform other_insert (intmap_insert seat (transform value) intmap) else if max_index<=seat then insert_multiple_insert_b min_index (seat+1) transform other_insert (intmap_insert seat (transform value) intmap) else insert_multiple_insert_b min_index max_index transform other_insert (intmap_insert seat (transform value) intmap)

create_leaf::Int->Maybe Int->Widget_request a->Engine a->IO (Engine a)
create_leaf leaf_id maybe_father_id widget_request engine=do
    (new_engine,widget)<-create_widget widget_request engine
    case maybe_father_id of
        Nothing->return (new_engine {leaf=intmap_insert leaf_id (Without {ancestry_id=DS.empty,object=widget}) new_engine.leaf})
        Just father_id->let (new_node,node)=intmap_update_lookup father_id (\this_node->this_node {leaf_child=intset_insert leaf_id this_node.leaf_child}) new_engine.node in return (new_engine {leaf=intmap_insert leaf_id (Without {ancestry_id=node.ancestry_id DS.|> father_id,object=widget}) new_engine.leaf,node=new_node})

create_widget::Widget_request a->Engine a->IO (Engine a,Widget a)
create_widget this_widget_request engine=case this_widget_request of
    Double_request {which,first_widget_request,second_widget_request}->do
        (new_engine,first_widget)<-create_widget first_widget_request engine
        (new_new_engine,second_widget)<-create_widget second_widget_request new_engine
        return (new_new_engine,Double {which=which,first_widget=first_widget,second_widget=second_widget})
    Group_request {initial_min_index,initial_max_index,index,multiple_insert}->do
        (group_widget,max_index,min_index,new_engine)<-init_multiple_insert engine initial_min_index initial_max_index id multiple_insert DIM.empty
        return (new_engine,Group {initial_min_index=initial_min_index,min_index=min_index,initial_max_index=initial_max_index,max_index=max_index,index=index,group_widget=group_widget})
    Trigger_request {next,trigger}->return (engine,Trigger {next=next,trigger=trigger})
    Io_trigger_request {next,io_trigger}->return (engine,Io_trigger {next=next,io_trigger=io_trigger})
    Mix_trigger_request {next,mix_trigger,order}->return (engine,Mix_trigger {next=next,mix_trigger=mix_trigger,order=order})
    Widget_trigger_request {next,widget_trigger,widget_request}->do
        (new_engine,widget)<-create_widget widget_request engine
        return (new_engine,Widget_trigger {next=next,widget_trigger=widget_trigger,widget=widget})
    Widget_io_trigger_request {next,widget_io_trigger,widget_request}->do
        (new_engine,widget)<-create_widget widget_request engine
        return (new_engine,Widget_io_trigger {next=next,widget_io_trigger=widget_io_trigger,widget=widget})
    Widget_mix_trigger_request {next,widget_mix_trigger,order,widget_request}->do
        (new_engine,widget)<-create_widget widget_request engine
        return (new_engine,Widget_mix_trigger {next=next,widget_mix_trigger=widget_mix_trigger,order=order,widget=widget})
    Coroutine_request {initial_min_index,initial_max_index,index,multiple_insert,raw_coroutine}->do
        (coroutine_state,max_index,min_index,new_engine)<-init_multiple_insert engine initial_min_index initial_max_index init_coroutine_state multiple_insert DIM.empty
        return (new_engine,Coroutine {initial_min_index=initial_min_index,min_index=min_index,initial_max_index=initial_max_index,max_index=max_index,index=index,coroutine_state=coroutine_state,linear_coroutine=let (linear_coroutine,_)=from_coroutine (to_coroutine raw_coroutine) in linear_coroutine})
    Store_request {store}->return (engine,Store {store=store})
    Collector_request {initial_min_index,initial_max_index}->return (engine,Collector {initial_min_index=initial_min_index,min_index=initial_min_index,initial_max_index=initial_max_index,max_index=initial_max_index,submit=DIM.empty})
    Visual_request {origin,matrix,maybe_clip,red,green,blue,alpha,visual_request}->do
        (new_engine,visual)<-create_visual visual_request engine
        return (new_engine,Visual {origin=origin,matrix=matrix,maybe_clip=maybe_clip,red=red,green=green,blue=blue,alpha=alpha,visual=visual})
    Text_request {origin,matrix,width,height,article,calculate_width,calculate_typesetting,load}->let charset=to_charset article in let new_height=height/2 in if load
        then do
            new_engine<-update_font charset engine
            return (new_engine,let (new_article,max_y)=do_typesetting new_height calculate_typesetting (for_text new_engine.font new_engine.font_map article calculate_width) in Text {origin=origin,matrix=matrix,width=width,height=height,y=0,max_y=max_y+new_height,article=new_article,charset=charset,locked=False})
        else return (engine,let (new_article,max_y)=do_typesetting new_height calculate_typesetting (for_text engine.font engine.font_map article calculate_width) in Text {origin=origin,matrix=matrix,width=width,height=height,y=0,max_y=max_y+new_height,article=new_article,charset=charset,locked=False})

init_multiple_insert::Engine a->Int->Int->(Widget a->b)->DS.Seq (Insert a (Widget_request a))->DIM.IntMap b->IO (DIM.IntMap b,Int,Int,Engine a)
init_multiple_insert engine min_index max_index transform multiple_insert intmap=case multiple_insert of
    DS.Empty->return (intmap,max_index,min_index,engine)
    insert DS.:<| other_insert->case insert of
        Insert {insert_strategy,value}->do
            (new_engine,widget)<-create_widget value engine
            case insert_strategy of
                Min_strategy->init_multiple_insert new_engine (min_index-1) max_index transform other_insert (intmap_insert min_index (transform widget) intmap)
                Max_strategy->init_multiple_insert new_engine min_index (max_index+1) transform other_insert (intmap_insert max_index (transform widget) intmap)
                Index_strategy {seat}->if seat<=min_index then init_multiple_insert new_engine (seat-1) max_index transform other_insert (intmap_insert seat (transform widget) intmap) else if max_index<=seat then init_multiple_insert new_engine min_index (seat+1) transform other_insert (intmap_insert seat (transform widget) intmap) else init_multiple_insert new_engine min_index max_index transform other_insert (intmap_insert seat (transform widget) intmap)

create_visual::Visual_request->Engine a->IO (Engine a,Visual)
create_visual visual_request engine=case visual_request of
    Triangle_request {first_point,second_point,third_point}->return (engine,Triangle {first_point=first_point,second_point=second_point,third_point=third_point})
    Convex_polygon_request {point}->return (engine,Convex_polygon {point=point})
    Regular_polygon_request {number,radius,angle}->return (engine,Regular_polygon {number=number,radius=radius,angle=angle})
    Picture_request {path}->create_picture path engine
    Large_picture_request {path}->do
        (texture,width,height)<-load_texture engine.device engine.picture_transfer_buffer engine.picture_size path
        return (engine {album=intmap_insert engine.album_id (Album {width=width,height=height,texture=texture}) engine.album,album_id=engine.album_id+1},Large_picture {width=fromIntegral width,height=fromIntegral height,album_id=engine.album_id})

create_picture::String->Engine a->IO (Engine a,Visual)
create_picture path engine=do
    (texture,width,height)<-load_texture engine.device engine.picture_transfer_buffer engine.picture_size path
    let (atlas,left,down,right,up)=atlas_insert width height engine.padding engine.atlas
    copy_texture engine.device texture engine.texture left down width height
    F.sdl_release_gpu_texture engine.device texture
    return (engine {atlas=atlas},Picture {width=fromIntegral width,height=fromIntegral height,min_u=fromIntegral left*engine.reciprocal_width,min_v=fromIntegral down*engine.reciprocal_height,max_u=fromIntegral right*engine.reciprocal_width,max_v=fromIntegral up*engine.reciprocal_height,path=path,locked=False})

remove_leaf::Int->Engine a->IO (Engine a)
remove_leaf leaf_id engine=let (leaf,projection)=intmap_delete_lookup leaf_id engine.leaf in case projection of
    Without {ancestry_id,object}->remove_leaf_a ancestry_id object leaf leaf_id engine
    With {ancestry_id,object}->remove_leaf_a ancestry_id object leaf leaf_id engine

remove_leaf_a::DS.Seq Int->Widget a->DIM.IntMap (Projection a)->Int->Engine a->IO (Engine a)
remove_leaf_a ancestry_id object leaf leaf_id engine=case ancestry_id of
    DS.Empty->remove_widget object (engine {leaf=leaf})
    _ DS.:|> node_id->remove_widget object (engine {leaf=leaf,node=intmap_update node_id (\node->node {leaf_child=intset_delete leaf_id node.leaf_child}) engine.node})

remove_widget::Widget a->Engine a->IO (Engine a)
remove_widget this_widget engine=case this_widget of
    Double {first_widget,second_widget}->do
        new_engine<-remove_widget first_widget engine
        remove_widget second_widget new_engine
    Group {group_widget}->CM.foldM (flip remove_widget) engine group_widget
    Widget_trigger {widget}->remove_widget widget engine
    Widget_io_trigger {widget}->remove_widget widget engine
    Widget_mix_trigger {widget}->remove_widget widget engine
    Coroutine {coroutine_state}->CM.foldM (\this_engine this_coroutine_state->remove_widget this_coroutine_state.widget this_engine) engine coroutine_state
    Visual {visual}->case visual of
        Large_picture {album_id}->let (new_album,album)=intmap_delete_lookup album_id engine.album in do
            F.sdl_release_gpu_texture engine.device album.texture
            return (engine {album=new_album})
        _->return engine
    _->return engine