{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Engine.Leaf where

import Engine.Atlas
import Engine.Other
import Engine.Text
import Engine.Type
import qualified SDL.Function as F
import qualified Control.Monad as CM
import qualified Data.IntMap as DIM
import qualified Data.Sequence as DS

get_widget::Widget a->Widget a
get_widget this_widget=case this_widget of
    Double {which,first_widget,second_widget}->if which then get_widget first_widget else get_widget second_widget
    Group {index,group_widget}->get_widget (intmap_lookup index group_widget)
    Widget_trigger {widget}->get_widget widget
    Widget_io_trigger {widget}->get_widget widget
    Widget_mix_trigger {widget}->get_widget widget
    _->this_widget

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
    Group_request {index,group_widget_request}->do
        (new_engine,group_widget)<-DIM.foldlWithKey' (\accumulation key widget_request->intmap_engine_monad_fold key create_widget widget_request accumulation) (return (engine,DIM.empty)) group_widget_request
        return (new_engine,Group {index=index,group_widget=group_widget})
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
    Store_request {store}->return (engine,Store {store=store})
    Collector_request {initial_min_index,initial_max_index}->return (engine,Collector {initial_min_index=initial_min_index,initial_max_index=initial_max_index,min_index=initial_min_index,max_index=initial_max_index,submit=DIM.empty})
    Visual_request {origin,matrix,maybe_clip,red,green,blue,alpha,visual_request}->do
        (new_engine,visual)<-create_visual visual_request engine
        return (new_engine,Visual {origin=origin,matrix=matrix,maybe_clip=maybe_clip,red=red,green=green,blue=blue,alpha=alpha,visual=visual})
    Text_request {origin,matrix,width,height,article,calculate_width,calculate_typesetting,load}->let charset=to_charset article in let new_height=height/2 in if load
        then do
            new_engine<-update_font charset engine
            return (new_engine,let (new_article,max_y)=do_typesetting new_height calculate_typesetting (for_text new_engine.font new_engine.font_map article calculate_width) in Text {origin=origin,matrix=matrix,width=width,height=height,y=0,max_y=max_y+new_height,article=new_article,charset=charset,locked=False})
        else return (engine,let (new_article,max_y)=do_typesetting new_height calculate_typesetting (for_text engine.font engine.font_map article calculate_width) in Text {origin=origin,matrix=matrix,width=width,height=height,y=0,max_y=max_y+new_height,article=new_article,charset=charset,locked=False})

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
    Visual {visual}->case visual of
        Large_picture {album_id}->let (album,single_album)=intmap_delete_lookup album_id engine.album in do
            F.sdl_release_gpu_texture engine.device single_album.texture
            return (engine {album=album})
        _->return engine
    _->return engine