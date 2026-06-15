{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Engine.Widget where

import Engine.Atlas
import Engine.Other
import Engine.Projection
import Engine.Text
import Engine.Type
import qualified SDL.Function as F
import qualified Data.IntMap as DIM
import qualified Data.Sequence as DS

create_active::Int->Maybe Int->Widget_request a->Engine a->Engine a
create_active active_id father widget_request engine=let (widget,next)=make_active widget_request in case father of
    Nothing->engine {active=intmap_insert active_id (Active {ancestry=DS.empty,projection=Without widget,next=next}) engine.active}
    Just node_id->let (new_node,node)=intmap_update_lookup node_id (\this_node->this_node {active_child=intset_insert active_id this_node.active_child}) engine.node in engine {active=intmap_insert active_id (Active {ancestry=node.ancestry DS.|> node_id,projection=Without widget,next=next}) engine.active,node=new_node}

make_active::Widget_request a->(Widget a,Engine a->Event->Maybe Int)
make_active widget_request=case widget_request of
    Trigger_request {trigger,next}->(Trigger {trigger=trigger},next)
    Io_trigger_request {io_trigger,next}->(Io_trigger {io_trigger=io_trigger},next)
    _->error "make_active: error 1"

create_inactive::Int->Maybe Int->Widget_request a->Engine a->IO (Engine a)
create_inactive inactive_id father widget_request engine=do
    (new_engine,widget)<-make_inactive widget_request engine
    case father of
        Nothing->return (new_engine {inactive=intmap_insert inactive_id (Inactive {ancestry=DS.empty,projection=Without widget}) new_engine.inactive})
        Just node_id->let (new_node,node)=intmap_update_lookup node_id (\this_node->this_node {inactive_child=intset_insert inactive_id this_node.inactive_child}) new_engine.node in return (new_engine {inactive=intmap_insert inactive_id (Inactive {ancestry=node.ancestry DS.|> node_id,projection=Without widget}) new_engine.inactive,node=new_node})

make_inactive::Widget_request a->Engine a->IO (Engine a,Widget a)
make_inactive widget_request engine=case widget_request of
    Collector_request {initial_min_index,initial_max_index}->return (engine,Collector {initial_min_index=initial_min_index,initial_max_index=initial_max_index,min_index=initial_min_index,max_index=initial_max_index,submit=DIM.empty})
    Visual_request {origin,matrix,clip,red,green,blue,alpha,visual_request}->do
        (new_engine,visual)<-make_visual visual_request engine
        return (new_engine,Visual {origin=origin,matrix=matrix,clip=clip,red=red,green=green,blue=blue,alpha=alpha,visual=visual})
    Text_request {origin,matrix,width,height,text,calculate_width,calculate_typesetting}->return (engine,Text {origin=origin,matrix=matrix,width=width,height=height,y=0,text=do_typesetting (height/2) calculate_typesetting (for_text engine.font text calculate_width)})
    _->error "make_inactive: error 1"

make_visual::Visual_request->Engine a->IO (Engine a,Visual)
make_visual visual_request engine=case visual_request of
    Triangle_request {first_point,second_point,third_point}->return (engine,Triangle {first_point=first_point,second_point=second_point,third_point=third_point})
    Convex_polygon_request {point}->return (engine,Convex_polygon {point})
    Regular_polygon_request {number,radius,angle}->return (engine,Regular_polygon {number=number,radius=radius,angle=angle})
    Picture_request {path}->do
        (texture,width,height)<-load_texture engine.device engine.picture_transfer_buffer engine.picture_size path
        let (atlas,left,down,right,up)=atlas_insert width height engine.padding engine.atlas
        copy_texture engine.device texture engine.texture left down width height
        return (engine {atlas=atlas,album=intmap_insert engine.album_id (Album width height texture) engine.album,album_id=engine.album_id+1},Picture {width=fromIntegral width,height=fromIntegral height,album_id=engine.album_id,min_u=fromIntegral left*engine.reciprocal_width,min_v=fromIntegral down*engine.reciprocal_height,max_u=fromIntegral right*engine.reciprocal_width,max_v=fromIntegral up*engine.reciprocal_height,locked=False})
    Large_picture_request {path}->do
        (texture,width,height)<-load_texture engine.device engine.picture_transfer_buffer engine.picture_size path
        return (engine {album=intmap_insert engine.album_id (Album width height texture) engine.album,album_id=engine.album_id+1},Large_picture {width=fromIntegral width,height=fromIntegral height,album_id=engine.album_id})

remove_active::Int->Engine a->IO (Engine a)
remove_active active_id engine=let (new_active,active)=intmap_delete_lookup active_id engine.active in case active.ancestry of
    DS.Empty->clean_widget (lookup_projection_object active.projection) (engine {active=new_active})
    _ DS.:|> node_id->clean_widget (lookup_projection_object active.projection) (engine {active=new_active,node=intmap_update node_id (\node->node {active_child=intset_delete active_id node.active_child}) engine.node})

remove_inactive::Int->Engine a->IO (Engine a)
remove_inactive inactive_id engine=let (new_inactive,inactive)=intmap_delete_lookup inactive_id engine.inactive in case inactive.ancestry of
    DS.Empty->clean_widget (lookup_projection_object inactive.projection) (engine {inactive=new_inactive})
    _ DS.:|> node_id->clean_widget (lookup_projection_object inactive.projection) (engine {inactive=new_inactive,node=intmap_update node_id (\node->node {inactive_child=intset_delete inactive_id node.inactive_child}) engine.node})

clean_widget::Widget a->Engine a->IO (Engine a)
clean_widget widget engine=case widget of
    Visual {visual}->case visual of
        Picture {album_id}->let (new_album,album)=intmap_delete_lookup album_id engine.album in do
            F.sdl_releasegputexture engine.device album.texture
            return (engine {album=new_album})
        Large_picture {album_id}->let (new_album,album)=intmap_delete_lookup album_id engine.album in do
            F.sdl_releasegputexture engine.device album.texture
            return (engine {album=new_album})
        _->return engine
    _->return engine