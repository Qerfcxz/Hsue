{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Engine.Widget where

import Engine.Atlas
import Engine.Other
import Engine.Type
import qualified SDL.Function as F
import qualified Data.IntMap as DIM
import qualified Data.Sequence as DS

create_active::Maybe Int->Widget_request a->Int->Engine a->Engine a
create_active father widget_request active_id engine=let (widget,next)=make_active widget_request in case father of
    Nothing->engine {active=intmap_insert active_id (Active {next=next,ancestry=DS.empty,projection=Without widget}) engine.active}
    Just node_id->let (new_node,node)=intmap_update_lookup node_id (\this_node->this_node {active_child=intset_insert active_id this_node.active_child}) engine.node in engine {active=intmap_insert active_id (Active {next=next,ancestry=node.ancestry DS.|> node_id,projection=Without widget}) engine.active,node=new_node}

make_active::Widget_request a->(Widget a,Engine a->Event->Maybe Int)
make_active widget_request=case widget_request of
    Trigger_request {next,trigger}->(Trigger {trigger=trigger},next)
    Io_trigger_request {next,io_trigger}->(Io_trigger {io_trigger=io_trigger},next)
    _->error "make_active: error 1"

create_inactive::Maybe Int->Widget_request a->Int->Engine a->IO (Engine a)
create_inactive father widget_request inactive_id engine=do
    (new_engine,widget)<-make_inactive widget_request engine
    case father of
        Nothing->return (new_engine {inactive=intmap_insert inactive_id (Inactive {ancestry=DS.empty,projection=Without widget}) new_engine.inactive})
        Just node_id->let (new_node,node)=intmap_update_lookup node_id (\this_node->this_node {inactive_child=intset_insert inactive_id this_node.inactive_child}) new_engine.node in return (new_engine {inactive=intmap_insert inactive_id (Inactive {ancestry=node.ancestry DS.|> node_id,projection=Without widget}) new_engine.inactive,node=new_node})

make_inactive::Widget_request a->Engine a->IO (Engine a,Widget a)
make_inactive widget_request engine=case widget_request of
    Collector_request {initial_min_index,initial_max_index}->return (engine,Collector {initial_min_index=initial_min_index,initial_max_index=initial_max_index,min_index=initial_min_index,max_index=initial_max_index,submit=DIM.empty})
    Visual_request {red,green,blue,alpha,matrix,visual_request}->do
        (new_engine,visual)<-make_visual visual_request engine
        return (new_engine,Visual {red=red,green=green,blue=blue,alpha=alpha,matrix=matrix,visual=visual})
    _->error "make_inactive: error 1"

make_visual::Visual_request->Engine a->IO (Engine a,Visual)
make_visual visual_request engine=case visual_request of
    Triangle_request {first_point,second_point,third_point}->return (engine,Triangle {first_point,second_point,third_point})
    Convex_polygon_request {point}->return (engine,Convex_polygon {point})
    Regular_polygon_request {number,center,radius,angle}->return (engine,Regular_polygon {number,center,radius,angle})
    Picture_request {center,path}->do
        (texture,width,height)<-load_texture engine.device engine.picture_transfer_buffer engine.picture_size path
        let (atlas,atlas_id,left,down)=atlas_insert width height engine.padding engine.atlas
        copy_texture engine.device texture engine.texture left down width height
        let new_width=fromIntegral width/2
        let new_height=fromIntegral height/2
        return (engine {atlas=atlas,album_id=engine.album_id+1,album=intmap_insert engine.album_id (Album width height texture) engine.album},Picture {left=center.x-new_width,down=center.y-new_height,right=center.x+new_width,up=center.y+new_height,album_id=engine.album_id,atlas_id=atlas_id})
    Large_picture_request {center,path}->do
        (texture,width,height)<-load_texture engine.device engine.picture_transfer_buffer engine.picture_size path
        let new_width=fromIntegral width/2
        let new_height=fromIntegral height/2
        return (engine {album_id=engine.album_id+1,album=intmap_insert engine.album_id (Album width height texture) engine.album},Large_picture {left=center.x-new_width,down=center.y-new_height,right=center.x+new_width,up=center.y+new_height,album_id=engine.album_id})

remove_active::Int->Engine a->IO (Engine a)
remove_active active_id engine=let (new_active,active)=intmap_delete_lookup active_id engine.active in case active.ancestry of
    DS.Empty->clean_widget active.projection.object (engine {active=new_active})
    _ DS.:|> node_id->clean_widget active.projection.object (engine {active=new_active,node=intmap_update node_id (\node->node {active_child=intset_delete active_id node.active_child}) engine.node})

remove_inactive::Int->Engine a->IO (Engine a)
remove_inactive inactive_id engine=let (new_inactive,inactive)=intmap_delete_lookup inactive_id engine.inactive in case inactive.ancestry of
    DS.Empty->clean_widget inactive.projection.object (engine {inactive=new_inactive})
    _ DS.:|> node_id->clean_widget inactive.projection.object (engine {inactive=new_inactive,node=intmap_update node_id (\node->node {inactive_child=intset_delete inactive_id node.inactive_child}) engine.node})

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