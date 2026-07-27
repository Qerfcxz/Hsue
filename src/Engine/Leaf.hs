{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Engine.Leaf where

import Engine.Atlas
import Engine.Container
import Engine.Coroutine
import Engine.Helper
import Engine.Projection
import Engine.Text
import Engine.Type
import qualified SDL.Function as SDLF
import qualified SDL.Include as SDLI
import qualified SDL.Type as SDLT
import qualified Error.Error as EE
import qualified Control.Monad as CM
import qualified Data.Foldable as DF
import qualified Data.IntMap as DIM
import qualified Data.Sequence as DS
import qualified Data.Vector.Storable as DVS
import qualified Data.Word as DW
import qualified Foreign.C.Types as FCT
import qualified Foreign.Marshal.Utils as FMU
import qualified Foreign.Ptr as FP
import qualified Foreign.Storable as FS

from_same_insert_widget::Int->DS.Seq Insert_strategy->Widget a b c d e->Engine a b c d e->Engine a b c d e
from_same_insert_widget leaf_id insert_widget_strategy widget engine=engine {leaf=intmap_update leaf_id (update_projection_object (from_same_insert_widget_a insert_widget_strategy widget)) engine.leaf}

from_same_insert_widget_a::DS.Seq Insert_strategy->Widget a b c d e->Widget a b c d e->Widget a b c d e
from_same_insert_widget_a insert_widget_strategy widget this_widget=case this_widget of
    Group {initial_min_index,min_index,initial_max_index,max_index,index,group_widget}->let (new_group_widget,new_max_index,new_min_index)=from_same_insert_widget_b min_index max_index insert_widget_strategy widget group_widget in Group {initial_min_index=initial_min_index,min_index=new_min_index,initial_max_index=initial_max_index,max_index=new_max_index,index=index,group_widget=new_group_widget}
    Coroutine {index,initial_min_index,min_index,initial_max_index,max_index,variable_length,user_variable_length,coroutine_state,layout,linear_coroutine,iterative}->let (new_coroutine_state,new_max_index,new_min_index)=from_same_insert_widget_b min_index max_index insert_widget_strategy (init_coroutine_state variable_length user_variable_length widget) coroutine_state in Coroutine {index=index,initial_min_index=initial_min_index,min_index=new_min_index,initial_max_index=initial_max_index,max_index=new_max_index,variable_length=variable_length,user_variable_length=user_variable_length,coroutine_state=new_coroutine_state,layout=layout,linear_coroutine=linear_coroutine,iterative=iterative}
    _->EE.quick_error "from_same_insert_widget_a" 0

from_same_insert_widget_b::Int->Int->DS.Seq Insert_strategy->a->DIM.IntMap a->(DIM.IntMap a,Int,Int)
from_same_insert_widget_b min_index max_index insert_widget_strategy value intmap=case insert_widget_strategy of
    DS.Empty->(intmap,max_index,min_index)
    insert_strategy DS.:<| other_insert_strategy->case insert_strategy of
        Min_strategy->from_same_insert_widget_b (min_index-1) max_index other_insert_strategy value (intmap_insert min_index value intmap)
        Max_strategy->from_same_insert_widget_b min_index (max_index+1) other_insert_strategy value (intmap_insert max_index value intmap)
        Index_strategy {seat}->if seat<=min_index then from_same_insert_widget_b (seat-1) max_index other_insert_strategy value (intmap_insert seat value intmap) else if max_index<=seat then from_same_insert_widget_b min_index (seat+1) other_insert_strategy value (intmap_insert seat value intmap) else from_same_insert_widget_b min_index max_index other_insert_strategy value (intmap_insert seat value intmap)

from_insert_widget::Int->DS.Seq (Insert (Widget a b c d e))->Engine a b c d e->Engine a b c d e
from_insert_widget leaf_id insert_widget engine=engine {leaf=intmap_update leaf_id (update_projection_object (from_insert_widget_a insert_widget)) engine.leaf}

from_insert_widget_a::DS.Seq (Insert (Widget a b c d e))->Widget a b c d e->Widget a b c d e
from_insert_widget_a insert_widget widget=case widget of
    Group {initial_min_index,min_index,initial_max_index,max_index,index,group_widget}->let (new_group_widget,new_max_index,new_min_index)=from_insert_widget_b min_index max_index id insert_widget group_widget in Group {initial_min_index=initial_min_index,min_index=new_min_index,initial_max_index=initial_max_index,max_index=new_max_index,index=index,group_widget=new_group_widget}
    Coroutine {index,initial_min_index,min_index,initial_max_index,max_index,variable_length,user_variable_length,coroutine_state,layout,linear_coroutine,iterative}->let (new_coroutine_state,new_max_index,new_min_index)=from_insert_widget_b min_index max_index (init_coroutine_state variable_length user_variable_length) insert_widget coroutine_state in Coroutine {index=index,initial_min_index=initial_min_index,min_index=new_min_index,initial_max_index=initial_max_index,max_index=new_max_index,variable_length=variable_length,user_variable_length=user_variable_length,coroutine_state=new_coroutine_state,layout=layout,linear_coroutine=linear_coroutine,iterative=iterative}
    _->EE.quick_error "from_insert_widget_a" 0

from_insert_widget_b::Int->Int->(Widget a b c d e->f)->DS.Seq (Insert (Widget a b c d e))->DIM.IntMap f->(DIM.IntMap f,Int,Int)
from_insert_widget_b min_index max_index transform insert_widget intmap=case insert_widget of
    DS.Empty->(intmap,max_index,min_index)
    insert DS.:<| other_insert->case insert of
        Insert {insert_strategy,value}->case insert_strategy of
            Min_strategy->from_insert_widget_b (min_index-1) max_index transform other_insert (intmap_insert min_index (transform value) intmap)
            Max_strategy->from_insert_widget_b min_index (max_index+1) transform other_insert (intmap_insert max_index (transform value) intmap)
            Index_strategy {seat}->if seat<=min_index then from_insert_widget_b (seat-1) max_index transform other_insert (intmap_insert seat (transform value) intmap) else if max_index<=seat then from_insert_widget_b min_index (seat+1) transform other_insert (intmap_insert seat (transform value) intmap) else from_insert_widget_b min_index max_index transform other_insert (intmap_insert seat (transform value) intmap)

create_leaf::Custom_widget_request e=>Int->Maybe Int->Widget_request a b c d e->Engine a b c d e->IO (Engine a b c d e)
create_leaf leaf_id maybe_father_id widget_request engine=do
    (new_engine,widget)<-create_widget widget_request engine
    case maybe_father_id of
        Nothing->return (new_engine {leaf=intmap_insert leaf_id (Without {ancestry_id=DS.empty,object=widget}) new_engine.leaf})
        Just father_id->let (node,single_node)=intmap_update_lookup father_id (\this_node->this_node {leaf_child=intset_insert leaf_id this_node.leaf_child}) new_engine.node in return (new_engine {leaf=intmap_insert leaf_id (Without {ancestry_id=single_node.ancestry_id DS.|> father_id,object=widget}) new_engine.leaf,node=node})

create_widget::Custom_widget_request e=>Widget_request a b c d e->Engine a b c d e->IO (Engine a b c d e,Widget a b c d e)
create_widget this_widget_request engine=case this_widget_request of
    Double_request {which,first_widget_request,second_widget_request}->do
        (new_engine,first_widget)<-create_widget first_widget_request engine
        (new_new_engine,second_widget)<-create_widget second_widget_request new_engine
        return (new_new_engine,Double {which=which,first_widget=first_widget,second_widget=second_widget})
    Group_request {initial_min_index,initial_max_index,index,insert_widget_request}->do
        (group_widget,max_index,min_index,new_engine)<-from_insert_widget_request engine initial_min_index initial_max_index id insert_widget_request DIM.empty
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
    Coroutine_request {index,initial_min_index,initial_max_index,insert_widget_request,raw_coroutine,iterative}->let (linear_coroutine,layout,variable_length,user_variable_length)=let (int,coroutine_sequence,_)=raw_coroutine.iterator 0 in from_coroutine (to_coroutine coroutine_sequence) int in do
        (coroutine_state,max_index,min_index,new_engine)<-from_insert_widget_request engine initial_min_index initial_max_index (init_coroutine_state variable_length user_variable_length) insert_widget_request DIM.empty
        return (new_engine,Coroutine {index=index,initial_min_index=initial_min_index,min_index=min_index,initial_max_index=initial_max_index,max_index=max_index,variable_length=variable_length,user_variable_length=user_variable_length,coroutine_state=coroutine_state,layout=layout,linear_coroutine=linear_coroutine,iterative=iterative})
    Store_request {store}->return (engine,Store {store=store})
    Collector_request {initial_min_index,initial_max_index}->return (engine,Collector {initial_min_index=initial_min_index,min_index=initial_min_index,initial_max_index=initial_max_index,max_index=initial_max_index,submit=DIM.empty})
    Visual_request {origin,matrix,red,green,blue,alpha,visual_request}->do
        (new_engine,visual)<-create_visual visual_request engine
        return (new_engine,Visual {origin=origin,matrix=matrix,red=red,green=green,blue=blue,alpha=alpha,visual=visual})
    Text_request {origin,matrix,width,height,article,calculate_width,calculate_typesetting,load}->let charset=to_charset article in let new_height=height/2 in if load
        then do
            new_engine<-update_font charset engine
            return (new_engine,let (new_article,max_y)=do_typesetting new_height calculate_typesetting (for_text new_engine.font new_engine.font_map article calculate_width) in Text {origin=origin,matrix=matrix,width=width,height=height,y=0,max_y=max_y+new_height,article=new_article,charset=charset,locked=False})
        else return (engine,let (new_article,max_y)=do_typesetting new_height calculate_typesetting (for_text engine.font engine.font_map article calculate_width) in Text {origin=origin,matrix=matrix,width=width,height=height,y=0,max_y=max_y+new_height,article=new_article,charset=charset,locked=False})
    Custom_widget_request {custom}->do
        (new_engine,new_custom)<-custom_widget_request custom engine
        return (new_engine,Custom_widget {custom=new_custom})

from_insert_widget_request::Custom_widget_request e=>Engine a b c d e->Int->Int->(Widget a b c d e->f)->DS.Seq (Insert (Widget_request a b c d e))->DIM.IntMap f->IO (DIM.IntMap f,Int,Int,Engine a b c d e)
from_insert_widget_request engine min_index max_index transform insert_widget_request intmap=case insert_widget_request of
    DS.Empty->return (intmap,max_index,min_index,engine)
    insert DS.:<| other_insert->case insert of
        Insert {insert_strategy,value}->do
            (new_engine,widget)<-create_widget value engine
            case insert_strategy of
                Min_strategy->from_insert_widget_request new_engine (min_index-1) max_index transform other_insert (intmap_insert min_index (transform widget) intmap)
                Max_strategy->from_insert_widget_request new_engine min_index (max_index+1) transform other_insert (intmap_insert max_index (transform widget) intmap)
                Index_strategy {seat}->if seat<=min_index then from_insert_widget_request new_engine (seat-1) max_index transform other_insert (intmap_insert seat (transform widget) intmap) else if max_index<=seat then from_insert_widget_request new_engine min_index (seat+1) transform other_insert (intmap_insert seat (transform widget) intmap) else from_insert_widget_request new_engine min_index max_index transform other_insert (intmap_insert seat (transform widget) intmap)

create_visual::Visual_request->Engine a b c d e->IO (Engine a b c d e,Visual)
create_visual visual_request engine=case visual_request of
    Triangle_request {first_point,second_point,third_point}->return (engine,Triangle {first_point=first_point,second_point=second_point,third_point=third_point})
    Convex_polygon_request {point}->return (engine,Convex_polygon {point=point})
    Regular_polygon_request {number,radius,angle}->return (engine,Regular_polygon {number=number,radius=radius,angle=angle})
    Picture_request {path}->create_picture path engine
    Large_picture_request {path}->do
        (texture,width,height)<-from_image engine.device engine.picture_transfer_buffer engine.picture_size path
        return (engine {album=intmap_insert engine.album_id (Album {width=width,height=height,texture=texture}) engine.album,album_id=engine.album_id+1},Large_picture {width=fromIntegral width,height=fromIntegral height,album_id=engine.album_id})
    Atlas_request {clip_request,path}->create_atlas 0 clip_request path engine
    Large_atlas_request {clip_request,path}->do
        (texture,width,height)<-from_image engine.device engine.picture_transfer_buffer engine.picture_size path
        return (engine {album=intmap_insert engine.album_id (Album {width=width,height=height,texture=texture}) engine.album,album_id=engine.album_id+1},Large_atlas {clip=DVS.fromListN (DS.length clip_request) (map (create_large_atlas (fromIntegral width) (fromIntegral height)) (DF.toList clip_request)),album_id=engine.album_id,index=0})
    Animation_request {min_delay,width,height,padding,path}->create_animation min_delay width height padding path engine

do_image::(DW.Word32->DW.Word32->DW.Word32->DW.Word32->DW.Word32->DW.Word32->Visual)->String->Engine a b c d e->IO (Engine a b c d e,Visual)
do_image action path engine=do
    (texture,width,height)<-from_image engine.device engine.picture_transfer_buffer engine.picture_size path
    let (atlas,left,down,right,up)=atlas_insert width height engine.padding engine.atlas
    copy_texture engine.device texture engine.texture left down width height
    SDLF.sdl_release_gpu_texture engine.device texture
    return (engine {atlas=atlas},action width height left down right up)

create_picture::String->Engine a b c d e->IO (Engine a b c d e,Visual)
create_picture path engine=do_image (\width height left down right up->Picture {width=fromIntegral width,height=fromIntegral height,min_u=fromIntegral left*engine.reciprocal_width,min_v=fromIntegral down*engine.reciprocal_height,max_u=fromIntegral right*engine.reciprocal_width,max_v=fromIntegral up*engine.reciprocal_height,path=path,locked=False}) path engine

create_atlas::Int->DS.Seq Clip_request->String->Engine a b c d e->IO (Engine a b c d e,Visual)
create_atlas index clip_request path engine=do_image (\width height left down right up->Atlas {clip_request=clip_request,path=path,clip=DVS.fromListN (DS.length clip_request) (map (create_atlas_a (fromIntegral width) (fromIntegral height) (fromIntegral (left+right)/2) (fromIntegral (down+up)/2) engine.reciprocal_width engine.reciprocal_height) (DF.toList clip_request)),index=index,locked=False}) path engine

create_atlas_a::FCT.CFloat->FCT.CFloat->FCT.CFloat->FCT.CFloat->FCT.CFloat->FCT.CFloat->Clip_request->Clip
create_atlas_a width height this_x this_y reciprocal_width reciprocal_height clip_request=case clip_request of
    Clip_request {x,y,min_u,min_v,max_u,max_v}->Clip {x=x,y=y,width=width*(max_u-min_u)/2,height=height*(max_v-min_v)/2,min_u=(this_x+min_u*width/2)*reciprocal_width,min_v=(this_y-max_v*height/2)*reciprocal_height,max_u=(this_x+max_u*width/2)*reciprocal_width,max_v=(this_y-min_v*height/2)*reciprocal_height}

create_large_atlas::FCT.CFloat->FCT.CFloat->Clip_request->Clip
create_large_atlas width height clip_request=case clip_request of
    Clip_request {x,y,min_u,min_v,max_u,max_v}->Clip {x=x,y=y,width=width*(max_u-min_u)/2,height=height*(max_v-min_v)/2,min_u=(1+min_u)/2,min_v=(1-max_v)/2,max_u=(1+max_u)/2,max_v=(1-min_v)/2}

create_animation::FCT.CFloat->DW.Word32->DW.Word32->Int->String->Engine a b c d e->IO (Engine a b c d e,Visual)
create_animation min_delay width height padding path engine=with_string path $ \this_path->do
    ptr_animation<-SDLF.img_load_animation this_path
    catch_null ptr_animation
    animation<-FS.peek ptr_animation
    case animation of
        SDLI.IMG_Animation {img_w,img_h,img_count,img_frames,img_delays}->let new_width=fromIntegral width in let new_height=fromIntegral height in let size=4*new_width*new_height in do
            CM.when (engine.picture_size<fromIntegral size) (EE.quick_error "create_animation" 0)
            let frame_width=fromIntegral img_w
            let frame_height=fromIntegral img_h
            let pack_width=frame_width+2*padding
            let pack_height=frame_height+2*padding
            let width_number=div new_width pack_width
            let height_number=div new_height pack_height
            let number=width_number*height_number
            CM.when (number==0) (EE.quick_error "create_animation" 1)
            let count=fromIntegral img_count
            delay<-DVS.generateM count (fmap (\this_delay->max min_delay (fromIntegral this_delay*millisecond)) . FS.peekElemOff img_delays)
            new_engine<-create_animation_a img_frames width height padding new_width size frame_width frame_height pack_width pack_height width_number number count 0 engine.album_id engine
            SDLF.img_free_animation ptr_animation
            return (new_engine,Animation {delay=delay,moment=0,frame_width=fromIntegral frame_width,frame_height=fromIntegral frame_height,width=fromIntegral width,height=fromIntegral height,padding=fromIntegral padding,width_number=width_number,height_number=height_number,album_number=div (count+number-1) number,album_id=engine.album_id,count=count,index=0})

create_animation_a::FP.Ptr (FP.Ptr SDLT.SDL_Surface)->DW.Word32->DW.Word32->Int->Int->Int->Int->Int->Int->Int->Int->Int->Int->Int->Int->Engine a b c d e->IO (Engine a b c d e)
create_animation_a frame width height padding this_width size frame_width frame_height pack_width pack_height width_number number count index album_id engine=if count<=index then return engine else do
    texture<-upload_texture engine.device engine.picture_transfer_buffer width height (create_animation_b frame padding this_width size frame_width frame_height pack_width pack_height width_number number count index)
    create_animation_a frame width height padding this_width size frame_width frame_height pack_width pack_height width_number number count (index+number) (album_id+1) (engine {album=intmap_insert album_id (Album {width=width,height=height,texture=texture}) engine.album,album_id=album_id+1})

create_animation_b::FP.Ptr (FP.Ptr SDLT.SDL_Surface)->Int->Int->Int->Int->Int->Int->Int->Int->Int->Int->Int->FP.Ptr ()->IO ()
create_animation_b frame padding width size frame_width frame_height pack_width pack_height width_number number count index map_transfer_buffer=do
    FMU.fillBytes (FP.castPtr map_transfer_buffer) 0 size
    CM.forM_ [0..min number (count-index)-1] $ \this_index->do
        surface_ptr<-FS.peekElemOff frame (index+this_index)
        surface<-SDLF.sdl_convert_surface surface_ptr SDLI.sdl_pixelformat_rgba32
        catch_null surface
        pitch<-SDLI.sdl_surface_pitch_peek surface
        pixel<-SDLI.sdl_surface_pixels_peek surface
        CM.forM_ [0..frame_height-1] $ \y->FMU.copyBytes (FP.plusPtr map_transfer_buffer (((div this_index width_number*pack_height+padding+y)*width+mod this_index width_number*pack_width+padding)*4)) (FP.plusPtr pixel (y*fromIntegral pitch)) (frame_width*4)
        SDLF.sdl_destroy_surface surface

remove_leaf::Custom_widget d=>Int->Engine a b c d e->IO (Engine a b c d e)
remove_leaf leaf_id engine=let (leaf,projection)=intmap_delete_lookup leaf_id engine.leaf in case projection of
    Without {ancestry_id,object}->remove_leaf_a ancestry_id object leaf leaf_id engine
    With {ancestry_id,object}->remove_leaf_a ancestry_id object leaf leaf_id engine

remove_leaf_a::Custom_widget d=>DS.Seq Int->Widget a b c d e->DIM.IntMap (Projection a b c d e)->Int->Engine a b c d e->IO (Engine a b c d e)
remove_leaf_a ancestry_id object leaf leaf_id engine=case ancestry_id of
    DS.Empty->remove_widget object (engine {leaf=leaf})
    _ DS.:|> node_id->remove_widget object (engine {leaf=leaf,node=intmap_update node_id (\node->node {leaf_child=intset_delete leaf_id node.leaf_child}) engine.node})

remove_widget::Custom_widget d=>Widget a b c d e->Engine a b c d e->IO (Engine a b c d e)
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
        Large_picture {album_id}->let (album,single_album)=intmap_delete_lookup album_id engine.album in do
            SDLF.sdl_release_gpu_texture engine.device single_album.texture
            return (engine {album=album})
        Large_atlas {album_id}->let (album,single_album)=intmap_delete_lookup album_id engine.album in do
            SDLF.sdl_release_gpu_texture engine.device single_album.texture
            return (engine {album=album})
        Animation {album_number,album_id}->do
            new_album<-CM.foldM (\album index->remove_animation engine.device index album_id album) engine.album [0..album_number-1]
            return (engine {album=new_album})
        _->return engine
    Custom_widget {custom}->custom_widget_remove custom engine
    _->return engine

remove_animation::FP.Ptr SDLT.SDL_GPUDevice->Int->Int->DIM.IntMap Album->IO (DIM.IntMap Album)
remove_animation device index album_id album=let (new_album,single_album)=intmap_delete_lookup (album_id+index) album in do
    SDLF.sdl_release_gpu_texture device single_album.texture
    return new_album