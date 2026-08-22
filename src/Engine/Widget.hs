{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Engine.Widget where

import Engine.Atlas
import Engine.Container
import Engine.Coroutine
import Engine.Projection
import Engine.Selector
import Engine.Text
import Engine.Type
import Engine.Underlying
import qualified SDL.Function as SDLF
import qualified SDL.Include as SDLI
import qualified SDL.Type as SDLT
import qualified Error.Error as EE
import qualified Control.Monad as CM
import qualified Data.Bits as DB
import qualified Data.Foldable as DF
import qualified Data.IntMap as DIM
import qualified Data.IntSet as DIS
import qualified Data.Sequence as DS
import qualified Data.Vector as DV
import qualified Data.Vector.Mutable as DVM
import qualified Data.Vector.Storable as DVS
import qualified Data.Word as DW
import qualified Foreign.C.Types as FCT
import qualified Foreign.Marshal.Utils as FMU
import qualified Foreign.Ptr as FP
import qualified Foreign.Storable as FS

from_same_insert_widget::Int->DS.Seq Insert_strategy->Widget a b c d e->Engine a b c d e->Engine a b c d e
from_same_insert_widget leaf_id insert_widget_strategy widget engine=engine {leaf=int_map_update leaf_id (update_projection_object (from_same_insert_widget_a insert_widget_strategy widget)) engine.leaf}

from_same_insert_widget_a::DS.Seq Insert_strategy->Widget a b c d e->Widget a b c d e->Widget a b c d e
from_same_insert_widget_a insert_widget_strategy widget this_widget=case this_widget of
    Group {initial_min_index,min_index,initial_max_index,max_index,index,group_widget}->let (new_group_widget,new_max_index,new_min_index)=from_same_insert_widget_b min_index max_index insert_widget_strategy widget group_widget in Group {initial_min_index=initial_min_index,min_index=new_min_index,initial_max_index=initial_max_index,max_index=new_max_index,index=index,group_widget=new_group_widget}
    Coroutine {index,initial_min_index,min_index,initial_max_index,max_index,variable_size,user_variable_size,coroutine_state,layout,linear_coroutine,iterative}->let (new_coroutine_state,new_max_index,new_min_index)=from_same_insert_widget_b min_index max_index insert_widget_strategy (init_coroutine_state variable_size user_variable_size widget) coroutine_state in Coroutine {index=index,initial_min_index=initial_min_index,min_index=new_min_index,initial_max_index=initial_max_index,max_index=new_max_index,variable_size=variable_size,user_variable_size=user_variable_size,coroutine_state=new_coroutine_state,layout=layout,linear_coroutine=linear_coroutine,iterative=iterative}
    _->EE.empty_error

from_same_insert_widget_b::Int->Int->DS.Seq Insert_strategy->a->DIM.IntMap a->(DIM.IntMap a,Int,Int)
from_same_insert_widget_b min_index max_index insert_widget_strategy value int_map=case insert_widget_strategy of
    DS.Empty->(int_map,max_index,min_index)
    insert_strategy DS.:<| other_insert_strategy->case insert_strategy of
        Min_strategy->from_same_insert_widget_b (min_index-1) max_index other_insert_strategy value (int_map_insert min_index value int_map)
        Max_strategy->from_same_insert_widget_b min_index (max_index+1) other_insert_strategy value (int_map_insert max_index value int_map)
        Index_strategy {seat}->if seat<=min_index then from_same_insert_widget_b (seat-1) max_index other_insert_strategy value (int_map_insert seat value int_map) else if max_index<=seat then from_same_insert_widget_b min_index (seat+1) other_insert_strategy value (int_map_insert seat value int_map) else from_same_insert_widget_b min_index max_index other_insert_strategy value (int_map_insert seat value int_map)

from_insert_widget::Int->DS.Seq (Insert (Widget a b c d e))->Engine a b c d e->Engine a b c d e
from_insert_widget leaf_id insert_widget engine=engine {leaf=int_map_update leaf_id (update_projection_object (from_insert_widget_a insert_widget)) engine.leaf}

from_insert_widget_a::DS.Seq (Insert (Widget a b c d e))->Widget a b c d e->Widget a b c d e
from_insert_widget_a insert_widget widget=case widget of
    Group {initial_min_index,min_index,initial_max_index,max_index,index,group_widget}->let (new_group_widget,new_max_index,new_min_index)=from_insert_widget_b min_index max_index id insert_widget group_widget in Group {initial_min_index=initial_min_index,min_index=new_min_index,initial_max_index=initial_max_index,max_index=new_max_index,index=index,group_widget=new_group_widget}
    Coroutine {index,initial_min_index,min_index,initial_max_index,max_index,variable_size,user_variable_size,coroutine_state,layout,linear_coroutine,iterative}->let (new_coroutine_state,new_max_index,new_min_index)=from_insert_widget_b min_index max_index (init_coroutine_state variable_size user_variable_size) insert_widget coroutine_state in Coroutine {index=index,initial_min_index=initial_min_index,min_index=new_min_index,initial_max_index=initial_max_index,max_index=new_max_index,variable_size=variable_size,user_variable_size=user_variable_size,coroutine_state=new_coroutine_state,layout=layout,linear_coroutine=linear_coroutine,iterative=iterative}
    _->EE.empty_error

from_insert_widget_b::Int->Int->(Widget a b c d e->f)->DS.Seq (Insert (Widget a b c d e))->DIM.IntMap f->(DIM.IntMap f,Int,Int)
from_insert_widget_b min_index max_index transform insert_widget int_map=case insert_widget of
    DS.Empty->(int_map,max_index,min_index)
    insert DS.:<| other_insert->case insert of
        Insert {insert_strategy,value}->case insert_strategy of
            Min_strategy->from_insert_widget_b (min_index-1) max_index transform other_insert (int_map_insert min_index (transform value) int_map)
            Max_strategy->from_insert_widget_b min_index (max_index+1) transform other_insert (int_map_insert max_index (transform value) int_map)
            Index_strategy {seat}->if seat<=min_index then from_insert_widget_b (seat-1) max_index transform other_insert (int_map_insert seat (transform value) int_map) else if max_index<=seat then from_insert_widget_b min_index (seat+1) transform other_insert (int_map_insert seat (transform value) int_map) else from_insert_widget_b min_index max_index transform other_insert (int_map_insert seat (transform value) int_map)

create_leaf::Custom_widget_request e=>Int->Maybe Int->Widget_request a b c d e->Engine a b c d e->IO (Engine a b c d e)
create_leaf leaf_id maybe_father_id widget_request engine=do
    (new_engine,widget)<-create_widget leaf_id widget_request engine
    case maybe_father_id of
        Nothing->return (new_engine {leaf=int_map_insert leaf_id (Without {ancestry_id=DS.empty,object=widget}) new_engine.leaf})
        Just father_id->let (node,single_node)=int_map_update_lookup father_id (\this_node->this_node {leaf_child=int_set_insert leaf_id this_node.leaf_child}) new_engine.node in return (new_engine {leaf=int_map_insert leaf_id (Without {ancestry_id=single_node.ancestry_id DS.|> father_id,object=widget}) new_engine.leaf,node=node})

create_widget::Custom_widget_request e=>Int->Widget_request a b c d e->Engine a b c d e->IO (Engine a b c d e,Widget a b c d e)
create_widget leaf_id this_widget_request engine=case this_widget_request of
    Group_request {initial_min_index,initial_max_index,index,insert_widget_request}->do
        (new_engine,group_widget,max_index,min_index)<-from_insert_widget_request leaf_id initial_min_index initial_max_index id insert_widget_request DIM.empty engine
        return (new_engine,Group {initial_min_index=initial_min_index,min_index=min_index,initial_max_index=initial_max_index,max_index=max_index,index=index,group_widget=group_widget})
    Vector_request {index,vector_widget_request}->let size=DS.length vector_widget_request in do
        vector_widget<-DVM.new size
        new_engine<-create_vector_widget leaf_id 0 vector_widget vector_widget_request engine
        new_vector_widget<-DV.unsafeFreeze vector_widget
        return (new_engine,Vector {index=index,vector_widget=new_vector_widget})
    Trigger_request {next,trigger}->return (engine,Trigger {next=next,trigger=trigger})
    Io_trigger_request {next,io_trigger}->return (engine,Io_trigger {next=next,io_trigger=io_trigger})
    Mix_trigger_request {next,mix_trigger,order}->return (engine,Mix_trigger {next=next,mix_trigger=mix_trigger,order=order})
    Widget_trigger_request {next,widget_trigger,widget_request}->do
        (new_engine,widget)<-create_widget leaf_id widget_request engine
        return (new_engine,Widget_trigger {next=next,widget_trigger=widget_trigger,widget=widget})
    Widget_io_trigger_request {next,widget_io_trigger,widget_request}->do
        (new_engine,widget)<-create_widget leaf_id widget_request engine
        return (new_engine,Widget_io_trigger {next=next,widget_io_trigger=widget_io_trigger,widget=widget})
    Widget_mix_trigger_request {next,widget_mix_trigger,order,widget_request}->do
        (new_engine,widget)<-create_widget leaf_id widget_request engine
        return (new_engine,Widget_mix_trigger {next=next,widget_mix_trigger=widget_mix_trigger,order=order,widget=widget})
    Coroutine_request {index,initial_min_index,initial_max_index,insert_widget_request,raw_coroutine,iterative}->let (int,coroutine_sequence,_)=raw_coroutine.iterator 0 in let (linear_coroutine,layout,variable_size,user_variable_size)=from_coroutine (to_coroutine coroutine_sequence) int in do
        (new_engine,coroutine_state,max_index,min_index)<-from_insert_widget_request leaf_id initial_min_index initial_max_index (init_coroutine_state variable_size user_variable_size) insert_widget_request DIM.empty engine
        return (new_engine,Coroutine {index=index,initial_min_index=initial_min_index,min_index=min_index,initial_max_index=initial_max_index,max_index=max_index,variable_size=variable_size,user_variable_size=user_variable_size,coroutine_state=coroutine_state,layout=layout,linear_coroutine=linear_coroutine,iterative=iterative})
    Store_request {store}->return (engine,Store {store=store})
    Collector_request {initial_min_index,initial_max_index}->return (engine,Collector {initial_min_index=initial_min_index,min_index=initial_min_index,initial_max_index=initial_max_index,max_index=initial_max_index,submit=DIM.empty})
    Visual_request {visual_request}->do
        (new_engine,visual)<-create_visual visual_request engine
        return (new_engine,Visual {visual=visual})
    Group_visual_request {arrange,collect_order,group_visual_request}->do
        (new_engine,group_visual)<-int_map_monad_action (\_ visual_request this_engine->create_visual visual_request this_engine) group_visual_request engine
        return (new_engine,Group_visual {arrange=arrange,collect_order=collect_order,group_visual=group_visual})
    Vector_visual_request {arrange,collect_order,vector_visual_request}->let size=DV.length vector_visual_request in do
        new_vector_visual<-DVM.new size
        new_engine<-vector_io_map (size-1) (\_ visual_request this_engine->create_visual visual_request this_engine) vector_visual_request new_vector_visual engine
        new_new_vector_visual<-DV.unsafeFreeze new_vector_visual
        return (new_engine,Vector_visual {arrange=arrange,collect_order=collect_order,vector_visual=new_new_vector_visual,size=size})
    Custom_widget_request {custom}->do
        (new_engine,new_custom)<-custom_widget_request custom engine
        return (new_engine,Custom_widget {custom=new_custom})

create_vector_widget::Custom_widget_request e=>Int->Int->DVM.IOVector (Widget a b c d e)->DS.Seq (Widget_request a b c d e)->Engine a b c d e->IO (Engine a b c d e)
create_vector_widget leaf_id index vector_widget vector_widget_request engine=case vector_widget_request of
    DS.Empty->return engine
    (widget_request DS.:<| other_widget_request)->do
        (new_engine,widget)<-create_widget leaf_id widget_request engine
        DVM.unsafeWrite vector_widget index widget
        create_vector_widget leaf_id (index+1) vector_widget other_widget_request new_engine

from_insert_widget_request::Custom_widget_request e=>Int->Int->Int->(Widget a b c d e->f)->DS.Seq (Insert (Widget_request a b c d e))->DIM.IntMap f->Engine a b c d e->IO (Engine a b c d e,DIM.IntMap f,Int,Int)
from_insert_widget_request leaf_id min_index max_index transform insert_widget_request int_map engine=case insert_widget_request of
    DS.Empty->return (engine,int_map,max_index,min_index)
    insert DS.:<| other_insert->case insert of
        Insert {insert_strategy,value}->do
            (new_engine,widget)<-create_widget leaf_id value engine
            case insert_strategy of
                Min_strategy->from_insert_widget_request leaf_id (min_index-1) max_index transform other_insert (int_map_insert min_index (transform widget) int_map) new_engine
                Max_strategy->from_insert_widget_request leaf_id min_index (max_index+1) transform other_insert (int_map_insert max_index (transform widget) int_map) new_engine
                Index_strategy {seat}->if seat<=min_index then from_insert_widget_request leaf_id (seat-1) max_index transform other_insert (int_map_insert seat (transform widget) int_map) new_engine else if max_index<=seat then from_insert_widget_request leaf_id min_index (seat+1) transform other_insert (int_map_insert seat (transform widget) int_map) new_engine else from_insert_widget_request leaf_id min_index max_index transform other_insert (int_map_insert seat (transform widget) int_map) new_engine

create_visual::Visual_request->Engine a b c d e->IO (Engine a b c d e,Visual)
create_visual visual_request engine=case visual_request of
    Rectangle_request {arrange,rectangle_width,rectangle_height}->return (engine,Rectangle {arrange=arrange,half_width=rectangle_width/2,half_height=rectangle_height/2})
    Triangle_request {arrange,first_point,second_point,third_point}->return (engine,Triangle {arrange=arrange,first_point=first_point,second_point=second_point,third_point=third_point})
    Convex_polygon_request {arrange,point_set}->return (engine,Convex_polygon {arrange=arrange,point_set=point_set})
    Regular_polygon_request {arrange,number,radius,angle}->return (engine,Regular_polygon {arrange=arrange,number=number,radius=radius,angle=angle})
    Picture_request {arrange,path}->create_picture arrange path engine
    Large_picture_request {arrange,path}->do
        (texture,width,height)<-from_image engine.device engine.picture_transfer_buffer engine.max_picture_size path
        return (engine {album=int_map_insert engine.album_id (Album {width=width,height=height,texture=texture}) engine.album,album_id=engine.album_id+1},Large_picture {arrange=arrange,half_width=fromIntegral width/2,half_height=fromIntegral height/2,album_id=engine.album_id})
    Atlas_request {arrange,clip_request,path}->create_atlas arrange clip_request path 0 engine
    Large_atlas_request {arrange,clip_request,path}->do
        (texture,width,height)<-from_image engine.device engine.picture_transfer_buffer engine.max_picture_size path
        return (engine {album=int_map_insert engine.album_id (Album {width=width,height=height,texture=texture}) engine.album,album_id=engine.album_id+1},let size=DS.length clip_request in Large_atlas {arrange=arrange,clip=DVS.fromListN size (map (create_large_atlas (fromIntegral width) (fromIntegral height)) (DF.toList clip_request)),album_id=engine.album_id,index=0})
    Animation_request {arrange,min_delay,animation_width,animation_height,padding,path}->create_animation arrange min_delay animation_width animation_height padding path engine
    Text_request {arrange,text_width,text_height,article,calculate_width,calculate_typesetting,load}->let charset=to_charset article in let half_height=text_height/2 in if load
        then do
            new_engine<-update_font charset engine
            return (new_engine,let (new_article,max_y)=do_typesetting half_height calculate_typesetting (for_text new_engine.font new_engine.font_map article calculate_width) in Text {arrange=arrange,half_width=text_width/2,half_height=half_height,current_y=0,min_y=0,max_y=max_y-half_height,article=new_article,charset=charset,locked=False})
        else return (engine,let (new_article,max_y)=do_typesetting half_height calculate_typesetting (for_text engine.font engine.font_map article calculate_width) in Text {arrange=arrange,half_width=text_width/2,half_height=half_height,current_y=0,min_y=0,max_y=max_y-half_height,article=new_article,charset=charset,locked=False})
    Canvas_request {arrange,canvas_width,canvas_height,maybe_canvas_id}->do
        texture<-FMU.with (SDLI.SDL_GPUTextureCreateInfo {sdl_type=SDLI.sdl_gpu_texturetype_2d,sdl_format=SDLI.sdl_gpu_textureformat_r8g8b8a8_unorm,sdl_usage=SDLI.sdl_gpu_textureusage_sampler DB..|. SDLI.sdl_gpu_textureusage_color_target,sdl_width=canvas_width,sdl_height=canvas_height,sdl_layer_count_or_depth=1,sdl_num_levels=1,sdl_sample_count=SDLI.sdl_gpu_samplecount_1}) (return_catch_null . SDLF.sdl_create_gpu_texture engine.device)
        temporary_texture<-FMU.with (SDLI.SDL_GPUTextureCreateInfo {sdl_type=SDLI.sdl_gpu_texturetype_2d,sdl_format=SDLI.sdl_gpu_textureformat_r8g8b8a8_unorm,sdl_usage=SDLI.sdl_gpu_textureusage_sampler DB..|. SDLI.sdl_gpu_textureusage_color_target,sdl_width=canvas_width,sdl_height=canvas_height,sdl_layer_count_or_depth=1,sdl_num_levels=1,sdl_sample_count=SDLI.sdl_gpu_samplecount_1}) (return_catch_null . SDLF.sdl_create_gpu_texture engine.device)
        case maybe_canvas_id of
            Nothing->return (engine {canvas=int_map_insert engine.canvas_id (Bound_canvas {texture=texture,temporary_texture=temporary_texture}) engine.canvas,canvas_id=engine.canvas_id+1},Canvas {arrange=arrange,canvas_width=canvas_width,canvas_height=canvas_height,half_width=fromIntegral canvas_width/2,half_height=fromIntegral canvas_height/2,canvas_id=engine.canvas_id})
            Just canvas_id->return (engine {canvas=int_map_insert canvas_id (Bound_canvas {texture=texture,temporary_texture=temporary_texture}) engine.canvas,canvas_id=max canvas_id engine.canvas_id+1},Canvas {arrange=arrange,canvas_width=canvas_width,canvas_height=canvas_height,half_width=fromIntegral canvas_width/2,half_height=fromIntegral canvas_height/2,canvas_id=canvas_id})

do_image::(DW.Word32->DW.Word32->DW.Word32->DW.Word32->DW.Word32->DW.Word32->Visual)->String->Engine a b c d e->IO (Engine a b c d e,Visual)
do_image action path engine=do
    (texture,width,height)<-from_image engine.device engine.picture_transfer_buffer engine.max_picture_size path
    let (atlas,left,down,right,up)=atlas_insert width height engine.padding engine.atlas
    copy_texture engine.device texture engine.texture left down width height
    SDLF.sdl_release_gpu_texture engine.device texture
    return (engine {atlas=atlas},action width height left down right up)

create_picture::Arrange->String->Engine a b c d e->IO (Engine a b c d e,Visual)
create_picture arrange path engine=do_image (\width height left down right up->Picture {arrange=arrange,half_width=fromIntegral width/2,half_height=fromIntegral height/2,min_u=scaleFloat (-engine.exponent_width) (fromIntegral left),min_v=scaleFloat (-engine.exponent_height) (fromIntegral down),max_u=scaleFloat (-engine.exponent_width) (fromIntegral right),max_v=scaleFloat (-engine.exponent_height) (fromIntegral up),path=path,locked=False}) path engine

create_atlas::Arrange->DS.Seq Clip_request->String->Int->Engine a b c d e->IO (Engine a b c d e,Visual)
create_atlas arrange clip_request path index engine=do_image (\width height left down right up->let size=DS.length clip_request in Atlas {arrange=arrange,clip_request=clip_request,path=path,clip=DVS.fromListN size (map (create_atlas_a (fromIntegral width) (fromIntegral height) (fromIntegral (left+right)/2) (fromIntegral (down+up)/2) engine.exponent_width engine.exponent_height) (DF.toList clip_request)),index=index,locked=False}) path engine

create_atlas_a::FCT.CFloat->FCT.CFloat->FCT.CFloat->FCT.CFloat->Int->Int->Clip_request->Clip
create_atlas_a width height this_x this_y exponent_width exponent_height clip_request=case clip_request of
    Clip_request {x,y,min_u,min_v,max_u,max_v}->Clip {x=x,y=y,half_width=width*(max_u-min_u)/4,half_height=height*(max_v-min_v)/4,min_u=scaleFloat (-exponent_width) (this_x+min_u*width/2),min_v=scaleFloat (-exponent_height) (this_y-max_v*height/2),max_u=scaleFloat (-exponent_width) (this_x+max_u*width/2),max_v=scaleFloat (-exponent_height) (this_y-min_v*height/2)}

create_large_atlas::FCT.CFloat->FCT.CFloat->Clip_request->Clip
create_large_atlas width height clip_request=case clip_request of
    Clip_request {x,y,min_u,min_v,max_u,max_v}->Clip {x=x,y=y,half_width=width*(max_u-min_u)/4,half_height=height*(max_v-min_v)/4,min_u=(1+min_u)/2,min_v=(1-max_v)/2,max_u=(1+max_u)/2,max_v=(1-min_v)/2}

create_animation::Arrange->FCT.CFloat->DW.Word32->DW.Word32->Int->String->Engine a b c d e->IO (Engine a b c d e,Visual)
create_animation arrange min_delay width height padding path engine=with_string path $ \this_path->do
    ptr_animation<-SDLF.img_load_animation this_path
    catch_null ptr_animation
    animation<-FS.peek ptr_animation
    case animation of
        SDLI.IMG_Animation {img_w,img_h,img_count,img_frames,img_delays}->let new_width=fromIntegral width in let new_height=fromIntegral height in let size=4*new_width*new_height in do
            CM.when (engine.max_picture_size<fromIntegral size) EE.empty_error
            let frame_width=fromIntegral img_w
            let frame_height=fromIntegral img_h
            let pack_width=frame_width+2*padding
            let pack_height=frame_height+2*padding
            let width_number=div new_width pack_width
            let height_number=div new_height pack_height
            let number=width_number*height_number
            CM.when (number==0) EE.empty_error
            let count=fromIntegral img_count
            delay<-DVS.generateM count (fmap (\this_delay->max min_delay (fromIntegral this_delay*millisecond)) . FS.peekElemOff img_delays)
            new_engine<-create_animation_a img_frames width height padding new_width size frame_width frame_height pack_width pack_height width_number number count 0 engine.album_id engine
            SDLF.img_free_animation ptr_animation
            return (new_engine,Animation {arrange=arrange,delay=delay,moment=0,half_width=fromIntegral frame_width/2,half_height=fromIntegral frame_height/2,reciprocal_width=1/fromIntegral width,reciprocal_height=1/fromIntegral height,padding=fromIntegral padding,width_number=width_number,height_number=height_number,album_number=div (count+number-1) number,album_id=engine.album_id,count=count,index=0})

create_animation_a::FP.Ptr (FP.Ptr SDLT.SDL_Surface)->DW.Word32->DW.Word32->Int->Int->Int->Int->Int->Int->Int->Int->Int->Int->Int->Int->Engine a b c d e->IO (Engine a b c d e)
create_animation_a frame width height padding this_width size frame_width frame_height pack_width pack_height width_number number count index album_id engine=if count<=index then return engine else do
    texture<-upload_texture engine.device engine.picture_transfer_buffer width height (create_animation_b frame padding this_width size frame_width frame_height pack_width pack_height width_number number count index)
    create_animation_a frame width height padding this_width size frame_width frame_height pack_width pack_height width_number number count (index+number) (album_id+1) (engine {album=int_map_insert album_id (Album {width=width,height=height,texture=texture}) engine.album,album_id=album_id+1})

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
remove_leaf leaf_id engine=let (leaf,projection)=int_map_delete_lookup leaf_id engine.leaf in case projection of
    Without {ancestry_id,object}->remove_leaf_a ancestry_id object leaf leaf_id engine
    With {ancestry_id,object}->remove_leaf_a ancestry_id object leaf leaf_id engine

remove_leaf_a::Custom_widget d=>DS.Seq Int->Widget a b c d e->DIM.IntMap (Projection a b c d e)->Int->Engine a b c d e->IO (Engine a b c d e)
remove_leaf_a ancestry_id object leaf leaf_id engine=case ancestry_id of
    DS.Empty->all_selector_monad_action remove_widget object (engine {leaf=leaf})
    _ DS.:|> father_id->all_selector_monad_action remove_widget object (engine {leaf=leaf,node=int_map_update father_id (\node->node {leaf_child=int_set_delete leaf_id node.leaf_child}) engine.node})

remove_widget::Custom_widget d=>Widget a b c d e->Engine a b c d e->IO (Engine a b c d e)
remove_widget widget engine=case widget of
    Visual {visual}->remove_visual visual engine
    Group_visual {group_visual}->DF.foldlM (flip remove_visual) engine group_visual
    Vector_visual {vector_visual}->DF.foldlM (flip remove_visual) engine vector_visual
    Custom_widget {custom}->custom_widget_remove custom engine
    _->return engine

remove_visual::Visual->Engine a b c d e->IO (Engine a b c d e)
remove_visual visual engine=case visual of
    Large_picture {album_id}->let (album,single_album)=int_map_delete_lookup album_id engine.album in do
        SDLF.sdl_release_gpu_texture engine.device single_album.texture
        return (engine {album=album})
    Large_atlas {album_id}->let (album,single_album)=int_map_delete_lookup album_id engine.album in do
        SDLF.sdl_release_gpu_texture engine.device single_album.texture
        return (engine {album=album})
    Animation {album_number,album_id}->do
        new_album<-CM.foldM (\album index->remove_animation engine.device index album_id album) engine.album [0..album_number-1]
        return (engine {album=new_album})
    Canvas {canvas_id}->let (canvas,single_canvas)=int_map_delete_lookup canvas_id engine.canvas in do
        clean_canvas engine.device single_canvas
        return (engine {canvas=canvas})
    _->return engine

clean_canvas::FP.Ptr SDLT.SDL_GPUDevice->Canvas->IO ()
clean_canvas device canvas=case canvas of
    Free_canvas {texture,temporary_texture}->do
        SDLF.sdl_release_gpu_texture device texture
        SDLF.sdl_release_gpu_texture device temporary_texture
    Bound_canvas {texture,temporary_texture}->do
        SDLF.sdl_release_gpu_texture device texture
        SDLF.sdl_release_gpu_texture device temporary_texture

remove_animation::FP.Ptr SDLT.SDL_GPUDevice->Int->Int->DIM.IntMap Album->IO (DIM.IntMap Album)
remove_animation device index album_id album=let (new_album,single_album)=int_map_delete_lookup (album_id+index) album in do
    SDLF.sdl_release_gpu_texture device single_album.texture
    return new_album

create_node::Int->Maybe Int->(Engine a b c d e->Event b->Event b)->(Event b->Engine a b c d e->Widget a b c d e->Widget a b c d e)->Engine a b c d e->Engine a b c d e
create_node node_id maybe_father_id event_transform widget_transform engine=case maybe_father_id of
    Nothing->engine {node=int_map_insert node_id (Node {ancestry_id=DS.empty,leaf_child=DIS.empty,node_child=DIS.empty,event_transform=event_transform,widget_transform=widget_transform}) engine.node}
    Just father_id->let (node,single_node)=int_map_update_lookup father_id (\this_node->this_node {node_child=int_set_insert node_id this_node.node_child}) engine.node in engine {node=int_map_insert node_id (Node {ancestry_id=single_node.ancestry_id DS.|> father_id,leaf_child=DIS.empty,node_child=DIS.empty,event_transform=event_transform,widget_transform=widget_transform}) node}

remove_node::Custom_widget d=>Int->Engine a b c d e->IO (Engine a b c d e)
remove_node node_id engine=let (node,single_node)=int_map_delete_lookup node_id engine.node in case single_node.ancestry_id of
    DS.Empty->remove_node_a single_node.leaf_child single_node.node_child (engine {node=node})
    _ DS.:|> father_id->remove_node_a single_node.leaf_child single_node.node_child (engine {node=int_map_update father_id (\this_node->this_node {node_child=int_set_delete node_id this_node.node_child}) node})

remove_node_a::Custom_widget d=>DIS.IntSet->DIS.IntSet->Engine a b c d e->IO (Engine a b c d e)
remove_node_a leaf_child node_child engine=do
    new_engine<-int_set_monad_fold remove_node_leaf leaf_child engine
    int_set_monad_fold remove_node_node node_child new_engine

remove_node_leaf::Custom_widget d=>Int->Engine a b c d e->IO (Engine a b c d e)
remove_node_leaf leaf_id engine=let (leaf,projection)=int_map_delete_lookup leaf_id engine.leaf in remove_widget (lookup_projection_object projection) (engine {leaf=leaf})

remove_node_node::Custom_widget d=>Int->Engine a b c d e->IO (Engine a b c d e)
remove_node_node node_id engine=let (node,single_node)=int_map_delete_lookup node_id engine.node in remove_node_a single_node.leaf_child single_node.node_child (engine {node=node})

{-# INLINE from_same_insert_widget #-}
{-# INLINE from_same_insert_widget_a #-}
{-# INLINE from_same_insert_widget_b #-}
{-# INLINE from_insert_widget #-}
{-# INLINE from_insert_widget_a #-}
{-# INLINE from_insert_widget_b #-}
{-# INLINE do_image #-}
{-# INLINE create_picture #-}
{-# INLINE create_atlas #-}
{-# INLINE create_atlas_a #-}
{-# INLINE create_large_atlas #-}
{-# INLINE clean_canvas #-}
{-# INLINE create_node #-}