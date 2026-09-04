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
import qualified Error.Function as EF
import qualified Error.Type as ET
import qualified Control.Monad as CM
import qualified Data.Bits as DB
import qualified Data.IntMap as DIM
import qualified Data.IntSet as DIS
import qualified Data.Sequence as DS
import qualified Data.Text as DT
import qualified Data.Vector as DV
import qualified Data.Vector.Mutable as DVM
import qualified Data.Vector.Storable as DVS
import qualified Data.Word as DW
import qualified Foreign.C.Types as FCT
import qualified Foreign.Marshal.Utils as FMU
import qualified Foreign.Ptr as FP
import qualified Foreign.Storable as FS

from_same_insert_widget::ET.Has_call_stack=>Int->DS.Seq Insert_strategy->Widget a->Engine a->Engine a
from_same_insert_widget leaf_id insert_widget_strategy widget engine=engine {leaf=int_map_update leaf_id (update_projection_object (from_same_insert_widget_a insert_widget_strategy widget)) engine.leaf}

from_same_insert_widget_a::ET.Has_call_stack=>DS.Seq Insert_strategy->Widget a->Widget a->Widget a
from_same_insert_widget_a insert_widget_strategy widget this_widget=case this_widget of
    Group {initial_min_index,min_index,initial_max_index,max_index,index,group_widget}->let (new_group_widget,new_max_index,new_min_index)=from_same_insert_widget_b min_index max_index insert_widget_strategy widget group_widget in Group {initial_min_index=initial_min_index,min_index=new_min_index,initial_max_index=initial_max_index,max_index=new_max_index,index=index,group_widget=new_group_widget}
    Coroutine {initial_min_index,min_index,initial_max_index,max_index,index,variable_size,user_variable_size,coroutine_state,layout,linear_coroutine,iterative}->let (new_coroutine_state,new_max_index,new_min_index)=from_same_insert_widget_b min_index max_index insert_widget_strategy (init_coroutine_state variable_size user_variable_size widget) coroutine_state in Coroutine {initial_min_index=initial_min_index,min_index=new_min_index,initial_max_index=initial_max_index,max_index=new_max_index,index=index,variable_size=variable_size,user_variable_size=user_variable_size,coroutine_state=new_coroutine_state,layout=layout,linear_coroutine=linear_coroutine,iterative=iterative}
    _->EF.empty_error

from_same_insert_widget_b::ET.Has_call_stack=>Int->Int->DS.Seq Insert_strategy->a->DIM.IntMap a->(DIM.IntMap a,Int,Int)
from_same_insert_widget_b min_index max_index insert_widget_strategy value int_map=case insert_widget_strategy of
    DS.Empty->(int_map,max_index,min_index)
    insert_strategy DS.:<| other_insert_strategy->case insert_strategy of
        Min_strategy->from_same_insert_widget_b (min_index-1) max_index other_insert_strategy value (int_map_insert min_index value int_map)
        Max_strategy->from_same_insert_widget_b min_index (max_index+1) other_insert_strategy value (int_map_insert max_index value int_map)
        Index_strategy {seat}->if seat<=min_index then from_same_insert_widget_b (seat-1) max_index other_insert_strategy value (int_map_insert seat value int_map) else if max_index<=seat then from_same_insert_widget_b min_index (seat+1) other_insert_strategy value (int_map_insert seat value int_map) else from_same_insert_widget_b min_index max_index other_insert_strategy value (int_map_insert seat value int_map)

from_insert_widget::ET.Has_call_stack=>Int->DS.Seq (Insert (Widget a))->Engine a->Engine a
from_insert_widget leaf_id insert_widget engine=engine {leaf=int_map_update leaf_id (update_projection_object (from_insert_widget_a insert_widget)) engine.leaf}

from_insert_widget_a::ET.Has_call_stack=>DS.Seq (Insert (Widget a))->Widget a->Widget a
from_insert_widget_a insert_widget widget=case widget of
    Group {initial_min_index,min_index,initial_max_index,max_index,index,group_widget}->let (new_group_widget,new_max_index,new_min_index)=from_insert_widget_b min_index max_index id insert_widget group_widget in Group {initial_min_index=initial_min_index,min_index=new_min_index,initial_max_index=initial_max_index,max_index=new_max_index,index=index,group_widget=new_group_widget}
    Coroutine {initial_min_index,min_index,initial_max_index,max_index,index,variable_size,user_variable_size,coroutine_state,layout,linear_coroutine,iterative}->let (new_coroutine_state,new_max_index,new_min_index)=from_insert_widget_b min_index max_index (init_coroutine_state variable_size user_variable_size) insert_widget coroutine_state in Coroutine {initial_min_index=initial_min_index,min_index=new_min_index,initial_max_index=initial_max_index,max_index=new_max_index,index=index,variable_size=variable_size,user_variable_size=user_variable_size,coroutine_state=new_coroutine_state,layout=layout,linear_coroutine=linear_coroutine,iterative=iterative}
    _->EF.empty_error

from_insert_widget_b::ET.Has_call_stack=>Int->Int->(Widget a->b)->DS.Seq (Insert (Widget a))->DIM.IntMap b->(DIM.IntMap b,Int,Int)
from_insert_widget_b min_index max_index transform insert_widget int_map=case insert_widget of
    DS.Empty->(int_map,max_index,min_index)
    insert DS.:<| other_insert->case insert of
        Insert {insert_strategy,value}->case insert_strategy of
            Min_strategy->from_insert_widget_b (min_index-1) max_index transform other_insert (int_map_insert min_index (transform value) int_map)
            Max_strategy->from_insert_widget_b min_index (max_index+1) transform other_insert (int_map_insert max_index (transform value) int_map)
            Index_strategy {seat}->if seat<=min_index then from_insert_widget_b (seat-1) max_index transform other_insert (int_map_insert seat (transform value) int_map) else if max_index<=seat then from_insert_widget_b min_index (seat+1) transform other_insert (int_map_insert seat (transform value) int_map) else from_insert_widget_b min_index max_index transform other_insert (int_map_insert seat (transform value) int_map)

create_leaf::ET.Has_call_stack=>Custom a=>Int->Maybe Int->Widget_request a->Engine a->IO (Engine a)
create_leaf leaf_id maybe_father_id widget_request engine=do
    (new_engine,widget)<-create_widget leaf_id widget_request engine
    case maybe_father_id of
        Nothing->return (new_engine {leaf=int_map_insert leaf_id (Without {ancestry_id=DS.empty,object=widget}) new_engine.leaf})
        Just father_id->let (node,single_node)=int_map_update_lookup father_id (\this_node->this_node {leaf_child=int_set_insert leaf_id this_node.leaf_child}) new_engine.node in return (new_engine {leaf=int_map_insert leaf_id (Without {ancestry_id=single_node.ancestry_id DS.|> father_id,object=widget}) new_engine.leaf,node=node})

create_widget::ET.Has_call_stack=>Custom a=>Int->Widget_request a->Engine a->IO (Engine a,Widget a)
create_widget leaf_id this_widget_request engine=case this_widget_request of
    Group_request {initial_min_index,initial_max_index,index,insert_widget_request}->do
        (new_engine,group_widget,max_index,min_index)<-from_insert_widget_request leaf_id initial_min_index initial_max_index id insert_widget_request DIM.empty engine
        return (new_engine,Group {initial_min_index=initial_min_index,min_index=min_index,initial_max_index=initial_max_index,max_index=max_index,index=index,group_widget=group_widget})
    Vector_request {index,vector_widget_request}->do
        vector_widget<-DVM.new (DS.length vector_widget_request)
        new_engine<-create_vector_widget leaf_id 0 vector_widget vector_widget_request engine
        new_vector_widget<-DV.unsafeFreeze vector_widget
        return (new_engine,Vector {index=index,vector_widget=new_vector_widget})
    Trigger_request {next,trigger}->return (engine,Trigger {next=next,trigger=trigger})
    Io_trigger_request {next,io_trigger}->return (engine,Io_trigger {next=next,io_trigger=io_trigger})
    Mix_trigger_request {next,mix_trigger,order}->return (engine,Mix_trigger {next=next,mix_trigger=mix_trigger,order=order})
    Widget_trigger_request {next,widget_request,widget_trigger}->do
        (new_engine,widget)<-create_widget leaf_id widget_request engine
        return (new_engine,Widget_trigger {next=next,widget=widget,widget_trigger=widget_trigger})
    Widget_io_trigger_request {next,widget_request,widget_io_trigger}->do
        (new_engine,widget)<-create_widget leaf_id widget_request engine
        return (new_engine,Widget_io_trigger {next=next,widget=widget,widget_io_trigger=widget_io_trigger})
    Widget_mix_trigger_request {next,widget_request,widget_mix_trigger,order}->do
        (new_engine,widget)<-create_widget leaf_id widget_request engine
        return (new_engine,Widget_mix_trigger {next=next,widget=widget,widget_mix_trigger=widget_mix_trigger,order=order})
    Visual_trigger_request {next,visual_request,visual_trigger}->do
        (new_engine,visual)<-create_visual visual_request engine
        return (new_engine,Visual_trigger {next=next,visual=visual,visual_trigger=visual_trigger})
    Visual_io_trigger_request {next,visual_request,visual_io_trigger}->do
        (new_engine,visual)<-create_visual visual_request engine
        return (new_engine,Visual_io_trigger {next=next,visual=visual,visual_io_trigger=visual_io_trigger})
    Visual_mix_trigger_request {next,visual_request,visual_mix_trigger,order}->do
        (new_engine,visual)<-create_visual visual_request engine
        return (new_engine,Visual_mix_trigger {next=next,visual=visual,visual_mix_trigger=visual_mix_trigger,order=order})
    Group_visual_request {arrange,group_visual_request}->do
        (new_engine,group_visual)<-int_map_monad_action (const (\visual_request this_engine->create_visual visual_request this_engine)) group_visual_request engine
        return (new_engine,Group_visual {arrange=arrange,group_visual=group_visual})
    Vector_visual_request {arrange,vector_visual_request}->do
        (new_engine,vector_visual)<-vector_io_map (\visual_request this_engine->create_visual visual_request this_engine) vector_visual_request engine
        return (new_engine,Vector_visual {arrange=arrange,vector_visual=vector_visual})
    Coroutine_request {initial_min_index,initial_max_index,index,insert_widget_request,raw_coroutine,iterative}->let (int,coroutine_sequence,_)=raw_coroutine.iterator 0 in let (linear_coroutine,layout,variable_size,user_variable_size)=from_coroutine (to_coroutine coroutine_sequence) int in do
        (new_engine,coroutine_state,max_index,min_index)<-from_insert_widget_request leaf_id initial_min_index initial_max_index (init_coroutine_state variable_size user_variable_size) insert_widget_request DIM.empty engine
        return (new_engine,Coroutine {initial_min_index=initial_min_index,min_index=min_index,initial_max_index=initial_max_index,max_index=max_index,index=index,variable_size=variable_size,user_variable_size=user_variable_size,coroutine_state=coroutine_state,layout=layout,linear_coroutine=linear_coroutine,iterative=iterative})
    Collector_request {initial_min_index,initial_max_index}->return (engine,Collector {initial_min_index=initial_min_index,min_index=initial_min_index,initial_max_index=initial_max_index,max_index=initial_max_index,submit=DIM.empty})
    Store_request {store}->return (engine,Store {store=store})

create_vector_widget::ET.Has_call_stack=>Custom a=>Int->Int->DVM.IOVector (Widget a)->DS.Seq (Widget_request a)->Engine a->IO (Engine a)
create_vector_widget leaf_id index vector_widget vector_widget_request engine=case vector_widget_request of
    DS.Empty->return engine
    widget_request DS.:<| other_widget_request->do
        (new_engine,widget)<-create_widget leaf_id widget_request engine
        DVM.unsafeWrite vector_widget index widget
        create_vector_widget leaf_id (index+1) vector_widget other_widget_request new_engine

from_insert_widget_request::ET.Has_call_stack=>Custom a=>Int->Int->Int->(Widget a->b)->DS.Seq (Insert (Widget_request a))->DIM.IntMap b->Engine a->IO (Engine a,DIM.IntMap b,Int,Int)
from_insert_widget_request leaf_id min_index max_index transform insert_widget_request int_map engine=case insert_widget_request of
    DS.Empty->return (engine,int_map,max_index,min_index)
    insert DS.:<| other_insert->case insert of
        Insert {insert_strategy,value}->do
            (new_engine,widget)<-create_widget leaf_id value engine
            case insert_strategy of
                Min_strategy->from_insert_widget_request leaf_id (min_index-1) max_index transform other_insert (int_map_insert min_index (transform widget) int_map) new_engine
                Max_strategy->from_insert_widget_request leaf_id min_index (max_index+1) transform other_insert (int_map_insert max_index (transform widget) int_map) new_engine
                Index_strategy {seat}->if seat<=min_index then from_insert_widget_request leaf_id (seat-1) max_index transform other_insert (int_map_insert seat (transform widget) int_map) new_engine else if max_index<=seat then from_insert_widget_request leaf_id min_index (seat+1) transform other_insert (int_map_insert seat (transform widget) int_map) new_engine else from_insert_widget_request leaf_id min_index max_index transform other_insert (int_map_insert seat (transform widget) int_map) new_engine

create_visual::ET.Has_call_stack=>Custom a=>Visual_request a->Engine a->IO (Engine a,Visual a)
create_visual visual_request engine=case visual_request of
    Rectangle_request {arrange,rectangle_width,rectangle_height}->return (engine,Rectangle {arrange=arrange,half_width=rectangle_width/2,half_height=rectangle_height/2})
    Triangle_request {arrange,first_point,second_point,third_point}->return (engine,Triangle {arrange=arrange,first_point=first_point,second_point=second_point,third_point=third_point})
    Convex_polygon_request {arrange,point_set}->return (engine,Convex_polygon {arrange=arrange,point_set=point_set})
    Regular_polygon_request {arrange,number,radius,angle}->return (engine,Regular_polygon {arrange=arrange,number=number,radius=radius,angle=angle})
    Picture_request {arrange,path}->create_picture arrange path engine
    Large_picture_request {arrange,path}->do
        (texture,width,height)<-from_image engine.device engine.picture_transfer_buffer engine.max_picture_size path
        return (engine {album=int_map_insert engine.album_id (Album {width=width,height=height,texture=texture}) engine.album,album_id=engine.album_id+1},Large_picture {arrange=arrange,half_width=fromIntegral width/2,half_height=fromIntegral height/2,album_id=engine.album_id})
    Atlas_request {arrange,path,clip_request}->create_atlas arrange path clip_request 0 engine
    Large_atlas_request {arrange,path,clip_request}->do
        (texture,width,height)<-from_image engine.device engine.picture_transfer_buffer engine.max_picture_size path
        return (engine {album=int_map_insert engine.album_id (Album {width=width,height=height,texture=texture}) engine.album,album_id=engine.album_id+1},Large_atlas {arrange=arrange,clip=to_storable_vector (create_large_atlas (fromIntegral width) (fromIntegral height)) clip_request (DS.length clip_request),index=0,album_id=engine.album_id})
    Animation_request {arrange,min_delay,padding,exponent_width,exponent_height,path}->create_animation arrange min_delay padding exponent_width exponent_height path engine
    Text_request {arrange,text_width,text_height,max_search_index,calculate_width,calculate_typesetting,anchor,article,load}->let charset=to_charset article in let half_height=text_height/2 in if load
        then do
            new_engine<-from_charset charset engine
            return (new_engine,let (new_article,number)=for_text max_search_index new_engine.font new_engine.font_map article calculate_width in let (new_new_article,max_y)=do_typesetting number half_height (calculate_typesetting new_article number) new_article in Text {arrange=arrange,half_width=text_width/2,half_height=half_height,current_y=0,min_y=0,max_y=max_y-half_height,anchor=anchor,article=new_new_article,charset=charset,locked=False})
        else return (engine,let (new_article,number)=for_text max_search_index engine.font engine.font_map article calculate_width in let (new_new_article,max_y)=do_typesetting number half_height (calculate_typesetting new_article number) new_article in Text {arrange=arrange,half_width=text_width/2,half_height=half_height,current_y=0,min_y=0,max_y=max_y-half_height,anchor=anchor,article=new_new_article,charset=charset,locked=False})
    Editor_request {}->error "未完待续"
    Canvas_request {arrange,canvas_width,canvas_height,maybe_canvas_id}->do
        texture<-FMU.with (SDLI.SDL_GPUTextureCreateInfo {sdl_type=SDLI.sdl_gpu_texturetype_2d,sdl_format=SDLI.sdl_gpu_textureformat_r8g8b8a8_unorm,sdl_usage=SDLI.sdl_gpu_textureusage_sampler DB..|. SDLI.sdl_gpu_textureusage_color_target,sdl_width=canvas_width,sdl_height=canvas_height,sdl_layer_count_or_depth=1,sdl_num_levels=1,sdl_sample_count=SDLI.sdl_gpu_samplecount_1}) (sdl_return_catch_null . SDLF.sdl_create_gpu_texture engine.device)
        temporary_texture<-FMU.with (SDLI.SDL_GPUTextureCreateInfo {sdl_type=SDLI.sdl_gpu_texturetype_2d,sdl_format=SDLI.sdl_gpu_textureformat_r8g8b8a8_unorm,sdl_usage=SDLI.sdl_gpu_textureusage_sampler DB..|. SDLI.sdl_gpu_textureusage_color_target,sdl_width=canvas_width,sdl_height=canvas_height,sdl_layer_count_or_depth=1,sdl_num_levels=1,sdl_sample_count=SDLI.sdl_gpu_samplecount_1}) (sdl_return_catch_null . SDLF.sdl_create_gpu_texture engine.device)
        case maybe_canvas_id of
            Nothing->return (engine {canvas=int_map_insert engine.canvas_id (Bound_canvas {texture=texture,temporary_texture=temporary_texture}) engine.canvas,canvas_id=engine.canvas_id+1},Canvas {arrange=arrange,canvas_width=canvas_width,canvas_height=canvas_height,half_width=fromIntegral canvas_width/2,half_height=fromIntegral canvas_height/2,canvas_id=engine.canvas_id})
            Just canvas_id->return (engine {canvas=int_map_insert canvas_id (Bound_canvas {texture=texture,temporary_texture=temporary_texture}) engine.canvas,canvas_id=max (canvas_id+1) engine.canvas_id},Canvas {arrange=arrange,canvas_width=canvas_width,canvas_height=canvas_height,half_width=fromIntegral canvas_width/2,half_height=fromIntegral canvas_height/2,canvas_id=canvas_id})
    Custom_visual_request {custom}->do
        (new_engine,new_custom)<-custom_visual_request custom engine
        return (new_engine,Custom_visual {custom=new_custom})

do_image::ET.Has_call_stack=>(DW.Word32->DW.Word32->DW.Word32->DW.Word32->DW.Word32->DW.Word32->Visual a)->DT.Text->Engine a->IO (Engine a,Visual a)
do_image action path engine=do
    (texture,width,height)<-from_image engine.device engine.picture_transfer_buffer engine.max_picture_size path
    let (atlas,left,down,right,up)=atlas_insert width height engine.padding engine.atlas
    copy_texture engine.device texture engine.texture left down width height
    SDLF.sdl_release_gpu_texture engine.device texture
    return (engine {atlas=atlas},action width height left down right up)

create_picture::ET.Has_call_stack=>Arrange->DT.Text->Engine a->IO (Engine a,Visual a)
create_picture arrange path engine=do_image (\width height left down right up->Picture {arrange=arrange,half_width=fromIntegral width/2,half_height=fromIntegral height/2,min_u=scaleFloat (negate engine.exponent_width) (fromIntegral left),min_v=scaleFloat (negate engine.exponent_height) (fromIntegral down),max_u=scaleFloat (negate engine.exponent_width) (fromIntegral right),max_v=scaleFloat (negate engine.exponent_height) (fromIntegral up),path=path,locked=False}) path engine

create_atlas::ET.Has_call_stack=>Arrange->DT.Text->DS.Seq Clip_request->Int->Engine a->IO (Engine a,Visual a)
create_atlas arrange path clip_request index engine=do_image (\width height left down right up->Atlas {arrange=arrange,path=path,clip_request=clip_request,clip=to_storable_vector (create_atlas_a (fromIntegral width) (fromIntegral height) (fromIntegral (left+right)/2) (fromIntegral (down+up)/2) engine.exponent_width engine.exponent_height) clip_request (DS.length clip_request),index=index,locked=False}) path engine

create_atlas_a::ET.Has_call_stack=>FCT.CFloat->FCT.CFloat->FCT.CFloat->FCT.CFloat->Int->Int->Clip_request->Clip
create_atlas_a width height this_x this_y exponent_width exponent_height clip_request=case clip_request of
    Clip_request {x,y,min_u,min_v,max_u,max_v}->Clip {x=x,y=y,half_width=width*(max_u-min_u)/4,half_height=height*(max_v-min_v)/4,min_u=scaleFloat (negate exponent_width) (this_x+min_u*width/2),min_v=scaleFloat (negate exponent_height) (this_y-max_v*height/2),max_u=scaleFloat (negate exponent_width) (this_x+max_u*width/2),max_v=scaleFloat (negate exponent_height) (this_y-min_v*height/2)}

create_large_atlas::ET.Has_call_stack=>FCT.CFloat->FCT.CFloat->Clip_request->Clip
create_large_atlas width height clip_request=case clip_request of
    Clip_request {x,y,min_u,min_v,max_u,max_v}->Clip {x=x,y=y,half_width=width*(max_u-min_u)/4,half_height=height*(max_v-min_v)/4,min_u=(1+min_u)/2,min_v=(1-max_v)/2,max_u=(1+max_u)/2,max_v=(1-min_v)/2}

create_animation::ET.Has_call_stack=>Arrange->FCT.CFloat->Int->Int->Int->DT.Text->Engine a->IO (Engine a,Visual a)
create_animation arrange min_delay padding exponent_width exponent_height path engine=with_text path $ \this_path->do
    ptr_animation<-SDLF.img_load_animation this_path
    sdl_catch_null ptr_animation
    animation<-FS.peek ptr_animation
    case animation of
        SDLI.IMG_Animation {img_w,img_h,img_count,img_frames,img_delays}->let width=DB.shiftL 1 exponent_width in let height=DB.shiftL 1 exponent_height in let size=4*width*height in do
            CM.when (engine.max_picture_size<fromIntegral size) EF.empty_error
            let frame_width=fromIntegral img_w
            let frame_height=fromIntegral img_h
            let pack_width=frame_width+2*padding
            let pack_height=frame_height+2*padding
            let width_number=div width pack_width
            let height_number=div height pack_height
            let number=width_number*height_number
            CM.when (number==0) EF.empty_error
            let count=fromIntegral img_count
            delay<-DVS.generateM count (fmap (\this_delay->max min_delay (fromIntegral this_delay*millisecond)) . FS.peekElemOff img_delays)
            new_engine<-create_animation_a (create_animation_b img_frames padding width size frame_width frame_height pack_width pack_height width_number) (fromIntegral width) (fromIntegral height) number count 0 engine.album_id engine
            SDLF.img_free_animation ptr_animation
            return (new_engine,Animation {arrange=arrange,delay=delay,moment=0,half_width=fromIntegral frame_width/2,half_height=fromIntegral frame_height/2,padding=fromIntegral padding,exponent_width=exponent_width,exponent_height=exponent_height,width_number=width_number,height_number=height_number,album_number=div (count+number-1) number,count=count,index=0,album_id=engine.album_id})

create_animation_a::ET.Has_call_stack=>(Int->Int->Int->FP.Ptr ()->IO ())->DW.Word32->DW.Word32->Int->Int->Int->Int->Engine a->IO (Engine a)
create_animation_a action width height number count index album_id engine=if count<=index then return engine else do
    texture<-upload_texture engine.device engine.picture_transfer_buffer width height (action number count index)
    create_animation_a action width height number count (index+number) (album_id+1) (engine {album=int_map_insert album_id (Album {width=width,height=height,texture=texture}) engine.album,album_id=album_id+1})

create_animation_b::ET.Has_call_stack=>FP.Ptr (FP.Ptr SDLT.SDL_Surface)->Int->Int->Int->Int->Int->Int->Int->Int->Int->Int->Int->FP.Ptr ()->IO ()
create_animation_b frame padding width size frame_width frame_height pack_width pack_height width_number number count index map_transfer_buffer=do
    FMU.fillBytes (FP.castPtr map_transfer_buffer) 0 size
    monad_for 0 (min number (count-index)-1) $ \this_index->do
        surface_ptr<-FS.peekElemOff frame (index+this_index)
        surface<-SDLF.sdl_convert_surface surface_ptr SDLI.sdl_pixelformat_rgba32
        sdl_catch_null surface
        pitch<-SDLI.sdl_surface_pitch_peek surface
        pixel<-SDLI.sdl_surface_pixels_peek surface
        let new_pitch=fromIntegral pitch in monad_for 0 (frame_height-1) (\y->FMU.copyBytes (FP.plusPtr map_transfer_buffer (((div this_index width_number*pack_height+padding+y)*width+mod this_index width_number*pack_width+padding)*4)) (FP.plusPtr pixel (y*new_pitch)) (frame_width*4))
        SDLF.sdl_destroy_surface surface

remove_leaf::ET.Has_call_stack=>Custom a=>Int->Engine a->IO (Engine a)
remove_leaf leaf_id engine=let (leaf,projection)=int_map_delete_lookup leaf_id engine.leaf in case projection of
    Without {ancestry_id,object}->remove_leaf_a ancestry_id object leaf leaf_id engine
    With {ancestry_id,object}->remove_leaf_a ancestry_id object leaf leaf_id engine

remove_leaf_a::ET.Has_call_stack=>Custom a=>DS.Seq Int->Widget a->DIM.IntMap (Projection a)->Int->Engine a->IO (Engine a)
remove_leaf_a ancestry_id object leaf leaf_id engine=case ancestry_id of
    DS.Empty->all_selector_monad_action remove_widget object (engine {leaf=leaf})
    _ DS.:|> father_id->all_selector_monad_action remove_widget object (engine {leaf=leaf,node=int_map_update father_id (\node->node {leaf_child=int_set_delete leaf_id node.leaf_child}) engine.node})

remove_widget::ET.Has_call_stack=>Custom a=>Widget a->Engine a->IO (Engine a)
remove_widget widget engine=any_visual_selector_monad_action False (const remove_visual) widget engine

remove_visual::ET.Has_call_stack=>Custom a=>Visual a->Engine a->IO (Engine a)
remove_visual visual engine=case visual of
    Large_picture {album_id}->let (album,single_album)=int_map_delete_lookup album_id engine.album in do
        SDLF.sdl_release_gpu_texture engine.device single_album.texture
        return (engine {album=album})
    Large_atlas {album_id}->let (album,single_album)=int_map_delete_lookup album_id engine.album in do
        SDLF.sdl_release_gpu_texture engine.device single_album.texture
        return (engine {album=album})
    Animation {album_number,album_id}->do
        new_album<-monad_fold 0 (album_number-1) engine.album (\index album->remove_animation engine.device index album_id album)
        return (engine {album=new_album})
    Canvas {canvas_id}->let (canvas,single_canvas)=int_map_delete_lookup canvas_id engine.canvas in do
        clean_canvas engine.device single_canvas
        return (engine {canvas=canvas})
    Custom_visual {custom}->custom_visual_remove custom engine
    _->return engine

clean_canvas::ET.Has_call_stack=>FP.Ptr SDLT.SDL_GPUDevice->Canvas->IO ()
clean_canvas device canvas=case canvas of
    Free_canvas {texture,temporary_texture}->do
        SDLF.sdl_release_gpu_texture device texture
        SDLF.sdl_release_gpu_texture device temporary_texture
    Bound_canvas {texture,temporary_texture}->do
        SDLF.sdl_release_gpu_texture device texture
        SDLF.sdl_release_gpu_texture device temporary_texture

remove_animation::ET.Has_call_stack=>FP.Ptr SDLT.SDL_GPUDevice->Int->Int->DIM.IntMap Album->IO (DIM.IntMap Album)
remove_animation device index album_id album=let (new_album,single_album)=int_map_delete_lookup (album_id+index) album in do
    SDLF.sdl_release_gpu_texture device single_album.texture
    return new_album

create_node::ET.Has_call_stack=>Int->Maybe Int->(Engine a->Event a->Event a)->(Event a->Engine a->Widget a->Widget a)->Engine a->Engine a
create_node node_id maybe_father_id event_transform widget_transform engine=case maybe_father_id of
    Nothing->engine {node=int_map_insert node_id (Node {ancestry_id=DS.empty,leaf_child=DIS.empty,node_child=DIS.empty,event_transform=event_transform,widget_transform=widget_transform}) engine.node}
    Just father_id->let (node,single_node)=int_map_update_lookup father_id (\this_node->this_node {node_child=int_set_insert node_id this_node.node_child}) engine.node in engine {node=int_map_insert node_id (Node {ancestry_id=single_node.ancestry_id DS.|> father_id,leaf_child=DIS.empty,node_child=DIS.empty,event_transform=event_transform,widget_transform=widget_transform}) node}

remove_node::ET.Has_call_stack=>Custom a=>Int->Engine a->IO (Engine a)
remove_node node_id engine=let (node,single_node)=int_map_delete_lookup node_id engine.node in case single_node.ancestry_id of
    DS.Empty->remove_node_a single_node.leaf_child single_node.node_child (engine {node=node})
    _ DS.:|> father_id->remove_node_a single_node.leaf_child single_node.node_child (engine {node=int_map_update father_id (\this_node->this_node {node_child=int_set_delete node_id this_node.node_child}) node})

remove_node_a::ET.Has_call_stack=>Custom a=>DIS.IntSet->DIS.IntSet->Engine a->IO (Engine a)
remove_node_a leaf_child node_child engine=do
    new_engine<-int_set_monad_fold remove_node_leaf leaf_child engine
    int_set_monad_fold remove_node_node node_child new_engine

remove_node_leaf::ET.Has_call_stack=>Custom a=>Int->Engine a->IO (Engine a)
remove_node_leaf leaf_id engine=let (leaf,projection)=int_map_delete_lookup leaf_id engine.leaf in all_selector_monad_action remove_widget (lookup_projection_object projection) (engine {leaf=leaf})

remove_node_node::ET.Has_call_stack=>Custom a=>Int->Engine a->IO (Engine a)
remove_node_node node_id engine=let (node,single_node)=int_map_delete_lookup node_id engine.node in remove_node_a single_node.leaf_child single_node.node_child (engine {node=node})

{-# INLINE from_same_insert_widget #-}
{-# INLINE from_same_insert_widget_a #-}
{-# INLINE from_insert_widget #-}
{-# INLINE from_insert_widget_a #-}
{-# INLINE create_atlas_a #-}
{-# INLINE create_large_atlas #-}
{-# INLINE create_node #-}