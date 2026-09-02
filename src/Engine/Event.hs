{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Engine.Event where

import Engine.Container
import Engine.Projection
import Engine.Request
import Engine.Selector
import Engine.Type
import Engine.Underlying
import qualified SDL.Function as SDLF
import qualified SDL.Include as SDLI
import qualified Error.Function as EF
import qualified Error.Type as ET
import qualified Data.Foldable as DF
import qualified Data.HashMap.Strict as DHMS
import qualified Data.HashSet as DHS
import qualified Data.Sequence as DS
import qualified Data.Word as DW
import qualified Foreign.Marshal.Alloc as FMA
import qualified Foreign.Marshal.Utils as FMU
import qualified Foreign.Ptr as FP
import qualified Foreign.Storable as FS
import qualified Foreign.StablePtr as FSP

loop_engine_off::ET.Has_call_stack=>Custom a=>FP.Ptr ()->Engine a->IO ()
loop_engine_off event engine=do
    (new_engine,switch)<-run_request False engine
    sdl_catch_false (SDLF.sdl_wait_event event)
    event_type<-SDLI.sdl_event_type_peek event
    loop_event switch event_type event new_engine

loop_engine_off_a::ET.Has_call_stack=>Custom a=>FP.Ptr ()->Engine a->IO ()
loop_engine_off_a event engine=do
    sdl_catch_false (SDLF.sdl_wait_event event)
    event_type<-SDLI.sdl_event_type_peek event
    loop_event False event_type event engine

loop_engine_on::ET.Has_call_stack=>Custom a=>FP.Ptr ()->Engine a->IO ()
loop_engine_on event engine=do
    (new_engine,switch)<-run_request False engine
    sdl_catch_false (SDLF.sdl_wait_event event)
    event_type<-SDLI.sdl_event_type_peek event
    if event_type==engine.event_number then let count=engine.count+1 in let interval=get_interval engine.timer in let time=engine.time+interval in loop_event_b (not switch) (Time {tick=count,time=time,interval=interval}) event (new_engine {time=time,count=count}) else loop_event (not switch) event_type event new_engine

loop_engine_on_a::ET.Has_call_stack=>Custom a=>FP.Ptr ()->Engine a->IO ()
loop_engine_on_a event engine=do
    sdl_catch_false (SDLF.sdl_wait_event event)
    event_type<-SDLI.sdl_event_type_peek event
    if event_type==engine.event_number then let count=engine.count+1 in let interval=get_interval engine.timer in let time=engine.time+interval in loop_event_b True (Time {tick=count,time=time,interval=interval}) event (engine {time=time,count=count}) else loop_event True event_type event engine

get_interval::ET.Has_call_stack=>Timer->DW.Word64
get_interval timer=case timer of
    On {interval}->interval
    _->EF.empty_error

to_mouse_button::ET.Has_call_stack=>DW.Word8->Mouse_button
to_mouse_button button=case button of
    SDLI.SDL_BUTTON_LEFT->Mouse_button_left
    SDLI.SDL_BUTTON_MIDDLE->Mouse_button_middle
    SDLI.SDL_BUTTON_RIGHT->Mouse_button_right
    _->Mouse_button_unknown

loop_event::ET.Has_call_stack=>Custom a=>Bool->DW.Word32->FP.Ptr ()->Engine a->IO ()
loop_event on event_type event engine=case event_type of
    SDLI.SDL_EVENT_QUIT->return ()
    SDLI.SDL_EVENT_WINDOW_CLOSE_REQUESTED->do
        sdl_window_id<-SDLI.sdl_windowevent_windowid_peek event
        case DHMS.lookup sdl_window_id engine.window_map of
            Nothing->loop_event_a on event engine
            Just window_id->loop_event_b on (At {window_id=window_id,action=Close}) event engine
    SDLI.SDL_EVENT_WINDOW_PIXEL_SIZE_CHANGED->do
        sdl_window_id<-SDLI.sdl_windowevent_windowid_peek event
        case DHMS.lookup sdl_window_id engine.window_map of
            Nothing->loop_event_a on event engine
            Just window_id->do
                first_data<-SDLI.sdl_windowevent_data1_peek event
                second_data<-SDLI.sdl_windowevent_data2_peek event
                loop_event_b on (At {window_id=window_id,action=Resize {width=fromIntegral first_data,height=fromIntegral second_data}}) event engine
    SDLI.SDL_EVENT_KEY_UP->do
        sdl_window_id<-SDLI.sdl_keyboardevent_windowid_peek event
        case DHMS.lookup sdl_window_id engine.window_map of
            Nothing->loop_event_a on event engine
            Just window_id->do
                keycode<-SDLI.sdl_keyboardevent_key_peek event
                let change=to_key keycode in let maintain=DHS.delete change engine.key in loop_event_b on (At {window_id=window_id,action=Press {press=Press_up,change=change,maintain=maintain}}) event (engine {key=maintain})
    SDLI.SDL_EVENT_KEY_DOWN->do
        sdl_window_id<-SDLI.sdl_keyboardevent_windowid_peek event
        case DHMS.lookup sdl_window_id engine.window_map of
            Nothing->loop_event_a on event engine
            Just window_id->do
                keycode<-SDLI.sdl_keyboardevent_key_peek event
                let change=to_key keycode in let maintain=DHS.insert change engine.key in loop_event_b on (At {window_id=window_id,action=Press {press=Press_down,change=change,maintain=maintain}}) event (engine {key=maintain})
    SDLI.SDL_EVENT_MOUSE_BUTTON_UP->do
        sdl_window_id<-SDLI.sdl_mousebuttonevent_windowid_peek event
        case DHMS.lookup sdl_window_id engine.window_map of
            Nothing->loop_event_a on event engine
            Just window_id->case int_map_lookup window_id engine.window of
                Window {adaptive_width,adaptive_height,width,height}->do
                    mouse_button<-SDLI.sdl_mousebuttonevent_button_peek event
                    x<-SDLI.sdl_mousebuttonevent_x_peek event
                    y<-SDLI.sdl_mousebuttonevent_y_peek event
                    loop_event_b on (At {window_id=window_id,action=Click {press=Press_up,mouse_button=to_mouse_button mouse_button,x=(x-width/2)*(adaptive_width/width),y=(height/2-y)*(adaptive_height/height)}}) event engine
    SDLI.SDL_EVENT_MOUSE_BUTTON_DOWN->do
        sdl_window_id<-SDLI.sdl_mousebuttonevent_windowid_peek event
        case DHMS.lookup sdl_window_id engine.window_map of
            Nothing->loop_event_a on event engine
            Just window_id->case int_map_lookup window_id engine.window of
                Window {adaptive_width,adaptive_height,width,height}->do
                    mouse_button<-SDLI.sdl_mousebuttonevent_button_peek event
                    x<-SDLI.sdl_mousebuttonevent_x_peek event
                    y<-SDLI.sdl_mousebuttonevent_y_peek event
                    loop_event_b on (At {window_id=window_id,action=Click {press=Press_down,mouse_button=to_mouse_button mouse_button,x=(x-width/2)*(adaptive_width/width),y=(height/2-y)*(adaptive_height/height)}}) event engine
    SDLI.SDL_EVENT_MOUSE_MOTION->do
        sdl_window_id<-SDLI.sdl_mousemotionevent_windowid_peek event
        case DHMS.lookup sdl_window_id engine.window_map of
            Nothing->loop_event_a on event engine
            Just window_id->case int_map_lookup window_id engine.window of
                Window {adaptive_width,adaptive_height,width,height}->do
                    x<-SDLI.sdl_mousemotionevent_x_peek event
                    y<-SDLI.sdl_mousemotionevent_y_peek event
                    delta_x<-SDLI.sdl_mousemotionevent_xrel_peek event
                    delta_y<-SDLI.sdl_mousemotionevent_yrel_peek event
                    loop_event_b on (At {window_id=window_id,action=let scale_x=adaptive_width/width in let scale_y=adaptive_height/height in Move {x=(x-width/2)*scale_x,y=(height/2-y)*scale_y,delta_x=delta_x*scale_x,delta_y=(negate delta_y)*scale_y}}) event engine
    SDLI.SDL_EVENT_MOUSE_WHEEL->do
        sdl_window_id<-SDLI.sdl_mousewheelevent_windowid_peek event
        case DHMS.lookup sdl_window_id engine.window_map of
            Nothing->loop_event_a on event engine
            Just window_id->case int_map_lookup window_id engine.window of
                Window {adaptive_width,adaptive_height,width,height}->do
                    x<-SDLI.sdl_mousewheelevent_x_peek event
                    y<-SDLI.sdl_mousewheelevent_y_peek event
                    mouse_x<-SDLI.sdl_mousewheelevent_mouse_x_peek event
                    mouse_y<-SDLI.sdl_mousewheelevent_mouse_y_peek event
                    loop_event_b on (At {window_id=window_id,action=let scale_x=adaptive_width/width in let scale_y=adaptive_height/height in Scroll {x=(mouse_x-width/2)*scale_x,y=(height/2-mouse_y)*scale_y,delta_x=x,delta_y=y}}) event engine
    _->if event_type==engine.event_number+1
        then do
            custom<-pop_event event
            loop_event_b on (Custom_event {custom=custom}) event engine
        else loop_event_a on event engine

loop_event_a::ET.Has_call_stack=>Custom a=>Bool->FP.Ptr ()->Engine a->IO ()
loop_event_a on ptr engine=if on then loop_engine_on_a ptr engine else loop_engine_off_a ptr engine

loop_event_b::ET.Has_call_stack=>Custom a=>Bool->Event a->FP.Ptr ()->Engine a->IO ()
loop_event_b on event ptr engine=let new_engine=maybe engine (\leaf_id->run_event leaf_id event engine) (engine.main_id event engine) in if on then loop_engine_on ptr new_engine else loop_engine_off ptr new_engine

run_event::ET.Has_call_stack=>Int->Event a->Engine a->Engine a
run_event leaf_id event engine=let (next,update,leaf)=int_map_functor_update leaf_id (\projection->let new_event=DF.foldl' (\this_event node_id->(int_map_lookup node_id engine.node).event_transform engine this_event) event (lookup_projection_ancestry_id projection) in run_event_a new_event (`insert_projection_object` projection) (trigger_selector_applicative_update True (run_widget new_event engine) (lookup_projection (engine.projection_strategy event engine) projection))) engine.leaf in let new_engine=update (engine {leaf=leaf}) in maybe new_engine (\this_leaf_id->run_event this_leaf_id event new_engine) (next new_engine)

run_event_a::ET.Has_call_stack=>Event a->(Widget a->Projection a)->Trigger_result a (Widget a)->(Engine a->Maybe Int,Engine a->Engine a,Projection a)
run_event_a event transform trigger_result=case trigger_result of
    Trigger_result {next,update,value}->(next event,update,transform value)

run_widget::ET.Has_call_stack=>Event a->Engine a->Widget a->Trigger_result a (Widget a)
run_widget event engine this_widget=case this_widget of
    Trigger {next,trigger}->Trigger_result {next=next,update=trigger event,value=this_widget}
    Io_trigger {next,io_trigger}->Trigger_result {next=next,update=create_request (Io {io=io_trigger event}),value=this_widget}
    Mix_trigger {next,mix_trigger,order}->Trigger_result {next=next,update=let (update,io_update)=mix_trigger event in if order then create_request (Io {io=io_update}) . update else update . create_request (Io {io=io_update}),value=this_widget}
    Widget_trigger {next,widget_trigger,widget}->let (new_widget,update)=widget_trigger event engine widget in Trigger_result {next=next,update=update,value=Widget_trigger {next=next,widget_trigger=widget_trigger,widget=new_widget}}
    Widget_io_trigger {next,widget_io_trigger,widget}->let (new_widget,update)=widget_io_trigger event engine widget in Trigger_result {next=next,update=create_request (Io {io=update}),value=Widget_io_trigger {next=next,widget_io_trigger=widget_io_trigger,widget=new_widget}}
    Widget_mix_trigger {next,widget_mix_trigger,order,widget}->let (new_widget,update,io_update)=widget_mix_trigger event engine widget in Trigger_result {next=next,update=if order then create_request (Io {io=io_update}) . update else update . create_request (Io {io=io_update}),value=Widget_mix_trigger {next=next,widget_mix_trigger=widget_mix_trigger,order=order,widget=new_widget}}
    _->EF.empty_error

run_request::ET.Has_call_stack=>Custom a=>Bool->Engine a->IO (Engine a,Bool)
run_request switch engine=case engine.request of
    DS.Empty->return (engine,switch)
    request DS.:<| other_request->do
        (new_engine,new_switch)<-do_request request (engine {request=other_request})
        run_request (switch/=new_switch) new_engine

to_key::ET.Has_call_stack=>DW.Word32->Key
to_key key=case key of
    SDLI.SDLK_A->Key_a
    SDLI.SDLK_B->Key_b
    SDLI.SDLK_C->Key_c
    SDLI.SDLK_D->Key_d
    SDLI.SDLK_E->Key_e
    SDLI.SDLK_F->Key_f
    SDLI.SDLK_G->Key_g
    SDLI.SDLK_H->Key_h
    SDLI.SDLK_I->Key_i
    SDLI.SDLK_J->Key_j
    SDLI.SDLK_K->Key_k
    SDLI.SDLK_L->Key_l
    SDLI.SDLK_M->Key_m
    SDLI.SDLK_N->Key_n
    SDLI.SDLK_O->Key_o
    SDLI.SDLK_P->Key_p
    SDLI.SDLK_Q->Key_q
    SDLI.SDLK_R->Key_r
    SDLI.SDLK_S->Key_s
    SDLI.SDLK_T->Key_t
    SDLI.SDLK_U->Key_u
    SDLI.SDLK_V->Key_v
    SDLI.SDLK_W->Key_w
    SDLI.SDLK_X->Key_x
    SDLI.SDLK_Y->Key_y
    SDLI.SDLK_Z->Key_z
    SDLI.SDLK_LEFT->Key_left
    SDLI.SDLK_DOWN->Key_down
    SDLI.SDLK_RIGHT->Key_right
    SDLI.SDLK_UP->Key_up
    SDLI.SDLK_PAGEDOWN->Key_page_down
    SDLI.SDLK_PAGEUP->Key_page_up
    _->Key_unknown

push_event::ET.Has_call_stack=>Engine a->Custom_event a->IO ()
push_event engine custom=do
    ptr_custom<-FSP.newStablePtr custom
    FMA.allocaBytesAligned SDLI.sdl_event_size SDLI.sdl_event_alignment $ \ptr->do
        FMU.fillBytes ptr 0 SDLI.sdl_event_size
        FS.poke (FP.castPtr ptr) (engine.event_number+1)
        let new_ptr=FP.castPtr ptr
        SDLI.sdl_user_event_data1_poke new_ptr (FSP.castStablePtrToPtr ptr_custom)
        sdl_catch_false (SDLF.sdl_push_event new_ptr)

pop_event::ET.Has_call_stack=>FP.Ptr ()->IO a
pop_event ptr=do
    new_ptr<-SDLI.sdl_user_event_data1_peek ptr
    let ptr_custom=FSP.castPtrToStablePtr new_ptr
    custom<-FSP.deRefStablePtr ptr_custom
    FSP.freeStablePtr ptr_custom
    return custom

{-# INLINE get_interval #-}
{-# INLINE to_mouse_button #-}
{-# INLINE run_event_a #-}
{-# INLINE run_widget #-}
{-# INLINE to_key #-}