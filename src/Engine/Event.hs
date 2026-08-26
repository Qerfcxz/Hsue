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

loop_engine_off::ET.Has_call_stack=>Custom_request c=>Custom_widget d=>Custom_widget_request e=>FP.Ptr ()->Engine a b c d e->IO ()
loop_engine_off event engine=do
    (new_engine,switch)<-run_request False engine
    sdl_catch_false (SDLF.sdl_wait_event event)
    event_type<-SDLI.sdl_event_type_peek event
    loop_event switch event_type event new_engine

loop_engine_off_a::ET.Has_call_stack=>Custom_request c=>Custom_widget d=>Custom_widget_request e=>FP.Ptr ()->Engine a b c d e->IO ()
loop_engine_off_a event engine=do
    sdl_catch_false (SDLF.sdl_wait_event event)
    event_type<-SDLI.sdl_event_type_peek event
    loop_event False event_type event engine

loop_engine_on::ET.Has_call_stack=>Custom_request c=>Custom_widget d=>Custom_widget_request e=>FP.Ptr ()->Engine a b c d e->IO ()
loop_engine_on event engine=do
    (new_engine,switch)<-run_request False engine
    sdl_catch_false (SDLF.sdl_wait_event event)
    event_type<-SDLI.sdl_event_type_peek event
    if event_type==engine.event_number then let count=engine.count+1 in let interval=get_interval engine.timer in let time=engine.time+interval in loop_event_b (not switch) (Time {tick=count,time=time,interval=interval}) event (new_engine {count=count,time=time}) else loop_event (not switch) event_type event new_engine

loop_engine_on_a::ET.Has_call_stack=>Custom_request c=>Custom_widget d=>Custom_widget_request e=>FP.Ptr ()->Engine a b c d e->IO ()
loop_engine_on_a event engine=do
    sdl_catch_false (SDLF.sdl_wait_event event)
    event_type<-SDLI.sdl_event_type_peek event
    if event_type==engine.event_number then let count=engine.count+1 in let interval=get_interval engine.timer in let time=engine.time+interval in loop_event_b True (Time {tick=count,time=time,interval=interval}) event (engine {count=count,time=time}) else loop_event True event_type event engine

get_interval::ET.Has_call_stack=>Timer->DW.Word64
get_interval timer=case timer of
    On {interval}->interval
    _->EF.empty_error

loop_event::ET.Has_call_stack=>Custom_request c=>Custom_widget d=>Custom_widget_request e=>Bool->DW.Word32->FP.Ptr ()->Engine a b c d e->IO ()
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
                    xrel<-SDLI.sdl_mousemotionevent_xrel_peek event
                    yrel<-SDLI.sdl_mousemotionevent_yrel_peek event
                    loop_event_b on (At {window_id=window_id,action=let scale_x=adaptive_width/width in let scale_y=adaptive_height/height in Move {x=(x-width/2)*scale_x,y=(height/2-y)*scale_y,delta_x=xrel*scale_x,delta_y=(-yrel)*scale_y}}) event engine
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

to_mouse_button::ET.Has_call_stack=>DW.Word8->Mouse_button
to_mouse_button button=case button of
    SDLI.SDL_BUTTON_LEFT->Mouse_button_left
    SDLI.SDL_BUTTON_MIDDLE->Mouse_button_middle
    SDLI.SDL_BUTTON_RIGHT->Mouse_button_right
    _->Mouse_button_unknown

loop_event_a::ET.Has_call_stack=>Custom_request c=>Custom_widget d=>Custom_widget_request e=>Bool->FP.Ptr ()->Engine a b c d e->IO ()
loop_event_a on sdl_event engine=if on then loop_engine_on_a sdl_event engine else loop_engine_off_a sdl_event engine

loop_event_b::ET.Has_call_stack=>Custom_request c=>Custom_widget d=>Custom_widget_request e=>Bool->Event a->FP.Ptr ()->Engine b a c d e->IO ()
loop_event_b on event sdl_event engine=let new_engine=run_event event engine in if on then loop_engine_on sdl_event new_engine else loop_engine_off sdl_event new_engine

run_event::ET.Has_call_stack=>Custom_widget d=>Event a->Engine b a c d e->Engine b a c d e
run_event event engine=case engine.main_id event engine of
    Nothing->engine
    Just leaf_id->run_event_a leaf_id event engine

run_event_a::ET.Has_call_stack=>Custom_widget d=>Int->Event a->Engine b a c d e->Engine b a c d e
run_event_a leaf_id event engine=case int_map_functor_update leaf_id (run_event_b event engine) engine.leaf of
    Event_result {first_value,update,second_value}->let new_engine=update (engine {leaf=second_value}) in case first_value new_engine of
        Nothing->new_engine
        Just new_leaf_id->run_event_a new_leaf_id event new_engine

run_event_b::ET.Has_call_stack=>Custom_widget d=>Event a->Engine b a c d e->Projection b a c d e->Event_result b a c d e (Engine b a c d e->Maybe Int) (Projection b a c d e)
run_event_b event engine projection=case projection of
    Without {ancestry_id}->let new_event=DF.foldl' (\this_event node_id->(int_map_lookup node_id engine.node).event_transform engine this_event) event ancestry_id in run_event_c new_event (`insert_projection_object` projection) (trigger_selector_applicative_update True (run_widget new_event engine) (lookup_projection (engine.projection_strategy event engine) projection))
    With {ancestry_id}->let new_event=DF.foldl' (\this_event node_id->(int_map_lookup node_id engine.node).event_transform engine this_event) event ancestry_id in run_event_c new_event (`insert_projection_object` projection) (trigger_selector_applicative_update True (run_widget new_event engine) (lookup_projection (engine.projection_strategy event engine) projection))

run_event_c::ET.Has_call_stack=>Event a->(Widget b a c d e->Projection b a c d e)->Event_result b a c d e (Event a->Engine b a c d e->Maybe Int) (Widget b a c d e)->Event_result b a c d e (Engine b a c d e->Maybe Int) (Projection b a c d e)
run_event_c event transform event_result=case event_result of
    Event_result {first_value,update,second_value}->Event_result {first_value=first_value event,update=update,second_value=transform second_value}

run_widget::ET.Has_call_stack=>Custom_widget d=>Event a->Engine b a c d e->Widget b a c d e->Event_result b a c d e (Event a->Engine b a c d e->Maybe Int) (Widget b a c d e)
run_widget event engine this_widget=case this_widget of
    Trigger {next,trigger}->Event_result {first_value=next,update=trigger event,second_value=this_widget}
    Io_trigger {next,io_trigger}->Event_result {first_value=next,update=create_request (Io {io=io_trigger event}),second_value=this_widget}
    Mix_trigger {next,mix_trigger,order}->Event_result {first_value=next,update=let (update,io_update)=mix_trigger event in if order then create_request (Io {io=io_update}) . update else update . create_request (Io {io=io_update}),second_value=this_widget}
    Widget_trigger {next,widget_trigger,widget}->let (new_widget,update)=widget_trigger event engine widget in Event_result {first_value=next,update=update,second_value=Widget_trigger {next=next,widget_trigger=widget_trigger,widget=new_widget}}
    Widget_io_trigger {next,widget_io_trigger,widget}->let (new_widget,update)=widget_io_trigger event engine widget in Event_result {first_value=next,update=create_request (Io {io=update}),second_value=Widget_io_trigger {next=next,widget_io_trigger=widget_io_trigger,widget=new_widget}}
    Widget_mix_trigger {next,widget_mix_trigger,order,widget}->let (new_widget,update,io_update)=widget_mix_trigger event engine widget in Event_result {first_value=next,update=if order then create_request (Io {io=io_update}) . update else update . create_request (Io {io=io_update}),second_value=Widget_mix_trigger {next=next,widget_mix_trigger=widget_mix_trigger,order=order,widget=new_widget}}
    Custom_widget {custom}->let (new_custom,update,next)=custom_widget_run event engine custom in Event_result {first_value=next,update=update,second_value=Custom_widget {custom=new_custom}}
    _->EF.empty_error

run_request::ET.Has_call_stack=>Custom_request c=>Custom_widget d=>Custom_widget_request e=>Bool->Engine a b c d e->IO (Engine a b c d e,Bool)
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

push_event::ET.Has_call_stack=>Engine a b c d e->b->IO ()
push_event engine custom=do
    ptr<-FSP.newStablePtr custom
    FMA.allocaBytesAligned SDLI.sdl_event_size SDLI.sdl_event_alignment $ \sdl_event->do
        FMU.fillBytes sdl_event 0 SDLI.sdl_event_size
        FS.poke (FP.castPtr sdl_event) (engine.event_number+1)
        let new_sdl_event=FP.castPtr sdl_event
        SDLI.sdl_user_event_data1_poke new_sdl_event (FSP.castStablePtrToPtr ptr)
        sdl_catch_false (SDLF.sdl_push_event new_sdl_event)

pop_event::ET.Has_call_stack=>FP.Ptr ()->IO a
pop_event sdl_event=do
    ptr<-SDLI.sdl_user_event_data1_peek sdl_event
    let new_ptr=FSP.castPtrToStablePtr ptr
    custom<-FSP.deRefStablePtr new_ptr
    FSP.freeStablePtr new_ptr
    return custom

{-# INLINE get_interval #-}
{-# INLINE to_mouse_button #-}
{-# INLINE run_event #-}
{-# INLINE run_event_b #-}
{-# INLINE run_event_c #-}
{-# INLINE run_widget #-}
{-# INLINE push_event #-}
{-# INLINE pop_event #-}