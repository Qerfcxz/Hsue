{-# LANGUAGE DuplicateRecordFields #-}

module Engine.Event where

import Engine.Type
import qualified SDL.Constant as C
import qualified SDL.Function as F
import qualified Data.Int as DI
import qualified Data.Map as DM
import qualified Data.Set as DS
import qualified Data.Word as DW
import qualified Foreign.Marshal.Utils as FMU
import qualified Foreign.Ptr as FP

get_event::FP.Ptr ()->DM.Map DW.Word32 Int->DS.Set Key->IO (DS.Set Key,Event)
get_event event window_map key=do
    value<-F.sdl_waitevent event
    if FMU.toBool value then get_event_a event window_map key else error "get_event: error 1"

get_event_time::DI.Int32->FP.Ptr ()->DM.Map DW.Word32 Int->DS.Set Key->IO (Maybe (DS.Set Key,Event))
get_event_time time event window_map key=do
    value<-F.sdl_waiteventtimeout event time
    if FMU.toBool value
        then do
            new<-get_event_a event window_map key
            return (Just new)
        else return Nothing

get_event_a::FP.Ptr ()->DM.Map DW.Word32 Int->DS.Set Key->IO (DS.Set Key,Event)
get_event_a event window_map key=do
    event_type<-C.sdl_event_type event
    case event_type of
        C.SDL_EVENT_QUIT->return (key,Quit)
        C.SDL_EVENT_WINDOW_CLOSE_REQUESTED->do
            sdl_window_id<-C.sdl_windowevent_windowid event
            case DM.lookup sdl_window_id window_map of
                Nothing->return (key,Unknown)
                Just window_id->return (key,At {window_id=window_id,action=Close})
        C.SDL_EVENT_KEY_UP->do
            sdl_window_id<-C.sdl_keyboardevent_windowid event
            sdl_keycode<-C.sdl_keyboardevent_key event
            let keycode=to_key sdl_keycode in let new_key=DS.delete keycode key in case DM.lookup sdl_window_id window_map of
                Nothing->return (new_key,Unknown)
                Just window_id->return (new_key,At {window_id=window_id,action=Press {press=Press_up,keycode=keycode,set_keycode=new_key}})
        C.SDL_EVENT_KEY_DOWN->do
            sdl_window_id<-C.sdl_keyboardevent_windowid event
            sdl_keycode<-C.sdl_keyboardevent_key event
            let keycode=to_key sdl_keycode in let new_key=DS.insert keycode key in case DM.lookup sdl_window_id window_map of
                Nothing->return (new_key,Unknown)
                Just window_id->return (new_key,At {window_id=window_id,action=Press {press=Press_down,keycode=keycode,set_keycode=new_key}})
        _->return (key,Unknown)

to_key::DW.Word32->Key
to_key key=case key of
    C.SDLK_A->Key_a
    C.SDLK_B->Key_b
    C.SDLK_C->Key_c
    C.SDLK_D->Key_d
    C.SDLK_E->Key_e
    C.SDLK_F->Key_f
    C.SDLK_G->Key_g
    C.SDLK_H->Key_h
    C.SDLK_I->Key_i
    C.SDLK_J->Key_j
    C.SDLK_K->Key_k
    C.SDLK_L->Key_l
    C.SDLK_M->Key_m
    C.SDLK_N->Key_n
    C.SDLK_O->Key_o
    C.SDLK_P->Key_p
    C.SDLK_Q->Key_q
    C.SDLK_R->Key_r
    C.SDLK_S->Key_s
    C.SDLK_T->Key_t
    C.SDLK_U->Key_u
    C.SDLK_V->Key_v
    C.SDLK_W->Key_w
    C.SDLK_X->Key_x
    C.SDLK_Y->Key_y
    C.SDLK_Z->Key_z
    _->Key_unknown