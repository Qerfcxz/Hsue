{-# LANGUAGE PatternSynonyms #-}

module SDL.Constant where

#include <SDL3/SDL.h>

import Foreign.Storable
import qualified Data.Word as DT
import qualified Foreign.Ptr as FP

sdl_init_video::DT.Word32
sdl_init_video=(#const SDL_INIT_VIDEO)

sdl_window_fullscreen::DT.Word64
sdl_window_fullscreen=(#const SDL_WINDOW_FULLSCREEN)

sdl_window_hidden::DT.Word64
sdl_window_hidden=(#const SDL_WINDOW_HIDDEN)

sdl_window_borderless::DT.Word64
sdl_window_borderless=(#const SDL_WINDOW_BORDERLESS)

sdl_window_resizable::DT.Word64
sdl_window_resizable=(#const SDL_WINDOW_RESIZABLE)

sdl_event_size::Int
sdl_event_size=(#size SDL_Event)

sdl_event_type::FP.Ptr ()->IO DT.Word32
sdl_event_type ptr=(#peek SDL_Event,type) ptr

sdl_windowevent_windowid::FP.Ptr ()->IO DT.Word32
sdl_windowevent_windowid ptr=(#peek SDL_WindowEvent,windowID) ptr

sdl_keyboardevent_windowid::FP.Ptr ()->IO DT.Word32
sdl_keyboardevent_windowid ptr=(#peek SDL_KeyboardEvent,windowID) ptr

sdl_keyboardevent_key::FP.Ptr ()->IO DT.Word32
sdl_keyboardevent_key ptr=(#peek SDL_KeyboardEvent,key) ptr

pattern SDL_EVENT_QUIT::DT.Word32
pattern SDL_EVENT_QUIT=(#const SDL_EVENT_QUIT)

pattern SDL_EVENT_WINDOW_CLOSE_REQUESTED::DT.Word32
pattern SDL_EVENT_WINDOW_CLOSE_REQUESTED=(#const SDL_EVENT_WINDOW_CLOSE_REQUESTED)

pattern SDL_EVENT_KEY_UP::DT.Word32
pattern SDL_EVENT_KEY_UP=(#const SDL_EVENT_KEY_UP)

pattern SDL_EVENT_KEY_DOWN::DT.Word32
pattern SDL_EVENT_KEY_DOWN=(#const SDL_EVENT_KEY_DOWN)

pattern SDLK_A::DT.Word32
pattern SDLK_A=(#const SDLK_A)

pattern SDLK_B::DT.Word32
pattern SDLK_B=(#const SDLK_B)

pattern SDLK_C::DT.Word32
pattern SDLK_C=(#const SDLK_C)

pattern SDLK_D::DT.Word32
pattern SDLK_D=(#const SDLK_D)

pattern SDLK_E::DT.Word32
pattern SDLK_E=(#const SDLK_E)

pattern SDLK_F::DT.Word32
pattern SDLK_F=(#const SDLK_F)

pattern SDLK_G::DT.Word32
pattern SDLK_G=(#const SDLK_G)

pattern SDLK_H::DT.Word32
pattern SDLK_H=(#const SDLK_H)

pattern SDLK_I::DT.Word32
pattern SDLK_I=(#const SDLK_I)

pattern SDLK_J::DT.Word32
pattern SDLK_J=(#const SDLK_J)

pattern SDLK_K::DT.Word32
pattern SDLK_K=(#const SDLK_K)

pattern SDLK_L::DT.Word32
pattern SDLK_L=(#const SDLK_L)

pattern SDLK_M::DT.Word32
pattern SDLK_M=(#const SDLK_M)

pattern SDLK_N::DT.Word32
pattern SDLK_N=(#const SDLK_N)

pattern SDLK_O::DT.Word32
pattern SDLK_O=(#const SDLK_O)

pattern SDLK_P::DT.Word32
pattern SDLK_P=(#const SDLK_P)

pattern SDLK_Q::DT.Word32
pattern SDLK_Q=(#const SDLK_Q)

pattern SDLK_R::DT.Word32
pattern SDLK_R=(#const SDLK_R)

pattern SDLK_S::DT.Word32
pattern SDLK_S=(#const SDLK_S)

pattern SDLK_T::DT.Word32
pattern SDLK_T=(#const SDLK_T)

pattern SDLK_U::DT.Word32
pattern SDLK_U=(#const SDLK_U)

pattern SDLK_V::DT.Word32
pattern SDLK_V=(#const SDLK_V)

pattern SDLK_W::DT.Word32
pattern SDLK_W=(#const SDLK_W)

pattern SDLK_X::DT.Word32
pattern SDLK_X=(#const SDLK_X)

pattern SDLK_Y::DT.Word32
pattern SDLK_Y=(#const SDLK_Y)

pattern SDLK_Z::DT.Word32
pattern SDLK_Z=(#const SDLK_Z)