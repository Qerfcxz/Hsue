{-# LANGUAGE PatternSynonyms #-}

module SDL.Constant where

#include <SDL3/SDL.h>

import qualified Data.Word as DT

sdl_init_audio::DT.Word32
sdl_init_audio=(#const SDL_INIT_AUDIO)

sdl_window_resizable::DT.Word64
sdl_window_resizable=(#const SDL_WINDOW_RESIZABLE)

pattern SDL_EVENT_QUIT::DT.Word32
pattern SDL_EVENT_QUIT=(#const SDL_EVENT_QUIT)