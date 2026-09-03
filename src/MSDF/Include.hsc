{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE StrictData #-}

module MSDF.Include where

#include "MSDF.h"

import Error.Function
import Error.Type
import Data.Word
import Foreign.C.Types
import Foreign.Ptr
import Foreign.Storable

data MSDF_Glyph=MSDF_Glyph {msdf_unicode::CInt,msdf_advance::CFloat,msdf_plane_left::CFloat,msdf_plane_down::CFloat,msdf_plane_right::CFloat,msdf_plane_up::CFloat,msdf_atlas_left::CFloat,msdf_atlas_down::CFloat,msdf_atlas_right::CFloat,msdf_atlas_up::CFloat}

instance Storable MSDF_Glyph where
    sizeOf=msdf_glyph_size_of
    alignment=msdf_glyph_alignment
    peek=msdf_glyph_peek
    poke=msdf_glyph_poke

msdf_glyph_size_of::Has_call_stack=>Num a=>MSDF_Glyph->a
msdf_glyph_size_of _=(#size MSDF_Glyph)

msdf_glyph_alignment::Has_call_stack=>Num a=>MSDF_Glyph->a
msdf_glyph_alignment _=(#alignment MSDF_Glyph)

msdf_glyph_peek::Has_call_stack=>Ptr MSDF_Glyph->IO MSDF_Glyph
msdf_glyph_peek ptr=do
    msdf_unicode<-(#peek MSDF_Glyph,unicode) ptr
    msdf_advance<-(#peek MSDF_Glyph,advance) ptr
    msdf_plane_left<-(#peek MSDF_Glyph,plane_left) ptr
    msdf_plane_down<-(#peek MSDF_Glyph,plane_down) ptr
    msdf_plane_right<-(#peek MSDF_Glyph,plane_right) ptr
    msdf_plane_up<-(#peek MSDF_Glyph,plane_up) ptr
    msdf_atlas_left<-(#peek MSDF_Glyph,atlas_left) ptr
    msdf_atlas_down<-(#peek MSDF_Glyph,atlas_down) ptr
    msdf_atlas_right<-(#peek MSDF_Glyph,atlas_right) ptr
    msdf_atlas_up<-(#peek MSDF_Glyph,atlas_up) ptr
    return (MSDF_Glyph {msdf_unicode=msdf_unicode,msdf_advance=msdf_advance,msdf_plane_left=msdf_plane_left,msdf_plane_down=msdf_plane_down,msdf_plane_right=msdf_plane_right,msdf_plane_up=msdf_plane_up,msdf_atlas_left=msdf_atlas_left,msdf_atlas_down=msdf_atlas_down,msdf_atlas_right=msdf_atlas_right,msdf_atlas_up=msdf_atlas_up})

msdf_glyph_poke::Has_call_stack=>Ptr MSDF_Glyph->MSDF_Glyph->IO ()
msdf_glyph_poke _ _=empty_error

data MSDF_Output=MSDF_Output {msdf_pixel::Ptr Word8,msdf_width::CInt,msdf_height::CInt,msdf_descent::CFloat,msdf_ascent::CFloat,msdf_glyph::Ptr MSDF_Glyph,msdf_count::CInt}

instance Storable MSDF_Output where
    sizeOf=msdf_output_size_of
    alignment=msdf_output_alignment
    peek=msdf_output_peek
    poke=msdf_output_poke

msdf_output_size_of::Has_call_stack=>Num a=>MSDF_Output->a
msdf_output_size_of _=(#size MSDF_Output)

msdf_output_alignment::Has_call_stack=>Num a=>MSDF_Output->a
msdf_output_alignment _=(#alignment MSDF_Output)

msdf_output_peek::Has_call_stack=>Ptr MSDF_Output->IO MSDF_Output
msdf_output_peek ptr=do
    msdf_pixel<-(#peek MSDF_Output,pixel) ptr
    msdf_width<-(#peek MSDF_Output,width) ptr
    msdf_height<-(#peek MSDF_Output,height) ptr
    msdf_descent<-(#peek MSDF_Output,descent) ptr
    msdf_ascent<-(#peek MSDF_Output,ascent) ptr
    msdf_glyph<-(#peek MSDF_Output,glyph) ptr
    msdf_count<-(#peek MSDF_Output,count) ptr
    return (MSDF_Output {msdf_pixel=msdf_pixel,msdf_width=msdf_width,msdf_height=msdf_height,msdf_descent=msdf_descent,msdf_ascent=msdf_ascent,msdf_glyph=msdf_glyph,msdf_count=msdf_count})

msdf_output_poke::Has_call_stack=>Ptr MSDF_Output->MSDF_Output->IO ()
msdf_output_poke _ _=empty_error

{-# INLINE msdf_glyph_size_of #-}
{-# INLINE msdf_glyph_alignment #-}
{-# INLINE msdf_glyph_peek #-}
{-# INLINE msdf_glyph_poke #-}
{-# INLINE msdf_output_size_of #-}
{-# INLINE msdf_output_alignment #-}
{-# INLINE msdf_output_peek #-}
{-# INLINE msdf_output_poke #-}