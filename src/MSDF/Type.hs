{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE StrictData #-}

module MSDF.Type where

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

msdf_glyph_size_of::MSDF_Glyph->Int
msdf_glyph_size_of _=40

msdf_glyph_alignment::MSDF_Glyph->Int
msdf_glyph_alignment _=4

msdf_glyph_peek::Ptr MSDF_Glyph->IO MSDF_Glyph
msdf_glyph_peek ptr=do
    msdf_unicode<-peekByteOff ptr 0
    msdf_advance<-peekByteOff ptr 4
    msdf_plane_left<-peekByteOff ptr 8
    msdf_plane_down<-peekByteOff ptr 12
    msdf_plane_right<-peekByteOff ptr 16
    msdf_plane_up<-peekByteOff ptr 20
    msdf_atlas_left<-peekByteOff ptr 24
    msdf_atlas_down<-peekByteOff ptr 28
    msdf_atlas_right<-peekByteOff ptr 32
    msdf_atlas_up<-peekByteOff ptr 36
    return (MSDF_Glyph {msdf_unicode=msdf_unicode,msdf_advance=msdf_advance,msdf_plane_left=msdf_plane_left,msdf_plane_down=msdf_plane_down,msdf_plane_right=msdf_plane_right,msdf_plane_up=msdf_plane_up,msdf_atlas_left=msdf_atlas_left,msdf_atlas_down=msdf_atlas_down,msdf_atlas_right=msdf_atlas_right,msdf_atlas_up=msdf_atlas_up})

msdf_glyph_poke::Ptr MSDF_Glyph->MSDF_Glyph->IO ()
msdf_glyph_poke _ _=error "msdf_glyph_poke: error 1"

data MSDF_Output=MSDF_Output {msdf_pixel::Ptr Word8,msdf_width::CInt,msdf_height::CInt,msdf_descent::CFloat,msdf_ascent::CFloat,msdf_glyph::Ptr MSDF_Glyph,msdf_count::CInt}

instance Storable MSDF_Output where
    sizeOf=msdf_output_size_of
    alignment=msdf_output_alignment
    peek=msdf_output_peek
    poke=msdf_output_poke

msdf_output_size_of::MSDF_Output->Int
msdf_output_size_of _=36

msdf_output_alignment::MSDF_Output->Int
msdf_output_alignment _=8

msdf_output_peek::Ptr MSDF_Output->IO MSDF_Output
msdf_output_peek ptr=do
    msdf_pixel<-peekByteOff ptr 0
    msdf_width<-peekByteOff ptr 8
    msdf_height<-peekByteOff ptr 12
    msdf_descent<-peekByteOff ptr 16
    msdf_ascent<-peekByteOff ptr 20
    msdf_glyph<-peekByteOff ptr 24
    msdf_count<-peekByteOff ptr 32
    return (MSDF_Output {msdf_pixel=msdf_pixel,msdf_width=msdf_width,msdf_height=msdf_height,msdf_descent=msdf_descent,msdf_ascent=msdf_ascent,msdf_glyph=msdf_glyph,msdf_count=msdf_count})

msdf_output_poke::Ptr MSDF_Output->MSDF_Output->IO ()
msdf_output_poke _ _=error "msdf_output_poke: error 1"