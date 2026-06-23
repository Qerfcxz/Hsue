{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

module Engine.Instance where

import qualified Data.Aeson as DA
import qualified Data.Aeson.Types as DAT
import qualified Data.Sequence as DS
import qualified Foreign.C.Types as FCT
import qualified Foreign.Ptr as FP
import qualified Foreign.Storable as FS

data Vertex=Vertex {red::FCT.CFloat,green::FCT.CFloat,blue::FCT.CFloat,alpha::FCT.CFloat,x::FCT.CFloat,y::FCT.CFloat,u::FCT.CFloat,v::FCT.CFloat,parameter_id::FCT.CFloat,size::FCT.CFloat}

instance FS.Storable Vertex where
    sizeOf=vertex_size_of
    alignment=vertex_alignment
    peek=vertex_peek
    poke=vertex_poke

vertex_size_of::Num a=>Vertex->a
vertex_size_of _=40

vertex_alignment::Num a=>Vertex->a
vertex_alignment _=4

vertex_peek::FP.Ptr Vertex->IO Vertex
vertex_peek _=error "vertex_peek: error 1"

vertex_poke::FP.Ptr Vertex->Vertex->IO ()
vertex_poke ptr vertex=case vertex of
    (Vertex {red,green,blue,alpha,x,y,u,v,parameter_id,size})->let new_ptr=FP.castPtr ptr in do
        FS.pokeElemOff new_ptr 0 red
        FS.pokeElemOff new_ptr 1 green
        FS.pokeElemOff new_ptr 2 blue
        FS.pokeElemOff new_ptr 3 alpha
        FS.pokeElemOff new_ptr 4 x
        FS.pokeElemOff new_ptr 5 y
        FS.pokeElemOff new_ptr 6 u
        FS.pokeElemOff new_ptr 7 v
        FS.pokeElemOff new_ptr 8 parameter_id
        FS.pokeElemOff new_ptr 9 size

data Parameter=Parameter {x::FCT.CFloat,y::FCT.CFloat,x_x::FCT.CFloat,x_y::FCT.CFloat,y_x::FCT.CFloat,y_y::FCT.CFloat,clip_flag::FCT.CFloat,clip_left::FCT.CFloat,clip_down::FCT.CFloat,clip_right::FCT.CFloat,clip_up::FCT.CFloat}

instance FS.Storable Parameter where
    sizeOf=parameter_size_of
    alignment=parameter_alignment
    peek=parameter_peek
    poke=parameter_poke

parameter_size_of::Num a=>Parameter->a
parameter_size_of _=48

parameter_alignment::Num a=>Parameter->a
parameter_alignment _=4

parameter_peek::FP.Ptr Parameter->IO Parameter
parameter_peek _=error "parameter_peek: error 1"

parameter_poke::FP.Ptr Parameter->Parameter->IO ()
parameter_poke ptr parameter=case parameter of
    (Parameter {x,y,x_x,x_y,y_x,y_y,clip_flag,clip_left,clip_down,clip_right,clip_up})->let new_ptr=FP.castPtr ptr::FP.Ptr FCT.CFloat in do
        FS.pokeElemOff new_ptr 0 x
        FS.pokeElemOff new_ptr 1 y
        FS.pokeElemOff new_ptr 2 x_x
        FS.pokeElemOff new_ptr 3 x_y
        FS.pokeElemOff new_ptr 4 y_x
        FS.pokeElemOff new_ptr 5 y_y
        FS.pokeElemOff new_ptr 6 0
        FS.pokeElemOff new_ptr 7 clip_flag
        FS.pokeElemOff new_ptr 8 clip_left
        FS.pokeElemOff new_ptr 9 clip_down
        FS.pokeElemOff new_ptr 10 clip_right
        FS.pokeElemOff new_ptr 11 clip_up

data MSDF_Metrics=MSDF_Metrics {msdf_ascender::FCT.CFloat,msdf_descender::FCT.CFloat}

instance DA.FromJSON MSDF_Metrics where
    parseJSON=msdf_metrics_parse_json

msdf_metrics_parse_json::DAT.Value->DAT.Parser MSDF_Metrics
msdf_metrics_parse_json=DA.withObject "MSDF_Metrics" $ \object->do
    ascender<-object DA..: "ascender"::DAT.Parser Double
    descender<-object DA..: "descender"::DAT.Parser Double
    return (MSDF_Metrics {msdf_ascender=realToFrac ascender,msdf_descender=realToFrac descender})

data MSDF_Bounds=MSDF_Bounds {msdf_left::FCT.CFloat,msdf_bottom::FCT.CFloat,msdf_right::FCT.CFloat,msdf_top::FCT.CFloat}

instance DA.FromJSON MSDF_Bounds where
    parseJSON=msdf_bounds_parse_json

msdf_bounds_parse_json::DAT.Value->DAT.Parser MSDF_Bounds
msdf_bounds_parse_json=DA.withObject "MSDF_Bounds" $ \object->do
    left<-object DA..: "left"::DAT.Parser Double
    bottom<-object DA..: "bottom"::DAT.Parser Double
    right<-object DA..: "right"::DAT.Parser Double
    top<-object DA..: "top"::DAT.Parser Double
    return (MSDF_Bounds {msdf_left=realToFrac left,msdf_bottom=realToFrac bottom,msdf_right=realToFrac right,msdf_top=realToFrac top})

data MSDF_Glyph=MSDF_Glyph {msdf_unicode::Int,msdf_advance::FCT.CFloat,msdf_plane_bounds::MSDF_Bounds,msdf_atlas_bounds::MSDF_Bounds}

instance DA.FromJSON MSDF_Glyph where
    parseJSON=msdf_glyph_parse_json

msdf_glyph_parse_json::DAT.Value->DAT.Parser MSDF_Glyph
msdf_glyph_parse_json=DA.withObject "MSDF_Glyph" $ \object->do
    unicode<-object DA..: "unicode"
    advance<-object DA..: "advance"::DAT.Parser Double
    plane_bound<-(object DA..:? "planeBounds") DA..!= MSDF_Bounds {msdf_left=0,msdf_bottom=0,msdf_right=0,msdf_top=0}
    atlas_bound<-(object DA..:? "atlasBounds") DA..!= MSDF_Bounds {msdf_left=0,msdf_bottom=0,msdf_right=0,msdf_top=0}
    return (MSDF_Glyph {msdf_unicode=unicode,msdf_advance=realToFrac advance,msdf_plane_bounds=plane_bound,msdf_atlas_bounds=atlas_bound})

data MSDF_Output=MSDF_Output {msdf_metrics::MSDF_Metrics,msdf_glyphs::DS.Seq MSDF_Glyph}

instance DA.FromJSON MSDF_Output where
    parseJSON=msdf_output_parse_json

msdf_output_parse_json::DAT.Value->DAT.Parser MSDF_Output
msdf_output_parse_json=DA.withObject "MSDF_Output" $ \object->do
    metric<-object DA..: "metrics"
    glyph<-object DA..: "glyphs"
    return (MSDF_Output {msdf_metrics=metric,msdf_glyphs=glyph})