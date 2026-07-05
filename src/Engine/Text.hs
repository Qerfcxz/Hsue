{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Engine.Text where

import Engine.Atlas
import Engine.Container
import Engine.Helper
import Engine.Type
import qualified SDL.Function as F
import qualified Data.Aeson as DA
import qualified Data.ByteString as DBS
import qualified Data.ByteString.Builder as DBSB
import qualified Data.ByteString.Lazy as DBSL
import qualified Data.Char as DC
import qualified Data.Foldable as DF
import qualified Data.IntMap as DIM
import qualified Data.IntSet as DIS
import qualified Data.Map as DM
import qualified Data.Sequence as DSeq
import qualified Data.Set as DSet
import qualified Data.Text as DT
import qualified Foreign.C.Types as FCT
import qualified System.Directory as SD
import qualified System.Process as SP

do_typesetting::FCT.CFloat->(Int->DSeq.Seq (DSeq.Seq Row)->(FCT.CFloat,FCT.CFloat))->DSeq.Seq (DSeq.Seq Row)->(DSeq.Seq (DSeq.Seq Row),FCT.CFloat)
do_typesetting height calculate_typesetting article=do_typesetting_a (-height) (`calculate_typesetting` article) 1 article DSeq.empty

do_typesetting_a::FCT.CFloat->(Int->(FCT.CFloat,FCT.CFloat))->Int->DSeq.Seq (DSeq.Seq Row)->DSeq.Seq (DSeq.Seq Row)->(DSeq.Seq (DSeq.Seq Row),FCT.CFloat)
do_typesetting_a y calculate_typesetting row_number article this_article=case article of
    DSeq.Empty->let (_,height)=calculate_typesetting row_number in (this_article,y+height)
    (paragraph DSeq.:<| other_paragraph)->let (new_paragraph,new_row_number,new_y)=do_typesetting_b y calculate_typesetting row_number paragraph in do_typesetting_a new_y calculate_typesetting new_row_number other_paragraph (this_article DSeq.|> new_paragraph)

do_typesetting_b::FCT.CFloat->(Int->(FCT.CFloat,FCT.CFloat))->Int->DSeq.Seq Row->(DSeq.Seq Row,Int,FCT.CFloat)
do_typesetting_b y calculate_typesetting row_number paragraph=case paragraph of
    DSeq.Empty->(DSeq.empty,row_number,y)
    (row DSeq.:<| other_row)->let (x,height)=calculate_typesetting row_number in let new_y=y+height in let (final_paragraph,final_row_number,final_y)=do_typesetting_b new_y calculate_typesetting (row_number+1) other_row in case row of
        Blank->(Blank DSeq.:<| final_paragraph,final_row_number,final_y)
        Row {row_core,width,min_down,max_up,min_descent,max_ascent}->(Row {row_core=row_core,x=x-width/2,y=new_y,width=width,min_down=min_down,max_up=max_up,min_descent=min_descent,max_ascent=max_ascent} DSeq.:<| final_paragraph,final_row_number,final_y)

for_text::DIM.IntMap Font->DM.Map String Int->DSeq.Seq (DSeq.Seq Sentence)->(Int->DSeq.Seq Row->DSeq.Seq (DSeq.Seq Row)->FCT.CFloat)->DSeq.Seq (DSeq.Seq Row)
for_text font font_map article calculate_width=for_text_a font font_map article calculate_width 1 DSeq.empty

for_text_a::DIM.IntMap Font->DM.Map String Int->DSeq.Seq (DSeq.Seq Sentence)->(Int->DSeq.Seq Row->DSeq.Seq (DSeq.Seq Row)->FCT.CFloat)->Int->DSeq.Seq (DSeq.Seq Row)->DSeq.Seq (DSeq.Seq Row)
for_text_a font font_map article calculate_width row_number this_article=case article of
    DSeq.Empty->this_article
    (paragraph DSeq.:<| other_paragraph)->if DSeq.null paragraph then for_text_a font font_map other_paragraph calculate_width row_number (this_article DSeq.|> DSeq.singleton Blank) else let (new_article,new_row_number)=for_text_b font font_map paragraph Positive_infinity Negative_infinity Positive_infinity Negative_infinity 0 (calculate_width row_number DSeq.empty this_article) calculate_width row_number DSeq.empty DSeq.empty this_article in for_text_a font font_map other_paragraph calculate_width new_row_number new_article

for_text_b::DIM.IntMap Font->DM.Map String Int->DSeq.Seq Sentence->Extended FCT.CFloat->Extended FCT.CFloat->Extended FCT.CFloat->Extended FCT.CFloat->FCT.CFloat->FCT.CFloat->(Int->DSeq.Seq Row->DSeq.Seq (DSeq.Seq Row)->FCT.CFloat)->Int->DSeq.Seq Character->DSeq.Seq Row->DSeq.Seq (DSeq.Seq Row)->(DSeq.Seq (DSeq.Seq Row),Int)
for_text_b font font_map paragraph min_down max_up min_descent max_ascent x width calculate_width row_number row_core this_paragraph article=case paragraph of
    DSeq.Empty->(article DSeq.|> (this_paragraph DSeq.|> Row {row_core=row_core,x=0,y=0,width=x,min_down=from_extended min_down,max_up=from_extended max_up,min_descent=from_extended min_descent,max_ascent=from_extended max_ascent}),row_number+1)
    (sentence DSeq.:<| other_sentence)->case sentence of
        Sentence {sentence_core,path}->let font_id=map_lookup path font_map in let single_font=intmap_lookup font_id font in let (new_paragraph,new_row_core,new_row_number,new_width,new_x,new_max_ascent,new_min_descent,new_max_up,new_min_down)=for_text_c font_id single_font.glyph sentence_core min_down max_up min_descent max_ascent single_font.descent single_font.ascent x width (\this_row_number this_this_paragraph->calculate_width this_row_number this_this_paragraph article) row_number row_core this_paragraph in for_text_b font font_map other_sentence new_min_down new_max_up new_min_descent new_max_ascent new_x new_width calculate_width new_row_number new_row_core new_paragraph article

for_text_c::Int->DIM.IntMap Glyph->DSeq.Seq Phrase->Extended FCT.CFloat->Extended FCT.CFloat->Extended FCT.CFloat->Extended FCT.CFloat->FCT.CFloat->FCT.CFloat->FCT.CFloat->FCT.CFloat->(Int->DSeq.Seq Row->FCT.CFloat)->Int->DSeq.Seq Character->DSeq.Seq Row->(DSeq.Seq Row,DSeq.Seq Character,Int,FCT.CFloat,FCT.CFloat,Extended FCT.CFloat,Extended FCT.CFloat,Extended FCT.CFloat,Extended FCT.CFloat)
for_text_c font_id glyph sentence_core min_down max_up min_descent max_ascent descent ascent x width calculate_width row_number row_core paragraph=case sentence_core of
    DSeq.Empty->(paragraph,row_core,row_number,width,x,max_ascent,min_descent,max_up,min_down)
    (phrase DSeq.:<| other_phrase)->case phrase of
        Phrase {phrase_core,size,red,green,blue,alpha}->let (new_paragraph,new_row_core,new_row_number,new_width,new_x,new_max_ascent,new_min_descent,new_max_up,new_min_down)=let new_descent=descent*size in let new_ascent=ascent*size in for_text_d font_id glyph phrase_core min_down max_up (min (to_extended new_descent) min_descent) (max (to_extended new_ascent) max_ascent) new_descent new_ascent size red green blue alpha x width calculate_width row_number row_core paragraph in for_text_c font_id glyph other_phrase new_min_down new_max_up new_min_descent new_max_ascent descent ascent new_x new_width calculate_width new_row_number new_row_core new_paragraph

for_text_d::Int->DIM.IntMap Glyph->DT.Text->Extended FCT.CFloat->Extended FCT.CFloat->Extended FCT.CFloat->Extended FCT.CFloat->FCT.CFloat->FCT.CFloat->FCT.CFloat->FCT.CFloat->FCT.CFloat->FCT.CFloat->FCT.CFloat->FCT.CFloat->FCT.CFloat->(Int->DSeq.Seq Row->FCT.CFloat)->Int->DSeq.Seq Character->DSeq.Seq Row->(DSeq.Seq Row,DSeq.Seq Character,Int,FCT.CFloat,FCT.CFloat,Extended FCT.CFloat,Extended FCT.CFloat,Extended FCT.CFloat,Extended FCT.CFloat)
for_text_d font_id glyph text min_down max_up min_descent max_ascent descent ascent size red green blue alpha x width calculate_width row_number row_core paragraph=case text of
    DT.Empty->(paragraph,row_core,row_number,width,x,max_ascent,min_descent,max_up,min_down)
    (char DT.:< other_char)->let unicode=DC.ord char in case intmap_lookup unicode glyph of
        Glyph {advance,left,down,right,up,min_u,min_v,max_u,max_v}->let new_advance=advance*size in let new_left=left*size in let new_down=down*size in let new_right=right*size in let new_up=up*size in let extended_down=to_extended new_down in let extended_up=to_extended new_up in if width<x+new_right then let (new_paragraph,new_row_number,new_width)=for_text_e new_right calculate_width (row_number+1) (paragraph DSeq.|> Row {row_core=row_core,x=0,y=0,width=x,min_down=from_extended min_down,max_up=from_extended max_up,min_descent=from_extended min_descent,max_ascent=from_extended max_ascent}) in for_text_d font_id glyph other_char extended_down extended_up (to_extended descent) (to_extended ascent) descent ascent size red green blue alpha new_advance new_width calculate_width new_row_number (DSeq.singleton (Character {unicode=unicode,font_id=font_id,size=size,left=new_left,down=new_down,right=new_right,up=new_up,min_u=min_u,min_v=min_v,max_u=max_u,max_v=max_v,red=red,green=green,blue=blue,alpha=alpha})) new_paragraph else for_text_d font_id glyph other_char (min extended_down min_down) (max extended_up max_up) min_descent max_ascent descent ascent size red green blue alpha (x+new_advance) width calculate_width row_number (row_core DSeq.|> Character {unicode=unicode,font_id=font_id,size=size,left=x+new_left,down=new_down,right=x+new_right,up=new_up,min_u=min_u,min_v=min_v,max_u=max_u,max_v=max_v,red=red,green=green,blue=blue,alpha=alpha}) paragraph

for_text_e::FCT.CFloat->(Int->DSeq.Seq Row->FCT.CFloat)->Int->DSeq.Seq Row->(DSeq.Seq Row,Int,FCT.CFloat)
for_text_e right calculate_width row_number paragraph=let width=calculate_width row_number paragraph in if right<width then (paragraph,row_number,width) else for_text_e right calculate_width (row_number+1) (paragraph DSeq.|> Blank)

to_charset::DSeq.Seq (DSeq.Seq Sentence)->DM.Map String (DSet.Set Char)
to_charset=DF.foldl' (DF.foldl' (flip to_charset_a)) DM.empty

to_charset_a::Sentence->DM.Map String (DSet.Set Char)->DM.Map String (DSet.Set Char)
to_charset_a sentence charset=case sentence of
    Sentence {sentence_core,path}->DM.insert path (DF.foldl' (\this_charset phrase->DT.foldl' (flip DSet.insert) this_charset phrase.phrase_core) (DM.findWithDefault DSet.empty path charset) sentence_core) charset

update_font::DM.Map String (DSet.Set Char)->Engine a->IO (Engine a)
update_font charset engine=DM.foldlWithKey' (\io_engine path char->io_engine>>=update_font_a path char) (return engine) charset

update_font_a::String->DSet.Set Char->Engine a->IO (Engine a)
update_font_a path char engine=let charset_path=path++"_charset_temporary" in let imageout_path=path++"_imageout_temporary" in let json_path=path++"_json_temporary" in case DM.lookup path engine.font_map of
    Nothing->do
        DBSL.writeFile charset_path (DBSB.toLazyByteString (DF.foldMap' (\unicode->DBSB.stringUtf8 (show unicode++" ")) (DIS.toAscList (DIS.fromDistinctAscList (map DC.ord (DSet.toAscList char))))))
        update_font_c engine.font_id path charset_path imageout_path json_path (engine {font_map=map_insert path engine.font_id engine.font_map,font_id=engine.font_id+1})
    Just font_id->case DIM.lookup font_id engine.font of
        Nothing->update_font_b font_id (DIS.fromDistinctAscList (map DC.ord (DSet.toAscList char))) path charset_path imageout_path json_path engine
        Just font->update_font_b font_id (DIS.difference (DIS.fromDistinctAscList (map DC.ord (DSet.toAscList char))) (DIM.keysSet font.glyph)) path charset_path imageout_path json_path engine

update_font_b::Int->DIS.IntSet->String->String->String->String->Engine a->IO (Engine a)
update_font_b font_id codeset path charset_path imageout_path json_path engine=if DIS.null codeset then return engine else do
    DBSL.writeFile charset_path (DBSB.toLazyByteString (DF.foldMap' (\unicode->DBSB.stringUtf8 (show unicode++" ")) (DIS.toAscList codeset)))
    update_font_c font_id path charset_path imageout_path json_path engine

update_font_c::Int->String->String->String->String->Engine a->IO (Engine a)
update_font_c font_id path charset_path imageout_path json_path engine=do
    SP.callProcess "msdf-atlas-gen.exe" ["-font",path,"-charset",charset_path,"-format","png","-imageout",imageout_path,"-json",json_path,"-size",show engine.font_size,"-pxrange",show engine.pixel_range,"-yorigin","top"]
    json<-DBS.readFile json_path
    case DA.decodeStrict json::Maybe MSDF_Output of
        Nothing->error "update_font_c: error 1"
        Just output->do
            (texture,width,height)<-load_texture engine.device engine.picture_transfer_buffer engine.picture_size imageout_path
            let (atlas,left,down,_,_)=atlas_insert width height engine.padding engine.atlas
            copy_texture engine.device texture engine.texture left down width height
            F.sdl_release_gpu_texture engine.device texture
            SD.removeFile charset_path
            SD.removeFile imageout_path
            SD.removeFile json_path
            return (engine {atlas=atlas,font=DIM.alter (from_maybe_font output.msdf_metrics.msdf_ascender output.msdf_metrics.msdf_descender output.msdf_glyphs (from_msdf_glyph (fromIntegral left) (fromIntegral down) engine.reciprocal_width engine.reciprocal_height)) font_id engine.font})

from_maybe_font::FCT.CFloat->FCT.CFloat->DSeq.Seq MSDF_Glyph->(MSDF_Glyph->(Glyph,Int))->Maybe Font->Maybe Font
from_maybe_font ascent descent msdf_glyph transform maybe_font=case maybe_font of
    Nothing->Just (Font {descent=descent,ascent=ascent,glyph=DF.foldl' (\this_glyph this_msdf_glyph->let (glyph,key)=transform this_msdf_glyph in intmap_insert key glyph this_glyph) DIM.empty msdf_glyph})
    Just font->Just (font {glyph=DF.foldl' (\this_glyph this_msdf_glyph->let (glyph,key)=transform this_msdf_glyph in intmap_insert key glyph this_glyph) font.glyph msdf_glyph})

from_msdf_glyph::FCT.CFloat->FCT.CFloat->FCT.CFloat->FCT.CFloat->MSDF_Glyph->(Glyph,Int)
from_msdf_glyph x y reciprocal_width reciprocal_height msdf_glyph=case msdf_glyph of
    MSDF_Glyph {msdf_unicode,msdf_advance,msdf_plane_bounds,msdf_atlas_bounds}->case msdf_plane_bounds of
        MSDF_Bounds {msdf_left=plane_left,msdf_bottom=plane_bottom,msdf_right=plane_right,msdf_top=plane_top}->case msdf_atlas_bounds of
            MSDF_Bounds {msdf_left=atlas_left,msdf_bottom=atlas_bottom,msdf_right=atlas_right,msdf_top=atlas_top}->
                (Glyph {advance=msdf_advance,left=plane_left,down=negate plane_bottom,right=plane_right,up=negate plane_top,min_u=(x+atlas_left)*reciprocal_width,min_v=(y+atlas_bottom)*reciprocal_height,max_u=(x+atlas_right)*reciprocal_width,max_v=(y+atlas_top)*reciprocal_height},msdf_unicode)