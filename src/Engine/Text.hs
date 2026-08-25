{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Engine.Text where

import Engine.Atlas
import Engine.Container
import Engine.Type
import Engine.Underlying
import qualified MSDF.Function as MSDFF
import qualified MSDF.Type as MSDFT
import qualified SDL.Function as SDLF
import qualified Error.Function as EF
import qualified Error.Type as ET
import qualified Data.Char as DC
import qualified Data.Foldable as DF
import qualified Data.HashMap.Strict as DHMS
import qualified Data.HashSet as DHS
import qualified Data.IntMap as DIM
import qualified Data.IntSet as DIS
import qualified Data.Sequence as DS
import qualified Data.Text as DT
import qualified Data.Word as DW
import qualified Foreign.C.Types as FCT
import qualified Foreign.Marshal.Array as FMA
import qualified Foreign.Marshal.Utils as FMU
import qualified Foreign.Ptr as FP
import qualified Foreign.Storable as FS

do_typesetting::ET.Has_call_stack=>Int->FCT.CFloat->(Int->(FCT.CFloat,FCT.CFloat,FCT.CFloat))->DS.Seq (DS.Seq Row)->(DS.Seq (DS.Seq Row),FCT.CFloat)
do_typesetting number height calculate_typesetting article=let (new_article,y,index)=do_typesetting_a 0 (-height) calculate_typesetting article DS.empty in (new_article,y+do_typesetting_c index (number-1) calculate_typesetting)

do_typesetting_a::ET.Has_call_stack=>Int->FCT.CFloat->(Int->(FCT.CFloat,FCT.CFloat,FCT.CFloat))->DS.Seq (DS.Seq Row)->DS.Seq (DS.Seq Row)->(DS.Seq (DS.Seq Row),FCT.CFloat,Int)
do_typesetting_a index y calculate_typesetting article this_article=case article of
    DS.Empty->(this_article,y,index)
    paragraph DS.:<| other_paragraph->let (new_paragraph,new_y,new_index)=do_typesetting_b index y calculate_typesetting paragraph in do_typesetting_a new_index new_y calculate_typesetting other_paragraph (this_article DS.|> new_paragraph)

do_typesetting_b::ET.Has_call_stack=>Int->FCT.CFloat->(Int->(FCT.CFloat,FCT.CFloat,FCT.CFloat))->DS.Seq Row->(DS.Seq Row,FCT.CFloat,Int)
do_typesetting_b this_index y calculate_typesetting paragraph=case paragraph of
    DS.Empty->(DS.empty,y,this_index)
    row DS.:<| other_row->case row of
        Row {row_core,index,width,min_down,max_up,min_descent,max_ascent}->let (lower,upper,x)=calculate_typesetting index in let new_y=y+upper+do_typesetting_c this_index (index-1) calculate_typesetting in let (final_paragraph,final_y,final_index)=do_typesetting_b (index+1) (new_y+lower) calculate_typesetting other_row in (Row {row_core=row_core,x=x-width/2,index=index,y=new_y,width=width,min_down=min_down,max_up=max_up,min_descent=min_descent,max_ascent=max_ascent} DS.<| final_paragraph,final_y,final_index)

do_typesetting_c::ET.Has_call_stack=>Int->Int->(Int->(FCT.CFloat,FCT.CFloat,FCT.CFloat))->FCT.CFloat
do_typesetting_c start_index end_index calculate_typesetting=if end_index<start_index then 0 else let (lower,upper,_)=calculate_typesetting start_index in lower+upper+do_typesetting_c (start_index+1) end_index calculate_typesetting

for_text::ET.Has_call_stack=>DIM.IntMap Font->DHMS.HashMap String Int->DS.Seq (DS.Seq Sentence)->(DS.Seq Row->DS.Seq (DS.Seq Row)->Int->FCT.CFloat)->(DS.Seq (DS.Seq Row),Int)
for_text font font_map article calculate_width=for_text_a 0 font font_map article calculate_width DS.empty

for_text_a::ET.Has_call_stack=>Int->DIM.IntMap Font->DHMS.HashMap String Int->DS.Seq (DS.Seq Sentence)->(DS.Seq Row->DS.Seq (DS.Seq Row)->Int->FCT.CFloat)->DS.Seq (DS.Seq Row)->(DS.Seq (DS.Seq Row),Int)
for_text_a index font font_map article calculate_width this_article=case article of
    DS.Empty->(this_article,index)
    paragraph DS.:<| other_paragraph->if DS.null paragraph then for_text_a (index+1) font font_map other_paragraph calculate_width (this_article DS.|> DS.empty) else let (new_article,new_index)=for_text_b index font font_map paragraph Positive_infinity Negative_infinity Positive_infinity Negative_infinity 0 (calculate_width DS.empty this_article index) calculate_width DS.empty DS.empty this_article in for_text_a new_index font font_map other_paragraph calculate_width new_article

for_text_b::ET.Has_call_stack=>Int->DIM.IntMap Font->DHMS.HashMap String Int->DS.Seq Sentence->Extended->Extended->Extended->Extended->FCT.CFloat->FCT.CFloat->(DS.Seq Row->DS.Seq (DS.Seq Row)->Int->FCT.CFloat)->DS.Seq Character->DS.Seq Row->DS.Seq (DS.Seq Row)->(DS.Seq (DS.Seq Row),Int)
for_text_b index font font_map paragraph min_down max_up min_descent max_ascent x width calculate_width row_core this_paragraph article=case paragraph of
    DS.Empty->(article DS.|> (this_paragraph DS.|> Row {row_core=row_core,index=index,x=0,y=0,width=x,min_down=from_extended min_down,max_up=from_extended max_up,min_descent=from_extended min_descent,max_ascent=from_extended max_ascent}),index+1)
    sentence DS.:<| other_sentence->case sentence of
        Sentence {sentence_core,path}->let font_id=hash_map_lookup path font_map in let single_font=int_map_lookup font_id font in let (new_paragraph,new_row_core,new_width,new_x,new_max_ascent,new_min_descent,new_max_up,new_min_down,new_index)=for_text_c index font_id single_font.glyph sentence_core min_down max_up min_descent max_ascent single_font.descent single_font.ascent x width (`calculate_width` article) row_core this_paragraph in for_text_b new_index font font_map other_sentence new_min_down new_max_up new_min_descent new_max_ascent new_x new_width calculate_width new_row_core new_paragraph article

for_text_c::ET.Has_call_stack=>Int->Int->DIM.IntMap Glyph->DS.Seq Phrase->Extended->Extended->Extended->Extended->FCT.CFloat->FCT.CFloat->FCT.CFloat->FCT.CFloat->(DS.Seq Row->Int->FCT.CFloat)->DS.Seq Character->DS.Seq Row->(DS.Seq Row,DS.Seq Character,FCT.CFloat,FCT.CFloat,Extended,Extended,Extended,Extended,Int)
for_text_c index font_id glyph sentence_core min_down max_up min_descent max_ascent descent ascent x width calculate_width row_core paragraph=case sentence_core of
    DS.Empty->(paragraph,row_core,width,x,max_ascent,min_descent,max_up,min_down,index)
    phrase DS.:<| other_phrase->case phrase of
        Phrase {phrase_core,font_size,color}->let (new_paragraph,new_row_core,new_width,new_x,new_max_ascent,new_min_descent,new_max_up,new_min_down,new_index)=let new_descent=descent*font_size in let new_ascent=ascent*font_size in for_text_d index font_id glyph phrase_core min_down max_up (min (to_extended new_descent) min_descent) (max (to_extended new_ascent) max_ascent) new_descent new_ascent font_size x width color calculate_width row_core paragraph in for_text_c new_index font_id glyph other_phrase new_min_down new_max_up new_min_descent new_max_ascent descent ascent new_x new_width calculate_width new_row_core new_paragraph

for_text_d::ET.Has_call_stack=>Int->Int->DIM.IntMap Glyph->DT.Text->Extended->Extended->Extended->Extended->FCT.CFloat->FCT.CFloat->FCT.CFloat->FCT.CFloat->FCT.CFloat->Color->(DS.Seq Row->Int->FCT.CFloat)->DS.Seq Character->DS.Seq Row->(DS.Seq Row,DS.Seq Character,FCT.CFloat,FCT.CFloat,Extended,Extended,Extended,Extended,Int)
for_text_d index font_id glyph text min_down max_up min_descent max_ascent descent ascent font_size x width color calculate_width row_core paragraph=case text of
    DT.Empty->(paragraph,row_core,width,x,max_ascent,min_descent,max_up,min_down,index)
    char DT.:< other_char->let unicode=DC.ord char in case int_map_lookup unicode glyph of
        Glyph {advance,left,down,right,up,min_u,min_v,max_u,max_v}->let new_advance=advance*font_size in let new_left=left*font_size in let new_down=down*font_size in let new_right=right*font_size in let new_up=up*font_size in let extended_down=to_extended new_down in let extended_up=to_extended new_up in if width<x+new_right then let new_paragraph=paragraph DS.|> Row {row_core=row_core,index=index,x=0,y=0,width=x,min_down=from_extended min_down,max_up=from_extended max_up,min_descent=from_extended min_descent,max_ascent=from_extended max_ascent} in let (new_width,new_index)=for_text_e (index+1) new_right calculate_width new_paragraph in for_text_d new_index font_id glyph other_char extended_down extended_up (to_extended descent) (to_extended ascent) descent ascent font_size new_advance new_width color calculate_width (DS.singleton (Character {unicode=unicode,font_id=font_id,font_size=font_size,left=new_left,down=new_down,right=new_right,up=new_up,min_u=min_u,min_v=min_v,max_u=max_u,max_v=max_v,color=color})) new_paragraph else for_text_d index font_id glyph other_char (min extended_down min_down) (max extended_up max_up) min_descent max_ascent descent ascent font_size (x+new_advance) width color calculate_width (row_core DS.|> Character {unicode=unicode,font_id=font_id,font_size=font_size,left=x+new_left,down=new_down,right=x+new_right,up=new_up,min_u=min_u,min_v=min_v,max_u=max_u,max_v=max_v,color=color}) paragraph

for_text_e::ET.Has_call_stack=>Int->FCT.CFloat->(DS.Seq Row->Int->FCT.CFloat)->DS.Seq Row->(FCT.CFloat,Int)
for_text_e index right calculate_width paragraph=let width=calculate_width paragraph index in if right<width then (width,index) else for_text_e (index+1) right calculate_width paragraph

to_charset::ET.Has_call_stack=>DS.Seq (DS.Seq Sentence)->DHMS.HashMap String (DHS.HashSet Char)
to_charset=DF.foldl' (DF.foldl' (flip to_charset_a)) DHMS.empty

to_charset_a::ET.Has_call_stack=>Sentence->DHMS.HashMap String (DHS.HashSet Char)->DHMS.HashMap String (DHS.HashSet Char)
to_charset_a sentence charset=case sentence of
    Sentence {sentence_core,path}->DHMS.insert path (DF.foldl' (\this_charset phrase->DT.foldl' (flip DHS.insert) this_charset phrase.phrase_core) (DHMS.findWithDefault DHS.empty path charset) sentence_core) charset

from_maybe_charset::ET.Has_call_stack=>String->Maybe (DHS.HashSet Char)->Engine a b c d e->IO (Engine a b c d e)
from_maybe_charset path maybe_charset engine=case DHMS.lookup path engine.font_map of
    Nothing->from_maybe_charset_a engine.font_id DIS.empty path maybe_charset (engine {font_map=hash_map_insert path engine.font_id engine.font_map,font_id=engine.font_id+1})
    Just font_id->case DIM.lookup font_id engine.font of
        Nothing->from_maybe_charset_a font_id DIS.empty path maybe_charset engine
        Just font->from_maybe_charset_a font_id (DIM.keysSet font.glyph) path maybe_charset engine

from_maybe_charset_a::ET.Has_call_stack=>Int->DIS.IntSet->String->Maybe (DHS.HashSet Char)->Engine a b c d e->IO (Engine a b c d e)
from_maybe_charset_a font_id unicode path maybe_charset engine=case maybe_charset of
    Just charset->from_charset_a False font_id (DIS.difference (DHS.foldl' (\this_unicode char->DIS.insert (DC.ord char) this_unicode) DIS.empty charset) unicode) path engine
    Nothing->from_charset_a True font_id unicode path engine

fold_from_charset::ET.Has_call_stack=>DHMS.HashMap String (DHS.HashSet Char)->Engine a b c d e->IO (Engine a b c d e)
fold_from_charset charset engine=DHMS.foldlWithKey' (\action path char->action>>=from_charset path char) (return engine) charset

from_charset::ET.Has_call_stack=>String->DHS.HashSet Char->Engine a b c d e->IO (Engine a b c d e)
from_charset path charset engine=let unicode=DHS.foldl' (\this_unicode char->DIS.insert (DC.ord char) this_unicode) DIS.empty charset in case DHMS.lookup path engine.font_map of
    Nothing->from_charset_a False engine.font_id unicode path (engine {font_map=hash_map_insert path engine.font_id engine.font_map,font_id=engine.font_id+1})
    Just font_id->case DIM.lookup font_id engine.font of
        Nothing->from_charset_a False font_id unicode path engine
        Just font->from_charset_a False font_id (DIS.difference unicode (DIM.keysSet font.glyph)) path engine

from_charset_a::ET.Has_call_stack=>Bool->Int->DIS.IntSet->String->Engine a b c d e->IO (Engine a b c d e)
from_charset_a exclude font_id unicode path engine=if not exclude&&DIS.null unicode then return engine else with_string path $ \this_path->let size=DIS.size unicode in FMA.allocaArray size $ \ptr_charset->do
    DIS.foldr (flip . from_charset_b) (const (return ())) unicode ptr_charset
    ptr_msdf_output<-MSDFF.msdf_generator this_path (FMU.fromBool exclude) ptr_charset (fromIntegral size) engine.font_size engine.pixel_range
    catch_null ptr_msdf_output
    msdf_output<-FS.peek ptr_msdf_output
    case msdf_output of
        MSDFT.MSDF_Output {msdf_pixel,msdf_width,msdf_height,msdf_descent,msdf_ascent,msdf_glyph,msdf_count}->if msdf_count==0
            then do
                MSDFF.msdf_cleaner ptr_msdf_output
                return engine
            else let new_msdf_width=fromIntegral msdf_width in let new_msdf_height=fromIntegral msdf_height in do
                texture<-from_pixel engine.device engine.picture_transfer_buffer engine.max_picture_size msdf_pixel new_msdf_width new_msdf_height
                let (atlas,left,down,_,_)=atlas_insert new_msdf_width new_msdf_height engine.padding engine.atlas
                copy_texture engine.device texture engine.texture left down new_msdf_width new_msdf_height
                SDLF.sdl_release_gpu_texture engine.device texture
                glyph<-from_charset_c (fromIntegral left) (fromIntegral down) engine.exponent_width engine.exponent_height 0 (fromIntegral msdf_count) msdf_glyph DIM.empty
                MSDFF.msdf_cleaner ptr_msdf_output
                case DIM.lookup font_id engine.font of
                    Nothing->return (engine {atlas=atlas,font=DIM.insert font_id (Font {descent=msdf_descent,ascent=msdf_ascent,glyph=glyph}) engine.font})
                    Just font->return (engine {atlas=atlas,font=DIM.insert font_id (Font {descent=msdf_descent,ascent=msdf_ascent,glyph=DIM.union glyph font.glyph}) engine.font})

from_charset_b::ET.Has_call_stack=>Int->FP.Ptr DW.Word32->(FP.Ptr DW.Word32->IO ())->IO ()
from_charset_b int ptr action=do
    FS.poke ptr (fromIntegral int)
    action (FP.plusPtr ptr 4)

from_charset_c::ET.Has_call_stack=>FCT.CFloat->FCT.CFloat->Int->Int->Int->Int->FP.Ptr MSDFT.MSDF_Glyph->DIM.IntMap Glyph->IO (DIM.IntMap Glyph)
from_charset_c x y exponent_width exponent_height index msdf_count msdf_glyph glyph=if msdf_count<=index then return glyph else do
    single_msdf_glyph<-FS.peekElemOff msdf_glyph index
    case single_msdf_glyph of
        MSDFT.MSDF_Glyph {msdf_unicode,msdf_advance,msdf_plane_left,msdf_plane_down,msdf_plane_right,msdf_plane_up,msdf_atlas_left,msdf_atlas_down,msdf_atlas_right,msdf_atlas_up}->from_charset_c x y exponent_width exponent_height (index+1) msdf_count msdf_glyph (DIM.insert (fromIntegral msdf_unicode) (Glyph {advance=msdf_advance,left=msdf_plane_left,down=msdf_plane_down,right=msdf_plane_right,up=msdf_plane_up,min_u=scaleFloat (-exponent_width) (x+msdf_atlas_left),min_v=scaleFloat (-exponent_height) (y+msdf_atlas_down),max_u=scaleFloat (-exponent_width) (x+msdf_atlas_right),max_v=scaleFloat (-exponent_height) (y+msdf_atlas_up)}) glyph)

scroll_text::ET.Has_call_stack=>FCT.CFloat->Visual->Visual
scroll_text scroll visual=case visual of
    Text {arrange,half_width,half_height,current_y,min_y,max_y,article,charset,locked}->Text {arrange=arrange,half_width=half_width,half_height=half_height,current_y=max min_y (min (max min_y max_y) (current_y+scroll)),min_y=min_y,max_y=max_y,article=article,charset=charset,locked=locked}
    _->EF.empty_error

scroll_top_text::ET.Has_call_stack=>Visual->Visual
scroll_top_text visual=case visual of
    Text {arrange,half_width,half_height,min_y,max_y,article,charset,locked}->Text {arrange=arrange,half_width=half_width,half_height=half_height,current_y=min_y,min_y=min_y,max_y=max_y,article=article,charset=charset,locked=locked}
    _->EF.empty_error

scroll_bottom_text::ET.Has_call_stack=>Visual->Visual
scroll_bottom_text visual=case visual of
    Text {arrange,half_width,half_height,min_y,max_y,article,charset,locked}->Text {arrange=arrange,half_width=half_width,half_height=half_height,current_y=max min_y max_y,min_y=min_y,max_y=max_y,article=article,charset=charset,locked=locked}
    _->EF.empty_error

{-# INLINE do_typesetting #-}
{-# INLINE for_text #-}
{-# INLINE to_charset #-}
{-# INLINE to_charset_a #-}
{-# INLINE from_maybe_charset #-}
{-# INLINE from_maybe_charset_a #-}
{-# INLINE fold_from_charset #-}
{-# INLINE from_charset #-}
{-# INLINE from_charset_a #-}
{-# INLINE from_charset_b #-}
{-# INLINE scroll_text #-}
{-# INLINE scroll_top_text #-}
{-# INLINE scroll_bottom_text #-}