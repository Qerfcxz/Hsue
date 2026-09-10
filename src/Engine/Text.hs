{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Engine.Text where

import Engine.Atlas
import Engine.Container
import Engine.Type
import Engine.Underlying
import qualified MSDF.Function as MSDFF
import qualified MSDF.Include as MSDFI
import qualified SDL.Function as SDLF
import qualified Error.Function as EF
import qualified Error.Type as ET
import qualified Control.Monad as CM
import qualified Control.Monad.ST as CMST
import qualified Data.Char as DC
import qualified Data.Foldable as DF
import qualified Data.HashMap.Strict as DHMS
import qualified Data.HashSet as DHS
import qualified Data.IntMap as DIM
import qualified Data.IntSet as DIS
import qualified Data.Sequence as DS
import qualified Data.Text as DT
import qualified Data.Vector.Storable as DVS
import qualified Data.Vector.Storable.Mutable as DVSM
import qualified Foreign.C.Types as FCT
import qualified Foreign.Marshal.Array as FMA
import qualified Foreign.Marshal.Utils as FMU
import qualified Foreign.Ptr as FP
import qualified Foreign.Storable as FS

do_typesetting::ET.Has_call_stack=>Int->FCT.CFloat->(Int->(FCT.CFloat,FCT.CFloat,FCT.CFloat))->DIM.IntMap Int->DVS.Vector Hole->DS.Seq (DS.Seq Row)->(DS.Seq (DS.Seq Row),DVS.Vector Hole,FCT.CFloat)
do_typesetting number height calculate_typesetting hole_index hole article=CMST.runST $ do
    new_hole<-DVS.unsafeThaw hole
    (new_article,y,index)<-do_typesetting_a 0 (negate height) calculate_typesetting hole_index new_hole article DS.empty
    new_new_hole<-DVS.unsafeFreeze new_hole
    return (new_article,new_new_hole,y+accumulate_typesetting index (number-1) calculate_typesetting)

do_typesetting_a::ET.Has_call_stack=>Int->FCT.CFloat->(Int->(FCT.CFloat,FCT.CFloat,FCT.CFloat))->DIM.IntMap Int->DVSM.MVector a Hole->DS.Seq (DS.Seq Row)->DS.Seq (DS.Seq Row)->CMST.ST a (DS.Seq (DS.Seq Row),FCT.CFloat,Int)
do_typesetting_a index y calculate_typesetting hole_index hole article this_article=case article of
    DS.Empty->return (this_article,y,index)
    paragraph DS.:<| other_paragraph->do
        (new_paragraph,new_y,new_index)<-do_typesetting_b index y calculate_typesetting hole_index hole paragraph DS.empty
        do_typesetting_a new_index new_y calculate_typesetting hole_index hole other_paragraph (this_article DS.|> new_paragraph)

do_typesetting_b::ET.Has_call_stack=>Int->FCT.CFloat->(Int->(FCT.CFloat,FCT.CFloat,FCT.CFloat))->DIM.IntMap Int->DVSM.MVector a Hole->DS.Seq Row->DS.Seq Row->CMST.ST a (DS.Seq Row,FCT.CFloat,Int)
do_typesetting_b this_index y calculate_typesetting hole_index hole paragraph this_paragraph=case paragraph of
    DS.Empty->return (this_paragraph,y,this_index)
    row DS.:<| other_row->case row of
        Row {row_core,index,number,width,min_down,max_up,min_descent,max_ascent}->let (lower,upper,x)=calculate_typesetting index in let new_x=x-width/2 in let new_y=y+upper+accumulate_typesetting this_index (index-1) calculate_typesetting in do
            monad_for (int_map_lookup index hole_index) (int_map_lookup (index+1) hole_index-1) (do_typesetting_c new_x new_y hole)
            do_typesetting_b (index+1) (new_y+lower) calculate_typesetting hole_index hole other_row (this_paragraph DS.|> Row {row_core=row_core,index=index,number=number,x=new_x,y=new_y,width=width,min_down=min_down,max_up=max_up,min_descent=min_descent,max_ascent=max_ascent})

do_typesetting_c::ET.Has_call_stack=>FCT.CFloat->FCT.CFloat->DVSM.MVector a Hole->Int->CMST.ST a ()
do_typesetting_c x y hole index=do
    single_hole<-DVSM.unsafeRead hole index
    case single_hole of
        Hole {hole_id,enable,red,green,blue,alpha,left,down,right,up}->DVSM.unsafeWrite hole index (Hole {hole_id=hole_id,enable=enable,red=red,green=green,blue=blue,alpha=alpha,left=left,down=down,right=right,up=up,x=x,y=y})

accumulate_typesetting::ET.Has_call_stack=>Int->Int->(Int->(FCT.CFloat,FCT.CFloat,FCT.CFloat))->FCT.CFloat
accumulate_typesetting start_index end_index calculate_typesetting=if end_index<start_index then 0 else let (lower,upper,_)=calculate_typesetting start_index in lower+upper+accumulate_typesetting (start_index+1) end_index calculate_typesetting

for_text::ET.Has_call_stack=>Int->Int->DIM.IntMap Font->DHMS.HashMap DT.Text Int->(FCT.CFloat,FCT.CFloat,FCT.CFloat,FCT.CFloat,FCT.CFloat,FCT.CFloat,FCT.CFloat,FCT.CFloat,FCT.CFloat)->(DS.Seq Row->DS.Seq (DS.Seq Row)->Int->FCT.CFloat)->DS.Seq (DS.Seq Sentence)->(DS.Seq (DS.Seq Row),DVS.Vector Hole,DIM.IntMap Int,Int)
for_text count max_search_index font font_map failure_glyph calculate_width article=CMST.runST $ do
    hole<-DVSM.new count
    (new_article,hole_index,index)<-for_text_a 0 0 max_search_index font font_map failure_glyph calculate_width (DIM.singleton 0 0) hole article DS.empty
    new_hole<-DVS.unsafeFreeze hole
    return (new_article,new_hole,hole_index,index)

for_text_a::ET.Has_call_stack=>Int->Int->Int->DIM.IntMap Font->DHMS.HashMap DT.Text Int->(FCT.CFloat,FCT.CFloat,FCT.CFloat,FCT.CFloat,FCT.CFloat,FCT.CFloat,FCT.CFloat,FCT.CFloat,FCT.CFloat)->(DS.Seq Row->DS.Seq (DS.Seq Row)->Int->FCT.CFloat)->DIM.IntMap Int->DVSM.MVector a Hole->DS.Seq (DS.Seq Sentence)->DS.Seq (DS.Seq Row)->CMST.ST a (DS.Seq (DS.Seq Row),DIM.IntMap Int,Int)
for_text_a count index max_search_index font font_map failure_glyph calculate_width hole_index hole article this_article=case article of
    DS.Empty->return (this_article,hole_index,index)
    paragraph DS.:<| other_paragraph->do
        (new_paragraph,new_hole_index,new_index,new_count)<-for_text_b True 0 count index max_search_index font font_map (1/0) (negate 1/0) (1/0) (negate 1/0) 0 (calculate_width DS.empty this_article index) failure_glyph (`calculate_width` this_article) hole_index hole paragraph DS.empty DS.empty
        for_text_a new_count new_index max_search_index font font_map failure_glyph calculate_width new_hole_index hole other_paragraph (this_article DS.|> new_paragraph)

for_text_b::ET.Has_call_stack=>Bool->Int->Int->Int->Int->DIM.IntMap Font->DHMS.HashMap DT.Text Int->FCT.CFloat->FCT.CFloat->FCT.CFloat->FCT.CFloat->FCT.CFloat->FCT.CFloat->(FCT.CFloat,FCT.CFloat,FCT.CFloat,FCT.CFloat,FCT.CFloat,FCT.CFloat,FCT.CFloat,FCT.CFloat,FCT.CFloat)->(DS.Seq Row->Int->FCT.CFloat)->DIM.IntMap Int->DVSM.MVector a Hole->DS.Seq Sentence->DS.Seq Character->DS.Seq Row->CMST.ST a (DS.Seq Row,DIM.IntMap Int,Int,Int)
for_text_b empty number count index max_search_index font font_map min_down max_up min_descent max_ascent x width failure_glyph calculate_width hole_index hole paragraph row_core this_paragraph=case paragraph of
    DS.Empty->let new_index=index+1 in return (if empty then this_paragraph else this_paragraph DS.|> Row {row_core=row_core,index=index,number=number,x=0,y=0,width=x,min_down=min_down,max_up=max_up,min_descent=min_descent,max_ascent=max_ascent},int_map_insert_strict new_index count hole_index,new_index,count)
    sentence DS.:<| other_sentence->case sentence of
        Sentence {sentence_core,path}->let font_id=hash_map_lookup path font_map in let single_font=int_map_lookup font_id font in do
            (new_empty,new_paragraph,new_row_core,new_hole_index,new_width,new_x,new_max_ascent,new_min_descent,new_max_up,new_min_down,new_index,new_count,new_number)<-for_text_c empty number count index max_search_index font_id single_font.glyph min_down max_up min_descent max_ascent single_font.descent single_font.ascent x width failure_glyph calculate_width hole_index hole sentence_core row_core this_paragraph
            for_text_b new_empty new_number new_count new_index max_search_index font font_map new_min_down new_max_up new_min_descent new_max_ascent new_x new_width failure_glyph calculate_width new_hole_index hole other_sentence new_row_core new_paragraph

for_text_c::ET.Has_call_stack=>Bool->Int->Int->Int->Int->Int->DIM.IntMap Glyph->FCT.CFloat->FCT.CFloat->FCT.CFloat->FCT.CFloat->FCT.CFloat->FCT.CFloat->FCT.CFloat->FCT.CFloat->(FCT.CFloat,FCT.CFloat,FCT.CFloat,FCT.CFloat,FCT.CFloat,FCT.CFloat,FCT.CFloat,FCT.CFloat,FCT.CFloat)->(DS.Seq Row->Int->FCT.CFloat)->DIM.IntMap Int->DVSM.MVector a Hole->DS.Seq Phrase->DS.Seq Character->DS.Seq Row->CMST.ST a (Bool,DS.Seq Row,DS.Seq Character,DIM.IntMap Int,FCT.CFloat,FCT.CFloat,FCT.CFloat,FCT.CFloat,FCT.CFloat,FCT.CFloat,Int,Int,Int)
for_text_c empty number count index max_search_index font_id glyph min_down max_up min_descent max_ascent descent ascent x width failure_glyph calculate_width hole_index hole sentence_core row_core paragraph=case sentence_core of
    DS.Empty->return (empty,paragraph,row_core,hole_index,width,x,max_ascent,min_descent,max_up,min_down,index,count,number)
    phrase DS.:<| other_phrase->case phrase of
        Phrase {phrase_core,font_size,color}->let new_descent=descent*font_size in let new_ascent=ascent*font_size in let (new_empty,new_paragraph,new_row_core,new_hole_index,new_width,new_x,new_max_ascent,new_min_descent,new_max_up,new_min_down,new_index,new_number)=for_text_d empty number count index max_search_index font_id glyph color font_size min_down max_up (min new_descent min_descent) (max new_ascent max_ascent) new_descent new_ascent x width failure_glyph calculate_width hole_index phrase_core row_core paragraph in for_text_c new_empty new_number count new_index max_search_index font_id glyph new_min_down new_max_up new_min_descent new_max_ascent descent ascent new_x new_width failure_glyph calculate_width new_hole_index hole other_phrase new_row_core new_paragraph
        Hole_phrase {hole_id,maybe_color,advance,left,down,right,up}->let (enable,red,green,blue,alpha)=from_maybe_color maybe_color in let new_x=x+right in if width<new_x
            then let new_paragraph=if empty then paragraph else paragraph DS.|> Row {row_core=row_core,index=index,number=number,x=0,y=0,width=x,min_down=min_down,max_up=max_up,min_descent=min_descent,max_ascent=max_ascent} in let new_index=index+1 in let (new_width,new_new_index)=search_width new_index (new_index+max_search_index) right calculate_width new_paragraph in do
                DVSM.unsafeWrite hole count (Hole {hole_id=hole_id,enable=enable,red=red,green=green,blue=blue,alpha=alpha,left=left,down=down,right=right,up=up,x=0,y=0})
                for_text_c False (if enable then 1 else 0) (count+1) new_new_index max_search_index font_id glyph down up down up descent ascent advance new_width failure_glyph calculate_width (if index+1==new_new_index then int_map_insert_strict new_new_index count hole_index else int_map_insert_strict new_new_index count (int_map_insert_strict (index+1) count hole_index)) hole other_phrase DS.empty new_paragraph
            else do
                DVSM.unsafeWrite hole count (Hole {hole_id=hole_id,enable=enable,red=red,green=green,blue=blue,alpha=alpha,left=x+left,down=down,right=x+right,up=up,x=0,y=0})
                for_text_c False (number+if enable then 1 else 0) (count+1) index max_search_index font_id glyph (min down min_down) (max up max_up) (min down min_descent) (max up max_ascent) descent ascent (x+advance) width failure_glyph calculate_width hole_index hole other_phrase row_core paragraph

for_text_d::ET.Has_call_stack=>Bool->Int->Int->Int->Int->Int->DIM.IntMap Glyph->Color->FCT.CFloat->FCT.CFloat->FCT.CFloat->FCT.CFloat->FCT.CFloat->FCT.CFloat->FCT.CFloat->FCT.CFloat->FCT.CFloat->(FCT.CFloat,FCT.CFloat,FCT.CFloat,FCT.CFloat,FCT.CFloat,FCT.CFloat,FCT.CFloat,FCT.CFloat,FCT.CFloat)->(DS.Seq Row->Int->FCT.CFloat)->DIM.IntMap Int->DT.Text->DS.Seq Character->DS.Seq Row->(Bool,DS.Seq Row,DS.Seq Character,DIM.IntMap Int,FCT.CFloat,FCT.CFloat,FCT.CFloat,FCT.CFloat,FCT.CFloat,FCT.CFloat,Int,Int)
for_text_d empty number count index max_search_index font_id glyph color font_size min_down max_up min_descent max_ascent descent ascent x width failure_glyph calculate_width hole_index phrase_core row_core paragraph=case phrase_core of
    DT.Empty->(empty,paragraph,row_core,hole_index,width,x,max_ascent,min_descent,max_up,min_down,index,number)
    char DT.:< other_char->let unicode=DC.ord char in let (advance,left,down,right,up,min_u,min_v,max_u,max_v)=lookup_glyph unicode glyph failure_glyph in let new_advance=advance*font_size in let new_left=left*font_size in let new_down=down*font_size in let new_right=right*font_size in let new_up=up*font_size in let new_x=x+new_right in if width<new_x then let new_paragraph=if empty then paragraph else paragraph DS.|> Row {row_core=row_core,index=index,number=number,x=0,y=0,width=x,min_down=min_down,max_up=max_up,min_descent=min_descent,max_ascent=max_ascent} in let new_index=index+1 in let (new_width,new_new_index)=search_width new_index (new_index+max_search_index) new_right calculate_width new_paragraph in for_text_d False 1 count new_new_index max_search_index font_id glyph color font_size new_down new_up descent ascent descent ascent new_advance new_width failure_glyph calculate_width (if index+1==new_new_index then int_map_insert_strict new_new_index count hole_index else int_map_insert_strict new_new_index count (int_map_insert_strict (index+1) count hole_index)) other_char (DS.singleton (Character {unicode=unicode,font_id=font_id,font_size=font_size,left=new_left,down=new_down,right=new_right,up=new_up,min_u=min_u,min_v=min_v,max_u=max_u,max_v=max_v,color=color})) new_paragraph else for_text_d False (number+1) count index max_search_index font_id glyph color font_size (min new_down min_down) (max new_up max_up) min_descent max_ascent descent ascent (x+new_advance) width failure_glyph calculate_width hole_index other_char (row_core DS.|> Character {unicode=unicode,font_id=font_id,font_size=font_size,left=x+new_left,down=new_down,right=new_x,up=new_up,min_u=min_u,min_v=min_v,max_u=max_u,max_v=max_v,color=color}) paragraph

from_maybe_color::ET.Has_call_stack=>Maybe Color->(Bool,FCT.CFloat,FCT.CFloat,FCT.CFloat,FCT.CFloat)
from_maybe_color maybe_color=case maybe_color of
    Nothing->(False,0,0,0,0)
    Just color->case color of
        Color {red,green,blue,alpha}->(True,red,green,blue,alpha)

search_width::ET.Has_call_stack=>Int->Int->FCT.CFloat->(DS.Seq Row->Int->FCT.CFloat)->DS.Seq Row->(FCT.CFloat,Int)
search_width index max_index width calculate_width paragraph=if index==max_index then EF.empty_error else let new_width=calculate_width paragraph index in if new_width<width then search_width (index+1) max_index width calculate_width paragraph else (new_width,index)

lookup_glyph::ET.Has_call_stack=>Int->DIM.IntMap Glyph->(FCT.CFloat,FCT.CFloat,FCT.CFloat,FCT.CFloat,FCT.CFloat,FCT.CFloat,FCT.CFloat,FCT.CFloat,FCT.CFloat)->(FCT.CFloat,FCT.CFloat,FCT.CFloat,FCT.CFloat,FCT.CFloat,FCT.CFloat,FCT.CFloat,FCT.CFloat,FCT.CFloat)
lookup_glyph unicode glyph failure_glyph=case DIM.lookup unicode glyph of
    Nothing->failure_glyph
    Just (Glyph {advance,left,down,right,up,min_u,min_v,max_u,max_v})->(advance,left,down,right,up,min_u,min_v,max_u,max_v)

summarize_text::ET.Has_call_stack=>DS.Seq (DS.Seq Sentence)->(DHMS.HashMap DT.Text (DHS.HashSet Char),Int)
summarize_text=DF.foldl' (DF.foldl' (flip summarize_text_a)) (DHMS.empty,0)

summarize_text_a::ET.Has_call_stack=>Sentence->(DHMS.HashMap DT.Text (DHS.HashSet Char),Int)->(DHMS.HashMap DT.Text (DHS.HashSet Char),Int)
summarize_text_a sentence (charset,count)=case sentence of
    Sentence {sentence_core,path}->let (new_charset,new_count)=DF.foldl' (flip summarize_text_b) (DHS.empty,0) sentence_core in (DHMS.insertWith DHS.union path new_charset charset,count+new_count)

summarize_text_b::ET.Has_call_stack=>Phrase->(DHS.HashSet Char,Int)->(DHS.HashSet Char,Int)
summarize_text_b phrase (charset,count)=case phrase of
    Phrase {phrase_core}->(DT.foldl' (flip DHS.insert) charset phrase_core,count)
    Hole_phrase {}->(charset,count+1)

update_font::ET.Has_call_stack=>DT.Text->Maybe (DHS.HashSet Char)->Engine a->IO (Engine a)
update_font path maybe_charset engine=case DHMS.lookup path engine.font_map of
    Nothing->let new_engine=engine {font_map=hash_map_insert_strict path engine.font_id engine.font_map,font_id=engine.font_id+1} in case maybe_charset of
        Nothing->from_charset_b True DIM.empty engine.font_id path DIS.empty new_engine
        Just charset->from_charset_b False DIM.empty engine.font_id path (DHS.foldl' (\this_unicode char->DIS.insert (DC.ord char) this_unicode) DIS.empty charset) new_engine
    Just font_id->let font=int_map_lookup font_id engine.font in case maybe_charset of
        Nothing->from_charset_b True font.glyph font_id path (DIM.keysSet font.glyph) engine
        Just charset->let new_unicode=DIS.difference (DHS.foldl' (\this_unicode char->DIS.insert (DC.ord char) this_unicode) DIS.empty charset) (DIM.keysSet font.glyph) in if DIS.null new_unicode then return engine else from_charset_b False font.glyph font_id path new_unicode engine

update_atlas_font::ET.Has_call_stack=>Bool->Int->DT.Text->Maybe (DHS.HashSet Char)->Engine a->IO (Engine a)
update_atlas_font strict_exist atlas_font_id path maybe_charset engine=fmap (\atlas_font->engine {atlas_font=atlas_font}) (int_map_applicative_update strict_exist atlas_font_id (update_atlas_font_a path maybe_charset engine) engine.atlas_font)

update_atlas_font_a::ET.Has_call_stack=>DT.Text->Maybe (DHS.HashSet Char)->Engine a->Atlas_font->IO Atlas_font
update_atlas_font_a path maybe_charset engine atlas_font=case maybe_charset of
    Nothing->update_atlas_font_b True path (DIM.keysSet atlas_font.glyph) engine atlas_font
    Just charset->let unicode=DIS.difference (DHS.foldl' (\this_unicode char->DIS.insert (DC.ord char) this_unicode) DIS.empty charset) (DIM.keysSet atlas_font.glyph) in if DIS.null unicode then return atlas_font else update_atlas_font_b False path unicode engine atlas_font

update_atlas_font_b::ET.Has_call_stack=>Bool->DT.Text->DIS.IntSet->Engine a->Atlas_font->IO Atlas_font
update_atlas_font_b exclude path unicode engine atlas_font=with_text path $ \this_path->let size=DIS.size unicode in FMA.allocaArray size $ \ptr_charset->do
    CM.void (int_set_monad_fold (\char_code index->integral_action (\this_index->FS.pokeElemOff ptr_charset this_index (fromIntegral char_code)) index) unicode 0)
    ptr_msdf_output<-MSDFF.msdf_generator this_path (FMU.fromBool exclude) ptr_charset (fromIntegral size) atlas_font.font_size atlas_font.pixel_range
    catch_null ptr_msdf_output
    msdf_output<-FS.peek ptr_msdf_output
    case msdf_output of
        MSDFI.MSDF_Output {msdf_pixel,msdf_width,msdf_height,msdf_descent,msdf_ascent,msdf_glyph,msdf_count}->if msdf_count==0
            then do
                MSDFF.msdf_cleaner ptr_msdf_output
                return atlas_font
            else let new_msdf_width=fromIntegral msdf_width in let new_msdf_height=fromIntegral msdf_height in do
                texture<-from_pixel engine.device engine.picture_transfer_buffer engine.max_picture_size msdf_pixel new_msdf_width new_msdf_height
                let (font_atlas,left,down,_,_)=atlas_insert new_msdf_width new_msdf_height atlas_font.padding atlas_font.font_atlas
                copy_texture engine.device texture atlas_font.texture left down new_msdf_width new_msdf_height
                SDLF.sdl_release_gpu_texture engine.device texture
                new_glyph<-from_charset_c (fromIntegral left) (fromIntegral down) atlas_font.exponent_width atlas_font.exponent_height 0 (fromIntegral msdf_count) msdf_glyph DIM.empty
                MSDFF.msdf_cleaner ptr_msdf_output
                return (atlas_font {font_atlas=font_atlas,glyph=DIM.union atlas_font.glyph new_glyph,descent=msdf_descent,ascent=msdf_ascent})

from_charset::ET.Has_call_stack=>DHMS.HashMap DT.Text (DHS.HashSet Char)->Engine a->IO (Engine a)
from_charset charset engine=DHMS.foldlWithKey' (\action path single_charset->action>>=from_charset_a path single_charset) (return engine) charset

from_charset_a::ET.Has_call_stack=>DT.Text->DHS.HashSet Char->Engine a->IO (Engine a)
from_charset_a path charset engine=let unicode=DHS.foldl' (\this_unicode char->DIS.insert (DC.ord char) this_unicode) DIS.empty charset in case DHMS.lookup path engine.font_map of
    Nothing->from_charset_b False DIM.empty engine.font_id path unicode (engine {font_map=hash_map_insert_strict path engine.font_id engine.font_map,font_id=engine.font_id+1})
    Just font_id->let font=int_map_lookup font_id engine.font in let new_unicode=DIS.difference unicode (DIM.keysSet font.glyph) in if DIS.null new_unicode then return engine else from_charset_b False font.glyph font_id path new_unicode engine

from_charset_b::ET.Has_call_stack=>Bool->DIM.IntMap Glyph->Int->DT.Text->DIS.IntSet->Engine a->IO (Engine a)
from_charset_b exclude glyph font_id path unicode engine=with_text path $ \this_path->let size=DIS.size unicode in FMA.allocaArray size $ \ptr_charset->do
    CM.void (int_set_monad_fold (\char_code index->integral_action (\this_index->FS.pokeElemOff ptr_charset this_index (fromIntegral char_code)) index) unicode 0)
    ptr_msdf_output<-MSDFF.msdf_generator this_path (FMU.fromBool exclude) ptr_charset (fromIntegral size) engine.font_size engine.pixel_range
    catch_null ptr_msdf_output
    msdf_output<-FS.peek ptr_msdf_output
    case msdf_output of
        MSDFI.MSDF_Output {msdf_pixel,msdf_width,msdf_height,msdf_descent,msdf_ascent,msdf_glyph,msdf_count}->if msdf_count==0
            then do
                MSDFF.msdf_cleaner ptr_msdf_output
                return (engine {font=DIM.insert font_id (Font {glyph=glyph,descent=msdf_descent,ascent=msdf_ascent}) engine.font})
            else let new_msdf_width=fromIntegral msdf_width in let new_msdf_height=fromIntegral msdf_height in do
                texture<-from_pixel engine.device engine.picture_transfer_buffer engine.max_picture_size msdf_pixel new_msdf_width new_msdf_height
                let (atlas,left,down,_,_)=atlas_insert new_msdf_width new_msdf_height engine.padding engine.atlas
                copy_texture engine.device texture engine.texture left down new_msdf_width new_msdf_height
                SDLF.sdl_release_gpu_texture engine.device texture
                new_glyph<-from_charset_c (fromIntegral left) (fromIntegral down) engine.exponent_width engine.exponent_height 0 (fromIntegral msdf_count) msdf_glyph DIM.empty
                MSDFF.msdf_cleaner ptr_msdf_output
                return (engine {atlas=atlas,font=DIM.insert font_id (Font {glyph=DIM.union glyph new_glyph,descent=msdf_descent,ascent=msdf_ascent}) engine.font})

from_charset_c::ET.Has_call_stack=>FCT.CFloat->FCT.CFloat->Int->Int->Int->Int->FP.Ptr MSDFI.MSDF_Glyph->DIM.IntMap Glyph->IO (DIM.IntMap Glyph)
from_charset_c x y exponent_width exponent_height index msdf_count msdf_glyph glyph=if msdf_count<=index then return glyph else do
    single_msdf_glyph<-FS.peekElemOff msdf_glyph index
    case single_msdf_glyph of
        MSDFI.MSDF_Glyph {msdf_unicode,msdf_advance,msdf_plane_left,msdf_plane_down,msdf_plane_right,msdf_plane_up,msdf_atlas_left,msdf_atlas_down,msdf_atlas_right,msdf_atlas_up}->from_charset_c x y exponent_width exponent_height (index+1) msdf_count msdf_glyph (DIM.insert (fromIntegral msdf_unicode) (Glyph {advance=msdf_advance,left=msdf_plane_left,down=msdf_plane_down,right=msdf_plane_right,up=msdf_plane_up,min_u=scaleFloat (negate exponent_width) (x+msdf_atlas_left),min_v=scaleFloat (negate exponent_height) (y+msdf_atlas_down),max_u=scaleFloat (negate exponent_width) (x+msdf_atlas_right),max_v=scaleFloat (negate exponent_height) (y+msdf_atlas_up)}) glyph)

scroll_text::ET.Has_call_stack=>Bool->FCT.CFloat->Visual a->Visual a
scroll_text strict_match scroll visual=case visual of
    Text {arrange,half_width,half_height,current_y,min_y,max_y,anchor,hole_index,hole,article,charset,locked}->Text {arrange=arrange,half_width=half_width,half_height=half_height,current_y=max min_y (min (max min_y max_y) (current_y+scroll)),min_y=min_y,max_y=max_y,anchor=anchor,hole_index=hole_index,hole=hole,article=article,charset=charset,locked=locked}
    _->if strict_match then EF.empty_error else visual

scroll_top_text::ET.Has_call_stack=>Bool->Visual a->Visual a
scroll_top_text strict_match visual=case visual of
    Text {arrange,half_width,half_height,min_y,max_y,anchor,hole_index,hole,article,charset,locked}->Text {arrange=arrange,half_width=half_width,half_height=half_height,current_y=min_y,min_y=min_y,max_y=max_y,anchor=anchor,hole_index=hole_index,hole=hole,article=article,charset=charset,locked=locked}
    _->if strict_match then EF.empty_error else visual

scroll_bottom_text::ET.Has_call_stack=>Bool->Visual a->Visual a
scroll_bottom_text strict_match visual=case visual of
    Text {arrange,half_width,half_height,min_y,max_y,anchor,hole_index,hole,article,charset,locked}->Text {arrange=arrange,half_width=half_width,half_height=half_height,current_y=max min_y max_y,min_y=min_y,max_y=max_y,anchor=anchor,hole_index=hole_index,hole=hole,article=article,charset=charset,locked=locked}
    _->if strict_match then EF.empty_error else visual

{-# INLINE do_typesetting_c #-}
{-# INLINE from_maybe_color #-}
{-# INLINE lookup_glyph #-}
{-# INLINE summarize_text #-}
{-# INLINE summarize_text_a #-}
{-# INLINE summarize_text_b #-}
{-# INLINE scroll_text #-}
{-# INLINE scroll_top_text #-}
{-# INLINE scroll_bottom_text #-}