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
import qualified Error.Error as EE
import qualified Data.Char as DC
import qualified Data.Foldable as DF
import qualified Data.IntMap as DIM
import qualified Data.IntSet as DIS
import qualified Data.Map as DM
import qualified Data.Sequence as DSeq
import qualified Data.Set as DSet
import qualified Data.Text as DT
import qualified Data.Word as DW
import qualified Foreign.C.Types as FCT
import qualified Foreign.Marshal.Array as FMA
import qualified Foreign.Ptr as FP
import qualified Foreign.Storable as FS

do_typesetting::FCT.CFloat->(DSeq.Seq (DSeq.Seq Row)->Int->(FCT.CFloat,FCT.CFloat,FCT.CFloat))->DSeq.Seq (DSeq.Seq Row)->(DSeq.Seq (DSeq.Seq Row),FCT.CFloat)
do_typesetting height calculate_typesetting article=do_typesetting_a (-height) (calculate_typesetting article) 0 article DSeq.empty

do_typesetting_a::FCT.CFloat->(Int->(FCT.CFloat,FCT.CFloat,FCT.CFloat))->Int->DSeq.Seq (DSeq.Seq Row)->DSeq.Seq (DSeq.Seq Row)->(DSeq.Seq (DSeq.Seq Row),FCT.CFloat)
do_typesetting_a y calculate_typesetting row_number article this_article=case article of
    DSeq.Empty->(this_article,y)
    (paragraph DSeq.:<| other_paragraph)->let (new_paragraph,new_row_number,new_y)=do_typesetting_b y calculate_typesetting row_number paragraph in do_typesetting_a new_y calculate_typesetting new_row_number other_paragraph (this_article DSeq.|> new_paragraph)

do_typesetting_b::FCT.CFloat->(Int->(FCT.CFloat,FCT.CFloat,FCT.CFloat))->Int->DSeq.Seq Row->(DSeq.Seq Row,Int,FCT.CFloat)
do_typesetting_b y calculate_typesetting row_number paragraph=case paragraph of
    DSeq.Empty->(DSeq.empty,row_number,y)
    (row DSeq.:<| other_row)->let (lower,upper,x)=calculate_typesetting row_number in let new_y=y+upper in let (final_paragraph,final_row_number,final_y)=do_typesetting_b (new_y+lower) calculate_typesetting (row_number+1) other_row in case row of
        Blank->(Blank DSeq.:<| final_paragraph,final_row_number,final_y)
        Row {row_core,width,min_down,max_up,min_descent,max_ascent}->(Row {row_core=row_core,x=x-width/2,y=new_y,width=width,min_down=min_down,max_up=max_up,min_descent=min_descent,max_ascent=max_ascent} DSeq.:<| final_paragraph,final_row_number,final_y)

for_text::DIM.IntMap Font->DM.Map String Int->DSeq.Seq (DSeq.Seq Sentence)->(DSeq.Seq Row->DSeq.Seq (DSeq.Seq Row)->Int->FCT.CFloat)->DSeq.Seq (DSeq.Seq Row)
for_text font font_map article calculate_width=for_text_a font font_map article calculate_width 0 DSeq.empty

for_text_a::DIM.IntMap Font->DM.Map String Int->DSeq.Seq (DSeq.Seq Sentence)->(DSeq.Seq Row->DSeq.Seq (DSeq.Seq Row)->Int->FCT.CFloat)->Int->DSeq.Seq (DSeq.Seq Row)->DSeq.Seq (DSeq.Seq Row)
for_text_a font font_map article calculate_width row_number this_article=case article of
    DSeq.Empty->this_article
    (paragraph DSeq.:<| other_paragraph)->if DSeq.null paragraph then for_text_a font font_map other_paragraph calculate_width row_number (this_article DSeq.|> DSeq.singleton Blank) else let (new_article,new_row_number)=for_text_b font font_map paragraph Positive_infinity Negative_infinity Positive_infinity Negative_infinity 0 (calculate_width DSeq.empty this_article row_number) calculate_width row_number DSeq.empty DSeq.empty this_article in for_text_a font font_map other_paragraph calculate_width new_row_number new_article

for_text_b::DIM.IntMap Font->DM.Map String Int->DSeq.Seq Sentence->Extended->Extended->Extended->Extended->FCT.CFloat->FCT.CFloat->(DSeq.Seq Row->DSeq.Seq (DSeq.Seq Row)->Int->FCT.CFloat)->Int->DSeq.Seq Character->DSeq.Seq Row->DSeq.Seq (DSeq.Seq Row)->(DSeq.Seq (DSeq.Seq Row),Int)
for_text_b font font_map paragraph min_down max_up min_descent max_ascent x width calculate_width row_number row_core this_paragraph article=case paragraph of
    DSeq.Empty->(article DSeq.|> (this_paragraph DSeq.|> Row {row_core=row_core,x=0,y=0,width=x,min_down=from_extended min_down,max_up=from_extended max_up,min_descent=from_extended min_descent,max_ascent=from_extended max_ascent}),row_number+1)
    (sentence DSeq.:<| other_sentence)->case sentence of
        Sentence {sentence_core,path}->let font_id=map_lookup path font_map in let single_font=intmap_lookup font_id font in let (new_paragraph,new_row_core,new_row_number,new_width,new_x,new_max_ascent,new_min_descent,new_max_up,new_min_down)=for_text_c font_id single_font.glyph sentence_core min_down max_up min_descent max_ascent single_font.descent single_font.ascent x width (`calculate_width` article) row_number row_core this_paragraph in for_text_b font font_map other_sentence new_min_down new_max_up new_min_descent new_max_ascent new_x new_width calculate_width new_row_number new_row_core new_paragraph article

for_text_c::Int->DIM.IntMap Glyph->DSeq.Seq Phrase->Extended->Extended->Extended->Extended->FCT.CFloat->FCT.CFloat->FCT.CFloat->FCT.CFloat->(DSeq.Seq Row->Int->FCT.CFloat)->Int->DSeq.Seq Character->DSeq.Seq Row->(DSeq.Seq Row,DSeq.Seq Character,Int,FCT.CFloat,FCT.CFloat,Extended,Extended,Extended,Extended)
for_text_c font_id glyph sentence_core min_down max_up min_descent max_ascent descent ascent x width calculate_width row_number row_core paragraph=case sentence_core of
    DSeq.Empty->(paragraph,row_core,row_number,width,x,max_ascent,min_descent,max_up,min_down)
    (phrase DSeq.:<| other_phrase)->case phrase of
        Phrase {phrase_core,font_size,color}->let (new_paragraph,new_row_core,new_row_number,new_width,new_x,new_max_ascent,new_min_descent,new_max_up,new_min_down)=let new_descent=descent*font_size in let new_ascent=ascent*font_size in for_text_d font_id glyph phrase_core min_down max_up (min (to_extended new_descent) min_descent) (max (to_extended new_ascent) max_ascent) new_descent new_ascent font_size x width color calculate_width row_number row_core paragraph in for_text_c font_id glyph other_phrase new_min_down new_max_up new_min_descent new_max_ascent descent ascent new_x new_width calculate_width new_row_number new_row_core new_paragraph

for_text_d::Int->DIM.IntMap Glyph->DT.Text->Extended->Extended->Extended->Extended->FCT.CFloat->FCT.CFloat->FCT.CFloat->FCT.CFloat->FCT.CFloat->Color->(DSeq.Seq Row->Int->FCT.CFloat)->Int->DSeq.Seq Character->DSeq.Seq Row->(DSeq.Seq Row,DSeq.Seq Character,Int,FCT.CFloat,FCT.CFloat,Extended,Extended,Extended,Extended)
for_text_d font_id glyph text min_down max_up min_descent max_ascent descent ascent font_size x width color calculate_width row_number row_core paragraph=case text of
    DT.Empty->(paragraph,row_core,row_number,width,x,max_ascent,min_descent,max_up,min_down)
    (char DT.:< other_char)->let unicode=DC.ord char in case intmap_lookup unicode glyph of
        Glyph {advance,left,down,right,up,min_u,min_v,max_u,max_v}->let new_advance=advance*font_size in let new_left=left*font_size in let new_down=down*font_size in let new_right=right*font_size in let new_up=up*font_size in let extended_down=to_extended new_down in let extended_up=to_extended new_up in if width<x+new_right then let (new_paragraph,new_row_number,new_width)=for_text_e new_right calculate_width (row_number+1) (paragraph DSeq.|> Row {row_core=row_core,x=0,y=0,width=x,min_down=from_extended min_down,max_up=from_extended max_up,min_descent=from_extended min_descent,max_ascent=from_extended max_ascent}) in for_text_d font_id glyph other_char extended_down extended_up (to_extended descent) (to_extended ascent) descent ascent font_size new_advance new_width color calculate_width new_row_number (DSeq.singleton (Character {unicode=unicode,font_id=font_id,font_size=font_size,left=new_left,down=new_down,right=new_right,up=new_up,min_u=min_u,min_v=min_v,max_u=max_u,max_v=max_v,color=color})) new_paragraph else for_text_d font_id glyph other_char (min extended_down min_down) (max extended_up max_up) min_descent max_ascent descent ascent font_size (x+new_advance) width color calculate_width row_number (row_core DSeq.|> Character {unicode=unicode,font_id=font_id,font_size=font_size,left=x+new_left,down=new_down,right=x+new_right,up=new_up,min_u=min_u,min_v=min_v,max_u=max_u,max_v=max_v,color=color}) paragraph

for_text_e::FCT.CFloat->(DSeq.Seq Row->Int->FCT.CFloat)->Int->DSeq.Seq Row->(DSeq.Seq Row,Int,FCT.CFloat)
for_text_e right calculate_width row_number paragraph=let width=calculate_width paragraph row_number in if right<width then (paragraph,row_number,width) else for_text_e right calculate_width (row_number+1) (paragraph DSeq.|> Blank)

to_charset::DSeq.Seq (DSeq.Seq Sentence)->DM.Map String (DSet.Set Char)
to_charset=DF.foldl' (DF.foldl' (flip to_charset_a)) DM.empty

to_charset_a::Sentence->DM.Map String (DSet.Set Char)->DM.Map String (DSet.Set Char)
to_charset_a sentence charset=case sentence of
    Sentence {sentence_core,path}->DM.insert path (DF.foldl' (\this_charset phrase->DT.foldl' (flip DSet.insert) this_charset phrase.phrase_core) (DM.findWithDefault DSet.empty path charset) sentence_core) charset

update_font::DM.Map String (DSet.Set Char)->Engine a b c d e->IO (Engine a b c d e)
update_font charset engine=DM.foldlWithKey' (\action path char->action>>=update_font_a path char) (return engine) charset

update_font_a::String->DSet.Set Char->Engine a b c d e->IO (Engine a b c d e)
update_font_a path char engine=let charset=DSet.foldl' (\this_charset single_char->DIS.insert (DC.ord single_char) this_charset) DIS.empty char in case DM.lookup path engine.font_map of
    Nothing->update_font_b engine.font_id path charset (engine {font_map=map_insert path engine.font_id engine.font_map,font_id=engine.font_id+1})
    Just font_id->case DIM.lookup font_id engine.font of
        Nothing->update_font_b font_id path charset engine
        Just font->update_font_b font_id path (DIS.difference charset (DIM.keysSet font.glyph)) engine

update_font_b::Int->String->DIS.IntSet->Engine a b c d e->IO (Engine a b c d e)
update_font_b font_id path charset engine=if DIS.null charset then return engine else with_string path $ \this_path->let size=DIS.size charset in FMA.allocaArray size $ \ptr_charset->do
    DIS.foldr (flip . update_font_c) (const (return ())) charset ptr_charset
    ptr_msdf_output<-MSDFF.msdf_generator this_path ptr_charset (fromIntegral size) engine.font_size engine.pixel_range
    catch_null ptr_msdf_output
    msdf_output<-FS.peek ptr_msdf_output
    case msdf_output of
        MSDFT.MSDF_Output {msdf_pixel,msdf_width,msdf_height,msdf_descent,msdf_ascent,msdf_glyph,msdf_count}->let new_msdf_width=fromIntegral msdf_width in let new_msdf_height=fromIntegral msdf_height in do
            texture<-from_pixel engine.device engine.picture_transfer_buffer engine.max_picture_size msdf_pixel new_msdf_width new_msdf_height
            let (atlas,left,down,_,_)=atlas_insert new_msdf_width new_msdf_height engine.padding engine.atlas
            copy_texture engine.device texture engine.texture left down new_msdf_width new_msdf_height
            SDLF.sdl_release_gpu_texture engine.device texture
            glyph<-update_font_d (fromIntegral left) (fromIntegral down) engine.exponent_width engine.exponent_height 0 (fromIntegral msdf_count) msdf_glyph DIM.empty
            MSDFF.msdf_cleaner ptr_msdf_output
            case DIM.lookup font_id engine.font of
                Nothing->return (engine {atlas=atlas,font=DIM.insert font_id (Font {descent=msdf_descent,ascent=msdf_ascent,glyph=glyph}) engine.font})
                Just font->return (engine {atlas=atlas,font=DIM.insert font_id (Font {descent=msdf_descent,ascent=msdf_ascent,glyph=DIM.union glyph font.glyph}) engine.font})

update_font_c::Int->FP.Ptr DW.Word32->(FP.Ptr DW.Word32->IO ())->IO ()
update_font_c int ptr action=do
    FS.poke ptr (fromIntegral int)
    action (FP.plusPtr ptr 4)

update_font_d::FCT.CFloat->FCT.CFloat->Int->Int->Int->Int->FP.Ptr MSDFT.MSDF_Glyph->DIM.IntMap Glyph->IO (DIM.IntMap Glyph)
update_font_d x y exponent_width exponent_height index msdf_count msdf_glyph glyph=if msdf_count<=index then return glyph else do
    single_msdf_glyph<-FS.peekElemOff msdf_glyph index
    case single_msdf_glyph of
        MSDFT.MSDF_Glyph {msdf_unicode,msdf_advance,msdf_plane_left,msdf_plane_down,msdf_plane_right,msdf_plane_up,msdf_atlas_left,msdf_atlas_down,msdf_atlas_right,msdf_atlas_up}->update_font_d x y exponent_width exponent_height (index+1) msdf_count msdf_glyph (DIM.insert (fromIntegral msdf_unicode) (Glyph {advance=msdf_advance,left=msdf_plane_left,down=msdf_plane_down,right=msdf_plane_right,up=msdf_plane_up,min_u=scaleFloat (-exponent_width) (x+msdf_atlas_left),min_v=scaleFloat (-exponent_height) (y+msdf_atlas_down),max_u=scaleFloat (-exponent_width) (x+msdf_atlas_right),max_v=scaleFloat (-exponent_height) (y+msdf_atlas_up)}) glyph)

scroll_text::FCT.CFloat->Visual->Visual
scroll_text scroll visual=case visual of
    Text {arrange,half_width,half_height,current_y,min_y,max_y,article,charset,locked}->Text {arrange=arrange,half_width=half_width,half_height=half_height,current_y=max min_y (min (max min_y max_y) (current_y+scroll)),min_y=min_y,max_y=max_y,article=article,charset=charset,locked=locked}
    _->EE.quick_error "scroll_text" 0

scroll_top_text::Visual->Visual
scroll_top_text visual=case visual of
    Text {arrange,half_width,half_height,min_y,max_y,article,charset,locked}->Text {arrange=arrange,half_width=half_width,half_height=half_height,current_y=min_y,min_y=min_y,max_y=max_y,article=article,charset=charset,locked=locked}
    _->EE.quick_error "scroll_top_text" 0

scroll_bottom_text::Visual->Visual
scroll_bottom_text visual=case visual of
    Text {arrange,half_width,half_height,min_y,max_y,article,charset,locked}->Text {arrange=arrange,half_width=half_width,half_height=half_height,current_y=max min_y max_y,min_y=min_y,max_y=max_y,article=article,charset=charset,locked=locked}
    _->EE.quick_error "scroll_bottom_text" 0

{-# INLINE do_typesetting #-}
{-# INLINE for_text #-}
{-# INLINE to_charset #-}
{-# INLINE to_charset_a #-}
{-# INLINE update_font_c #-}
{-# INLINE scroll_text #-}
{-# INLINE scroll_top_text #-}
{-# INLINE scroll_bottom_text #-}