{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Engine.Text where

import Engine.Other
import Engine.Type
import qualified Data.Char as DC
import qualified Data.IntMap as DIM
import qualified Data.Sequence as DS
import qualified Data.Text as DT
import qualified Foreign.C.Types as FCT

do_typesetting::FCT.CFloat->(Int->DS.Seq (DS.Seq Row)->(FCT.CFloat,FCT.CFloat))->DS.Seq (DS.Seq Row)->(DS.Seq (DS.Seq Row),FCT.CFloat)
do_typesetting height calculate_typesetting article=do_typesetting_a (-height) (`calculate_typesetting` article) 1 article DS.empty

do_typesetting_a::FCT.CFloat->(Int->(FCT.CFloat,FCT.CFloat))->Int->DS.Seq (DS.Seq Row)->DS.Seq (DS.Seq Row)->(DS.Seq (DS.Seq Row),FCT.CFloat)
do_typesetting_a y calculate_typesetting row_number article this_article=case article of
    DS.Empty->let (_,height)=calculate_typesetting row_number in (this_article,y+height)
    (paragraph DS.:<| other_paragraph)->let (new_paragraph,new_row_number,new_y)=do_typesetting_b y calculate_typesetting row_number paragraph in do_typesetting_a new_y calculate_typesetting new_row_number other_paragraph (this_article DS.|> new_paragraph)

do_typesetting_b::FCT.CFloat->(Int->(FCT.CFloat,FCT.CFloat))->Int->DS.Seq Row->(DS.Seq Row,Int,FCT.CFloat)
do_typesetting_b y calculate_typesetting row_number paragraph=case paragraph of
    DS.Empty->(DS.empty,row_number,y)
    (row DS.:<| other_row)->let (x,height)=calculate_typesetting row_number in let new_y=y+height in let (final_paragraph,final_row_number,final_y)=do_typesetting_b new_y calculate_typesetting (row_number+1) other_row in case row of
        Blank->(Blank DS.:<| final_paragraph,final_row_number,final_y)
        Row {row_core,width,min_down,max_up,min_descent,max_ascent}->(Row {row_core=row_core,x=x-width/2,y=new_y,width=width,min_down=min_down,max_up=max_up,min_descent=min_descent,max_ascent=max_ascent} DS.:<| final_paragraph,final_row_number,final_y)

for_text::DIM.IntMap Font->DS.Seq (DS.Seq Sentence)->(Int->DS.Seq Row->DS.Seq (DS.Seq Row)->FCT.CFloat)->DS.Seq (DS.Seq Row)
for_text font article calculate_width=for_text_a font article calculate_width 1 DS.empty

for_text_a::DIM.IntMap Font->DS.Seq (DS.Seq Sentence)->(Int->DS.Seq Row->DS.Seq (DS.Seq Row)->FCT.CFloat)->Int->DS.Seq (DS.Seq Row)->DS.Seq (DS.Seq Row)
for_text_a font article calculate_width row_number this_article=case article of
    DS.Empty->this_article
    (paragraph DS.:<| other_paragraph)->if DS.null paragraph then for_text_a font other_paragraph calculate_width row_number (this_article DS.|> DS.singleton Blank) else let (new_article,new_row_number)=for_text_b font paragraph Positive_infinity Negative_infinity Positive_infinity Negative_infinity 0 (calculate_width row_number DS.empty this_article) calculate_width row_number DS.empty DS.empty this_article in for_text_a font other_paragraph calculate_width new_row_number new_article

for_text_b::DIM.IntMap Font->DS.Seq Sentence->Extended FCT.CFloat->Extended FCT.CFloat->Extended FCT.CFloat->Extended FCT.CFloat->FCT.CFloat->FCT.CFloat->(Int->DS.Seq Row->DS.Seq (DS.Seq Row)->FCT.CFloat)->Int->DS.Seq Character->DS.Seq Row->DS.Seq (DS.Seq Row)->(DS.Seq (DS.Seq Row),Int)
for_text_b font paragraph min_down max_up min_descent max_ascent x width calculate_width row_number row_core this_paragraph article=case paragraph of
    DS.Empty->(article DS.|> (this_paragraph DS.|> Row {row_core=row_core,x=0,y=0,width=x,min_down=from_extended min_down,max_up=from_extended max_up,min_descent=from_extended min_descent,max_ascent=from_extended max_ascent}),row_number+1)
    (sentence DS.:<| other_sentence)->case sentence of
        Sentence {sentence_core,font_id}->let new_font=intmap_lookup font_id font in let (new_paragraph,new_row_core,new_row_number,new_width,new_x,new_max_ascent,new_min_descent,new_max_up,new_min_down)=for_text_c font_id new_font.glyph sentence_core min_down max_up min_descent max_ascent new_font.descent new_font.ascent x width (\this_row_number this_this_paragraph->calculate_width this_row_number this_this_paragraph article) row_number row_core this_paragraph in for_text_b font other_sentence new_min_down new_max_up new_min_descent new_max_ascent new_x new_width calculate_width new_row_number new_row_core new_paragraph article

for_text_c::Int->DIM.IntMap Glyph->DS.Seq Phrase->Extended FCT.CFloat->Extended FCT.CFloat->Extended FCT.CFloat->Extended FCT.CFloat->FCT.CFloat->FCT.CFloat->FCT.CFloat->FCT.CFloat->(Int->DS.Seq Row->FCT.CFloat)->Int->DS.Seq Character->DS.Seq Row->(DS.Seq Row,DS.Seq Character,Int,FCT.CFloat,FCT.CFloat,Extended FCT.CFloat,Extended FCT.CFloat,Extended FCT.CFloat,Extended FCT.CFloat)
for_text_c font_id glyph sentence_core min_down max_up min_descent max_ascent descent ascent x width calculate_width row_number row_core paragraph=case sentence_core of
    DS.Empty->(paragraph,row_core,row_number,width,x,max_ascent,min_descent,max_up,min_down)
    (phrase DS.:<| other_phrase)->case phrase of
        Phrase {phrase_core,size,red,green,blue,alpha}->let (new_paragraph,new_row_core,new_row_number,new_width,new_x,new_max_ascent,new_min_descent,new_max_up,new_min_down)=let new_descent=descent*size in let new_ascent=ascent*size in for_text_d font_id glyph phrase_core min_down max_up (min (to_extended new_descent) min_descent) (max (to_extended new_ascent) max_ascent) new_descent new_ascent size red green blue alpha x width calculate_width row_number row_core paragraph in for_text_c font_id glyph other_phrase new_min_down new_max_up new_min_descent new_max_ascent descent ascent new_x new_width calculate_width new_row_number new_row_core new_paragraph

for_text_d::Int->DIM.IntMap Glyph->DT.Text->Extended FCT.CFloat->Extended FCT.CFloat->Extended FCT.CFloat->Extended FCT.CFloat->FCT.CFloat->FCT.CFloat->FCT.CFloat->FCT.CFloat->FCT.CFloat->FCT.CFloat->FCT.CFloat->FCT.CFloat->FCT.CFloat->(Int->DS.Seq Row->FCT.CFloat)->Int->DS.Seq Character->DS.Seq Row->(DS.Seq Row,DS.Seq Character,Int,FCT.CFloat,FCT.CFloat,Extended FCT.CFloat,Extended FCT.CFloat,Extended FCT.CFloat,Extended FCT.CFloat)
for_text_d font_id glyph text min_down max_up min_descent max_ascent descent ascent size red green blue alpha x width calculate_width row_number row_core paragraph=case text of
    DT.Empty->(paragraph,row_core,row_number,width,x,max_ascent,min_descent,max_up,min_down)
    (char DT.:< other_char)->let unicode=DC.ord char in case intmap_lookup unicode glyph of
        Glyph {advance,left,down,right,up,min_u,min_v,max_u,max_v}->let new_advance=advance*size in let new_down=down*size in let new_right=right*size in let new_up=up*size in let new_new_down=to_extended new_down in let new_new_up=to_extended new_up in if width<x+new_right then let (new_paragraph,new_row_number,new_width)=for_text_e new_right calculate_width (row_number+1) (paragraph DS.|> Row {row_core=row_core,x=0,y=0,width=x,min_down=from_extended min_down,max_up=from_extended max_up,min_descent=from_extended min_descent,max_ascent=from_extended max_ascent}) in for_text_d font_id glyph other_char new_new_down new_new_up (to_extended descent) (to_extended ascent) descent ascent size red green blue alpha new_advance new_width calculate_width new_row_number (DS.singleton (Character {unicode=unicode,font_id=font_id,size=size,left=left*size,down=new_down,right=new_right,up=new_up,min_u=min_u,min_v=min_v,max_u=max_u,max_v=max_v,red=red,green=green,blue=blue,alpha=alpha})) new_paragraph else for_text_d font_id glyph other_char (min new_new_down min_down) (max new_new_up max_up) min_descent max_ascent descent ascent size red green blue alpha (x+new_advance) width calculate_width row_number (row_core DS.|> Character {unicode=unicode,font_id=font_id,size=size,left=x+left*size,down=new_down,right=x+new_right,up=new_up,min_u=min_u,min_v=min_v,max_u=max_u,max_v=max_v,red=red,green=green,blue=blue,alpha=alpha}) paragraph

for_text_e::FCT.CFloat->(Int->DS.Seq Row->FCT.CFloat)->Int->DS.Seq Row->(DS.Seq Row,Int,FCT.CFloat)
for_text_e right calculate_width row_number paragraph=let width=calculate_width row_number paragraph in if right<width then (paragraph,row_number,width) else for_text_e right calculate_width (row_number+1) (paragraph DS.|> Blank)