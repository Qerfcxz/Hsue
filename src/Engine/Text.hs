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

do_typesetting::(Int->DS.Seq (DS.Seq Row)->(FCT.CFloat,FCT.CFloat))->DS.Seq (DS.Seq Row)->DS.Seq (DS.Seq Row)
do_typesetting calculate_typesetting seq_seq_row=do_typesetting_a 0 (`calculate_typesetting` seq_seq_row) 1 seq_seq_row

do_typesetting_a::FCT.CFloat->(Int->(FCT.CFloat,FCT.CFloat))->Int->DS.Seq (DS.Seq Row)->DS.Seq (DS.Seq Row)
do_typesetting_a y calculate_typesetting row_number seq_seq_row=case seq_seq_row of
    DS.Empty->DS.empty
    (seq_row DS.:<| other_seq_row)->let (next_y,new_row_number,new_seq_row)=do_typesetting_b y calculate_typesetting row_number seq_row in new_seq_row DS.<| do_typesetting_a next_y calculate_typesetting new_row_number other_seq_row

do_typesetting_b::FCT.CFloat->(Int->(FCT.CFloat,FCT.CFloat))->Int->DS.Seq Row->(FCT.CFloat,Int,DS.Seq Row)
do_typesetting_b y calculate_typesetting row_number seq_row=case seq_row of
    DS.Empty->(y,row_number,DS.empty)
    (row DS.:<| other_row)->let (x,new_y)=calculate_typesetting row_number in let new_new_y=y+new_y in let (final_y,final_row_number,final_seq_row)=do_typesetting_b new_new_y calculate_typesetting (row_number+1) other_row in case row of
        Blank->(final_y,final_row_number,Blank DS.:<| final_seq_row)
        Row {seq_character,width,min_down,max_up,min_descent,max_ascent}->(final_y,final_row_number,row {seq_character=seq_character,x=x,y=new_new_y,width=width,min_down=min_down,max_up=max_up,min_descent=min_descent,max_ascent=max_ascent} DS.:<| final_seq_row)

for_text::DIM.IntMap Font->DS.Seq (DS.Seq Sentence)->(Int->DS.Seq Row->DS.Seq (DS.Seq Row)->FCT.CFloat)->DS.Seq (DS.Seq Row)
for_text font seq_seq_sentence calculate_width=for_text_a font seq_seq_sentence calculate_width 1 DS.empty

for_text_a::DIM.IntMap Font->DS.Seq (DS.Seq Sentence)->(Int->DS.Seq Row->DS.Seq (DS.Seq Row)->FCT.CFloat)->Int->DS.Seq (DS.Seq Row)->DS.Seq (DS.Seq Row)
for_text_a font seq_seq_sentence calculate_width row_number seq_seq_row=case seq_seq_sentence of
    DS.Empty->seq_seq_row
    (seq_sentence DS.:<| other_seq_sentence)->if DS.null seq_sentence then for_text_a font other_seq_sentence calculate_width row_number (seq_seq_row DS.|> DS.singleton Blank) else let (new_seq_seq_row,new_row_number)=for_text_b font seq_sentence Positive_infinity Negative_infinity Positive_infinity Negative_infinity 0 (calculate_width row_number DS.empty seq_seq_row) calculate_width row_number DS.empty DS.empty seq_seq_row in for_text_a font other_seq_sentence calculate_width new_row_number new_seq_seq_row

for_text_b::DIM.IntMap Font->DS.Seq Sentence->Extended FCT.CFloat->Extended FCT.CFloat->Extended FCT.CFloat->Extended FCT.CFloat->FCT.CFloat->FCT.CFloat->(Int->DS.Seq Row->DS.Seq (DS.Seq Row)->FCT.CFloat)->Int->DS.Seq Character->DS.Seq Row->DS.Seq (DS.Seq Row)->(DS.Seq (DS.Seq Row),Int)
for_text_b font seq_sentence min_down max_up min_descent max_ascent x width calculate_width row_number seq_character seq_row seq_seq_row=case seq_sentence of
    DS.Empty->(seq_seq_row DS.|> (seq_row DS.|> Row {seq_character=seq_character,x=0,y=0,width=x,min_down=from_extended min_down,max_up=from_extended max_up,min_descent=from_extended min_descent,max_ascent=from_extended max_ascent}),row_number+1)
    (sentence DS.:<| other_sentence)->case sentence of
        Sentence {seq_phrase,font_id}->let new_font=intmap_lookup font_id font in let (new_min_down,new_max_up,new_min_descent,new_max_ascent,new_x,new_width,new_row_number,new_seq_character,new_seq_row)=for_text_c font_id new_font.glyph seq_phrase min_down max_up min_descent max_ascent new_font.descent new_font.ascent x width (\this_row_number this_seq_row->calculate_width this_row_number this_seq_row seq_seq_row) row_number seq_character seq_row in for_text_b font other_sentence new_min_down new_max_up new_min_descent new_max_ascent new_x new_width calculate_width new_row_number new_seq_character new_seq_row seq_seq_row

for_text_c::Int->DIM.IntMap Glyph->DS.Seq Phrase->Extended FCT.CFloat->Extended FCT.CFloat->Extended FCT.CFloat->Extended FCT.CFloat->FCT.CFloat->FCT.CFloat->FCT.CFloat->FCT.CFloat->(Int->DS.Seq Row->FCT.CFloat)->Int->DS.Seq Character->DS.Seq Row->(Extended FCT.CFloat,Extended FCT.CFloat,Extended FCT.CFloat,Extended FCT.CFloat,FCT.CFloat,FCT.CFloat,Int,DS.Seq Character,DS.Seq Row)
for_text_c font_id glyph seq_phrase min_down max_up min_descent max_ascent descent ascent x width calculate_width row_number seq_character seq_row=case seq_phrase of
    DS.Empty->(min_down,max_up,min_descent,max_ascent,x,width,row_number,seq_character,seq_row)
    (phrase DS.:<| other_phrase)->case phrase of
        Phrase {text,size,red,green,blue,alpha}->let (new_min_down,new_max_up,new_min_descent,new_max_ascent,new_x,new_width,new_row_number,new_seq_character,new_seq_row)=let new_descent=descent*size in let new_ascent=ascent*size in for_text_d font_id glyph text min_down max_up (min (to_extended new_descent) min_descent) (max (to_extended new_ascent) max_ascent) new_descent new_ascent size red green blue alpha x width calculate_width row_number seq_character seq_row in for_text_c font_id glyph other_phrase new_min_down new_max_up new_min_descent new_max_ascent descent ascent new_x new_width calculate_width new_row_number new_seq_character new_seq_row

for_text_d::Int->DIM.IntMap Glyph->DT.Text->Extended FCT.CFloat->Extended FCT.CFloat->Extended FCT.CFloat->Extended FCT.CFloat->FCT.CFloat->FCT.CFloat->FCT.CFloat->FCT.CFloat->FCT.CFloat->FCT.CFloat->FCT.CFloat->FCT.CFloat->FCT.CFloat->(Int->DS.Seq Row->FCT.CFloat)->Int->DS.Seq Character->DS.Seq Row->(Extended FCT.CFloat,Extended FCT.CFloat,Extended FCT.CFloat,Extended FCT.CFloat,FCT.CFloat,FCT.CFloat,Int,DS.Seq Character,DS.Seq Row)
for_text_d font_id glyph text min_down max_up min_descent max_ascent descent ascent size red green blue alpha x width calculate_width row_number seq_character seq_row=case text of
    DT.Empty->(min_down,max_up,min_descent,max_ascent,x,width,row_number,seq_character,seq_row)
    (char DT.:< other_char)->let unicode=DC.ord char in case intmap_lookup unicode glyph of
        Glyph {advance,left,down,right,up,min_u,min_v,max_u,max_v}->let new_advance=advance*size in let new_down=down*size in let new_right=right*size in let new_up=up*size in let new_new_down=to_extended new_down in let new_new_up=to_extended new_up in if width<x+new_right then let (new_width,new_row_number,new_seq_row)=for_text_e new_right calculate_width (row_number+1) (seq_row DS.|> Row {seq_character=seq_character,x=0,y=0,width=x,min_down=from_extended min_down,max_up=from_extended max_up,min_descent=from_extended min_descent,max_ascent=from_extended max_ascent}) in for_text_d font_id glyph other_char new_new_down new_new_up (to_extended descent) (to_extended ascent) descent ascent size red green blue alpha new_advance new_width calculate_width new_row_number (DS.singleton (Character {unicode=unicode,size=size,font_id=font_id,left=left*size,down=new_down,right=new_right,up=new_up,min_u=min_u,min_v=min_v,max_u=max_u,max_v=max_v,red=red,green=green,blue=blue,alpha=alpha})) new_seq_row else for_text_d font_id glyph other_char (min new_new_down min_down) (max new_new_up max_up) min_descent max_ascent descent ascent size red green blue alpha (x+new_advance) width calculate_width row_number (seq_character DS.|> Character {unicode=unicode,size=size,font_id=font_id,left=x+left*size,down=new_down,right=x+new_right,up=new_up,min_u=min_u,min_v=min_v,max_u=max_u,max_v=max_v,red=red,green=green,blue=blue,alpha=alpha}) seq_row

for_text_e::FCT.CFloat->(Int->DS.Seq Row->FCT.CFloat)->Int->DS.Seq Row->(FCT.CFloat,Int,DS.Seq Row)
for_text_e right calculate_width row_number seq_row=let width=calculate_width row_number seq_row in if right<width then (width,row_number,seq_row) else for_text_e right calculate_width (row_number+1) (seq_row DS.|> Blank)