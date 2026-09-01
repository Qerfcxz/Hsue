{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Engine.Editor.Underlying where

import Engine.Container
import Engine.Type
import qualified Error.Type as ET
import qualified Data.IntMap as DIM
import qualified Data.Vector.Storable as DVS
import qualified Foreign.C.Types as FCT

get_editor_typesetting::ET.Has_call_stack=>Int->Int->DVS.Vector Typesetting->DIM.IntMap Typesetting->Typesetting
get_editor_typesetting index max_typesetting_size typesetting appended_typesetting=if index<max_typesetting_size then typesetting DVS.! index else int_map_lookup index appended_typesetting

get_editor_index_lower::ET.Has_call_stack=>FCT.CFloat->DIM.IntMap Typesetting->DVS.Vector Typesetting->Int->Int->Int->Int
get_editor_index_lower this_y appended_typesetting typesetting max_typesetting_size min_index max_index=if min_index<=max_index
    then let middle_index=div (min_index+max_index) 2 in case get_editor_typesetting middle_index max_typesetting_size typesetting appended_typesetting of
        Typesetting {y,lower}->if y+lower<this_y then get_editor_index_lower this_y appended_typesetting typesetting max_typesetting_size (middle_index+1) max_index else get_editor_index_lower this_y appended_typesetting typesetting max_typesetting_size min_index (middle_index-1)
    else min_index

get_editor_index_upper::ET.Has_call_stack=>FCT.CFloat->DIM.IntMap Typesetting->DVS.Vector Typesetting->Int->Int->Int->Int
get_editor_index_upper this_y appended_typesetting typesetting max_typesetting_size min_index max_index=if min_index<=max_index
    then let middle_index=div (min_index+max_index) 2 in case get_editor_typesetting middle_index max_typesetting_size typesetting appended_typesetting of
        Typesetting {y,upper}->if y-upper<=this_y then get_editor_index_lower this_y appended_typesetting typesetting max_typesetting_size (middle_index+1) max_index else get_editor_index_lower this_y appended_typesetting typesetting max_typesetting_size min_index (middle_index-1)
    else max_index

{-# INLINE get_editor_typesetting #-}
{-# INLINE get_editor_index_lower #-}
{-# INLINE get_editor_index_upper #-}