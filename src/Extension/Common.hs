{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}

module Extension.Common where

import Extension.Type
import Engine.Helper
import Engine.Type
import qualified Foreign.C.Types as FCT

create_rectangle_request::Color->FCT.CFloat->FCT.CFloat->Visual_request
create_rectangle_request color half_width half_height=case color of
    Color {red,green,blue,alpha}->Rectangle_request {arrange=Arrange {point=origin,matrix=identity_matrix,red=red,green=green,blue=blue,alpha=alpha},rectangle_width=2*half_width,rectangle_height=2*half_height}

{-# INLINE create_rectangle_request #-}