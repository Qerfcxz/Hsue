{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE StrictData #-}

module Extension.Type where

import Engine.Type
import qualified Foreign.C.Types as FCT

data Color=Color {red::FCT.CFloat,green::FCT.CFloat,blue::FCT.CFloat,alpha::FCT.CFloat}

data Extension_widget_request a b c d e=Page {window_id::Int,text_request::Widget_request a b c d e,inner_thickness::FCT.CFloat,outer_thickness::FCT.CFloat,inner_color::Color,outer_color::Color,inner_hovered_color::Color,outer_hovered_color::Color,inner_selected_color::Color,outer_selected_color::Color,inner_hovered_selected_color::Color,outer_hovered_selected_color::Color}|Button {window_id::Int,text_request::Widget_request a b c d e,inner_thickness::FCT.CFloat,outer_thickness::FCT.CFloat,inner_color::Color,outer_color::Color,inner_hovered_color::Color,outer_hovered_color::Color,inner_pressed_color::Color,outer_pressed_color::Color,inner_hovered_pressed_color::Color,outer_hovered_pressed_color::Color}

data Tag=Page_tag|Button_tag