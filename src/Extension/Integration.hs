{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}

module Extension.Integration where

import Extension.Button
import Extension.Common
import Extension.Page
import Extension.Type
import Engine.Helper
import Engine.Type
import qualified Error.Error as EE
import qualified Data.Sequence as DS
import qualified Data.Vector as DV
import qualified Foreign.C.Types as FCT

create_extension_widget_request::(Event a->Engine b a c d e->Maybe Int)->(Engine b a c d e->Engine b a c d e)->Extension_widget_request b a c d e->Widget_request b a c d e
create_extension_widget_request next action extension_widget_request=case extension_widget_request of
    Button {window_id,text_request,inner_thickness,outer_thickness,inner_color,outer_color,inner_hovered_color,outer_hovered_color,inner_pressed_color,outer_pressed_color,inner_hovered_pressed_color,outer_hovered_pressed_color}->case text_request of
        Visual_request {visual_request}->case visual_request of
            Text_request {text_width,text_height}->let half_width=text_width/2 in let half_height=text_height/2 in let inner_width=half_width+inner_thickness in let inner_height=half_height+inner_thickness in let outer_width=inner_width+outer_thickness in let outer_height=inner_height+outer_thickness in Widget_trigger_request {next=next,widget_trigger=button_widget_trigger action,widget_request=Vector_request {index=0,vector_widget_request=DS.singleton (Vector_visual_request {arrange=Arrange {point=Point {x=0,y=0},matrix=identity_matrix,red=1,green=1,blue=1,alpha=1},collect_order=2 DS.<| 1 DS.<| DS.singleton 0,vector_visual_request=DV.fromList [visual_request,create_rectangle_request inner_color inner_width inner_height,create_rectangle_request outer_color outer_width outer_height,create_rectangle_request inner_hovered_color inner_width inner_height,create_rectangle_request outer_hovered_color outer_width outer_height,create_rectangle_request inner_pressed_color inner_width inner_height,create_rectangle_request outer_pressed_color outer_width outer_height,create_rectangle_request inner_hovered_pressed_color inner_width inner_height,create_rectangle_request outer_hovered_pressed_color outer_width outer_height]}) DS.|> Store_request {store=convert False} DS.|> Store_request {store=convert False} DS.|> Store_request {store=convert True} DS.|> Store_request {store=convert window_id}}}
            _->EE.quick_error "create_extension_widget_request" 0
        _->EE.quick_error "create_extension_widget_request" 1
    _->EE.quick_error "create_extension_widget_request" 2

maybe_update_collect_extension_widget::Custom_widget e=>Tag->Maybe (Border FCT.CFloat)->Projection_path->Int->Selector a->Insert_strategy->Engine b c d e f->Engine b c d e f
maybe_update_collect_extension_widget tag=case tag of
    Page_tag->maybe_update_collect_page
    Button_tag->maybe_update_collect_button

maybe_collect_update_extension_widget::Custom_widget e=>Tag->Maybe (Border FCT.CFloat)->Projection_path->Int->Selector a->Insert_strategy->Engine b c d e f->Engine b c d e f
maybe_collect_update_extension_widget tag=case tag of
    Page_tag->maybe_collect_update_page
    Button_tag->maybe_collect_update_button

collect_extension_widget::Custom_widget e=>Tag->Maybe (Border FCT.CFloat)->Projection_path->Int->Selector a->Insert_strategy->Engine b c d e f->Engine b c d e f
collect_extension_widget tag=case tag of
    Page_tag->collect_page
    Button_tag->collect_button