{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Extension.Page where

import Extension.Common
import Extension.Type
import Engine.Collector
import Engine.Container
import Engine.Helper
import Engine.Operation
import Engine.Projection
import Engine.Selector
import Engine.Type
import Engine.Underlying
import qualified Error.Error as EE
import qualified Data.Functor.Compose as DFC
import qualified Data.Sequence as DS
import qualified Data.Vector as DV
import qualified Foreign.C.Types as FCT

update_text::(Visual->Visual)->Visual->(Visual,Bool)
update_text update visual=case visual of
    Text {current_y=first_y}->let new_visual=update visual in case new_visual of
        Text {current_y=second_y}->(new_visual,first_y/=second_y)
        _->EE.quick_error "update_text" 0
    _->EE.quick_error "update_text" 1

create_page_request::(Event a->Engine b a c d e->Maybe Int)->(Event a->Engine b a c d e->Widget b a c d e->(Widget b a c d e,Engine b a c d e->Engine b a c d e))->Extension_widget_request b a c d e->Widget_request b a c d e
create_page_request next widget_trigger page_request=case page_request of
    Page {window_id,text_request,inner_thickness,outer_thickness,inner_color,outer_color,inner_hovered_color,outer_hovered_color,inner_selected_color,outer_selected_color,inner_hovered_selected_color,outer_hovered_selected_color}->case text_request of
        Visual_request {visual_request}->case visual_request of
            Text_request {text_width,text_height}->let half_width=text_width/2 in let half_height=text_height/2 in let inner_width=half_width+inner_thickness in let inner_height=half_height+inner_thickness in let outer_width=inner_width+outer_thickness in let outer_height=inner_height+outer_thickness in Widget_trigger_request {next=next,widget_trigger=widget_trigger,widget_request=Vector_request {index=0,vector_widget_request=DS.singleton (Vector_visual_request {arrange=Arrange {point=Point {x=0,y=0},matrix=identity_matrix,red=1,green=1,blue=1,alpha=1},collect_order=6 DS.<| 5 DS.<| DS.singleton 0,vector_visual_request=DV.fromList [visual_request,create_rectangle_request inner_color inner_width inner_height,create_rectangle_request outer_color outer_width outer_height,create_rectangle_request inner_hovered_color inner_width inner_height,create_rectangle_request outer_hovered_color outer_width outer_height,create_rectangle_request inner_selected_color inner_width inner_height,create_rectangle_request outer_selected_color outer_width outer_height,create_rectangle_request inner_hovered_selected_color inner_width inner_height,create_rectangle_request outer_hovered_selected_color outer_width outer_height]}) DS.|> Store_request {store=convert False} DS.|> Store_request {store=convert True} DS.|> Store_request {store=convert True} DS.|> Store_request {store=convert window_id}}}
            _->EE.quick_error "create_page_request" 0
        _->EE.quick_error "create_page_request" 1
    _->EE.quick_error "create_page_request" 2

above_page::FCT.CFloat->FCT.CFloat->Widget a b c d e->Maybe (Widget a b c d e)
above_page x y widget=case widget of
    Vector {vector_widget}->Just (update_vector_widget 1 (update_store_widget (const (above_page_a x y (vector_widget DV.! 0)))) widget)
    _->EE.quick_error "above_page" 0

above_page_a::FCT.CFloat->FCT.CFloat->Widget a b c d e->Bool
above_page_a x y widget=case widget of
    Vector_visual {arrange=first_arrange,vector_visual}->case vector_visual DV.! 1 of
        Rectangle {arrange=second_arrange,half_width,half_height}->case combine_arrange first_arrange second_arrange of
            Arrange {point,matrix}->let determinant=matrix.x_x*matrix.y_y-matrix.x_y*matrix.y_x in let new_x=x-point.x-matrix.x in let new_y=y-point.y-matrix.y in abs (matrix.x+(matrix.y_y*new_x-matrix.x_y*new_y)/determinant)<=half_width&&abs (matrix.y+(matrix.x_x*new_y-matrix.y_x*new_x)/determinant)<=half_height
        _->EE.quick_error "above_page_a" 0
    _->EE.quick_error "above_page_a" 1

view_page_bool::Widget a b c d e->Int->Bool
view_page_bool widget index=case widget of
    Vector {vector_widget}->case vector_widget DV.! index of
        Store {store}->convert store
        _->EE.quick_error "view_page_bool" 0
    _->EE.quick_error "view_page_bool" 1

update_page_bool::(Bool->Bool)->Int->Widget a b c d e->Widget a b c d e
update_page_bool update index=update_vector_widget index (update_store_widget update)

view_page::Widget a b c d e->Widget a b c d e
view_page this_widget=case this_widget of
    Widget_trigger {widget}->case widget of
        Vector {vector_widget}->case vector_widget DV.! 0 of
            Vector_visual {arrange,size,vector_visual}->let hovered=get_store_widget (vector_widget DV.! 1) in let selected=get_store_widget (vector_widget DV.! 2) in let offset=if selected then if hovered then 6 else 4 else if hovered then 2 else 0 in Vector_visual {arrange=arrange,collect_order=(2+offset) DS.<| (1+offset) DS.<| DS.singleton 0,size=size,vector_visual=vector_visual}
            _->EE.quick_error "view_page" 0
        _->EE.quick_error "view_page" 1
    _->EE.quick_error "view_page" 2

update_page::Widget a b c d e->Maybe (Widget a b c d e)
update_page this_widget=case this_widget of
    Widget_trigger {next,widget_trigger,widget}->case widget of
        Vector {vector_widget}->if get_store_widget (vector_widget DV.! 3) then Just (Widget_trigger {next=next,widget_trigger=widget_trigger,widget=update_vector_widget 3 (update_store_widget (const False)) widget}) else Nothing
        _->EE.quick_error "update_page" 0
    _->EE.quick_error "update_page" 1

maybe_update_collect_page::Custom_widget e=>Maybe (Border FCT.CFloat)->Projection_path->Int->Selector a->Insert_strategy->Engine b c d e f->Engine b c d e f
maybe_update_collect_page maybe_border projection_path leaf_id selector collect_strategy engine=case DFC.getCompose (functor_lookup_projection_widget projection_path (\widget->DFC.Compose {getCompose=fmap (\this_widget->(to_collect engine.u engine.v maybe_border (view_page this_widget),this_widget)) (selector_monad_update (const update_page) selector widget)}) engine) of
    Nothing->engine
    Just (submit,new_engine)->new_engine {leaf=intmap_update leaf_id (update_projection_object (collect_a submit collect_strategy)) new_engine.leaf}

maybe_collect_update_page::Custom_widget e=>Maybe (Border FCT.CFloat)->Projection_path->Int->Selector a->Insert_strategy->Engine b c d e f->Engine b c d e f
maybe_collect_update_page maybe_border projection_path leaf_id selector collect_strategy engine=let (update,maybe_engine)=DFC.getCompose (functor_lookup_projection_widget projection_path (\widget->DFC.Compose {getCompose=(intmap_update leaf_id (update_projection_object (collect_a (to_collect engine.u engine.v maybe_border (view_page widget)) collect_strategy)),selector_monad_update (const update_page) selector widget)}) engine) in case maybe_engine of
    Nothing->engine
    Just new_engine->new_engine {leaf=update new_engine.leaf}

collect_page::Custom_widget e=>Maybe (Border FCT.CFloat)->Projection_path->Int->Selector a->Insert_strategy->Engine b c d e f->Engine b c d e f
collect_page maybe_border projection_path leaf_id selector collect_strategy engine=engine {leaf=intmap_update leaf_id (update_projection_object (selector_update (const (collect_a (to_collect engine.u engine.v maybe_border (view_page (lookup_projection_widget projection_path engine))) collect_strategy)) selector)) engine.leaf}

{-# INLINE view_page_bool #-}
{-# INLINE update_page_bool #-}