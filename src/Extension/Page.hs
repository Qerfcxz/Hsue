{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Extension.Page where

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

create_rectangle_request::Color->FCT.CFloat->FCT.CFloat->Visual_request
create_rectangle_request color half_width half_height=case color of
    Color {red,green,blue,alpha}->Convex_polygon_request {arrange=Arrange {point=origin,matrix=identity_matrix,red=red,green=green,blue=blue,alpha=alpha},point_set=DS.singleton (Point {x=negate half_width,y=negate half_height}) DS.|> Point {x=half_width,y=negate half_height} DS.|> Point {x=half_width,y=half_height} DS.|> Point {x=negate half_width,y=half_height}}

create_page_request::(Event a->Engine b a c d e->Maybe Int)->(Event a->Engine b a c d e->Widget b a c d e->Widget b a c d e)->Extension_widget_request b a c d e->Widget_request b a c d e
create_page_request next logic page_request=case page_request of
    Page {text_request,inner_thickness,outer_thickness,inner_color,outer_color,inner_selected_color,outer_selected_color}->case text_request of
        Visual_request {visual_request}->case visual_request of
            Text_request {text_width,text_height}->let half_width=text_width/2 in let half_height=text_height/2 in let inner_width=half_width+inner_thickness in let inner_height=half_height+inner_thickness in let outer_width=inner_width+outer_thickness in let outer_height=inner_height+outer_thickness in Widget_trigger_request {next=next,widget_trigger=create_page_request_a logic,widget_request=Vector_request {index=0,vector_widget_request=DS.singleton (Vector_visual_request {arrange=Arrange {point=Point {x=0,y=0},matrix=identity_matrix,red=1,green=1,blue=1,alpha=1},collect_order=DS.singleton 2 DS.|> 1 DS.|> 0,vector_visual_request=DV.fromList [visual_request,create_rectangle_request inner_color inner_width inner_height,create_rectangle_request outer_color outer_width outer_height,create_rectangle_request inner_selected_color inner_width inner_height,create_rectangle_request outer_selected_color outer_width outer_height]}) DS.|> Store_request {store=convert False} DS.|> Store_request {store=convert True}}}
            _->EE.quick_error "create_page_request" 0
        _->EE.quick_error "create_page_request" 1

create_page_request_a::(Event a->Engine b a c d e->Widget b a c d e->Widget b a c d e)->Event a->Engine b a c d e->Widget b a c d e->(Widget b a c d e,Engine b a c d e->Engine b a c d e)
create_page_request_a logic event engine widget=(logic event engine widget,id)

view_page_bool::Widget a b c d e->Bool->Bool
view_page_bool this_widget which=case this_widget of
    Widget_trigger {widget}->case widget of
        Vector {vector_widget}->case vector_widget DV.! (if which then 2 else 1) of
            Store {store}->convert store
            _->EE.quick_error "view_page_bool" 0
        _->EE.quick_error "view_page_bool" 1
    _->EE.quick_error "view_page_bool" 2

click_page::FCT.CFloat->FCT.CFloat->Widget a b c d e->Maybe (Widget a b c d e)
click_page x y this_widget=case this_widget of
    Widget_trigger {next,widget_trigger,widget}->case widget of
        Vector {vector_widget}->Just (Widget_trigger {next=next,widget_trigger=widget_trigger,widget=update_vector_widget 1 (update_store_widget (const (click_page_a x y (vector_widget DV.! 0)))) widget})
        _->EE.quick_error "click_page" 0
    _->EE.quick_error "click_page" 1

click_page_a::FCT.CFloat->FCT.CFloat->Widget a b c d e->Bool
click_page_a x y widget=case widget of
    Vector_visual {arrange,vector_visual}->case vector_visual DV.! 1 of
        Convex_polygon {arrange=inner_arrange,point_set}->case combine_arrange arrange inner_arrange of
            Arrange {point,matrix}->let positive_point=DS.index point_set 2 in let determinant=matrix.x_x*matrix.y_y-matrix.x_y*matrix.y_x in let new_x=x-point.x-matrix.x in let new_y=y-point.y-matrix.y in abs (matrix.x+(matrix.y_y*new_x-matrix.x_y*new_y)/determinant)<=positive_point.x&&abs (matrix.y+(matrix.x_x*new_y-matrix.y_x*new_x)/determinant)<=positive_point.y
        _->EE.quick_error "click_page_a" 0
    _->EE.quick_error "click_page_a" 1

update_page_bool::(Bool->Bool)->Bool->Widget a b c d e->Widget a b c d e
update_page_bool update which this_widget=case this_widget of
    Widget_trigger {next,widget_trigger,widget}->let index=if which then 2 else 1 in Widget_trigger {next=next,widget_trigger=widget_trigger,widget=update_vector_widget index (update_store_widget update) widget}
    _->EE.quick_error "update_page_bool" 0

view_page::Widget a b c d e->Widget a b c d e
view_page this_widget=case this_widget of
    Widget_trigger {widget}->case widget of
        Vector {vector_widget}->case vector_widget DV.! 0 of
            Vector_visual {arrange,size,vector_visual}->if get_store_widget (vector_widget DV.! 2) then Vector_visual {arrange=arrange,collect_order=4 DS.<| 3 DS.<| DS.singleton 0,size=size,vector_visual=vector_visual} else Vector_visual {arrange=arrange,collect_order=2 DS.<| 1 DS.<| DS.singleton 0,size=size,vector_visual=vector_visual}
            _->EE.quick_error "view_page" 0
        _->EE.quick_error "view_page" 1
    _->EE.quick_error "view_page" 2

update_page::Widget a b c d e->Maybe (Widget a b c d e)
update_page this_widget=case this_widget of
    Widget_trigger {next,widget_trigger,widget}->case widget of
        Vector {vector_widget}->if get_store_widget (vector_widget DV.! 2) then Just (Widget_trigger {next=next,widget_trigger=widget_trigger,widget=update_vector_widget 2 (update_store_widget (const False)) widget}) else Nothing
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