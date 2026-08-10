{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Extension.Button where

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

create_button_request::(Event a->Engine b a c d e->Maybe Int)->(Engine b a c d e->Engine b a c d e)->Extension_widget_request b a c d e->Widget_request b a c d e
create_button_request next action button_request=case button_request of
    Button {window_id,text_request,inner_thickness,outer_thickness,inner_color,outer_color,inner_hovered_color,outer_hovered_color,inner_pressed_color,outer_pressed_color,inner_hovered_pressed_color,outer_hovered_pressed_color}->case text_request of
        Visual_request {visual_request}->case visual_request of
            Text_request {text_width,text_height}->let half_width=text_width/2 in let half_height=text_height/2 in let inner_width=half_width+inner_thickness in let inner_height=half_height+inner_thickness in let outer_width=inner_width+outer_thickness in let outer_height=inner_height+outer_thickness in Widget_trigger_request {next=next,widget_trigger=button_widget_trigger action,widget_request=Vector_request {index=0,vector_widget_request=DS.singleton (Vector_visual_request {arrange=Arrange {point=Point {x=0,y=0},matrix=identity_matrix,red=1,green=1,blue=1,alpha=1},collect_order=2 DS.<| 1 DS.<| DS.singleton 0,vector_visual_request=DV.fromList [visual_request,create_rectangle_request inner_color inner_width inner_height,create_rectangle_request outer_color outer_width outer_height,create_rectangle_request inner_hovered_color inner_width inner_height,create_rectangle_request outer_hovered_color outer_width outer_height,create_rectangle_request inner_pressed_color inner_width inner_height,create_rectangle_request outer_pressed_color outer_width outer_height,create_rectangle_request inner_hovered_pressed_color inner_width inner_height,create_rectangle_request outer_hovered_pressed_color outer_width outer_height]}) DS.|> Store_request {store=convert False} DS.|> Store_request {store=convert False} DS.|> Store_request {store=convert True} DS.|> Store_request {store=convert window_id}}}
            _->EE.quick_error "create_button_request" 0
        _->EE.quick_error "create_button_request" 1
    _->EE.quick_error "create_button_request" 2

button_widget_trigger::(Engine a b c d e->Engine a b c d e)->Event b->Engine a b c d e->Widget a b c d e->(Widget a b c d e,Engine a b c d e->Engine a b c d e)
button_widget_trigger this_action event _ widget=case event of
    At {window_id=event_window_id,action}->case widget of
        Vector {vector_widget}->let this_window_id=get_store_widget (vector_widget DV.! 4) in if event_window_id==this_window_id then case action of
            Click {press,mouse_button,x,y}->case mouse_button of
                Mouse_button_left->case press of
                    Press_up->let above=above_button_a x y (vector_widget DV.! 0) in let pressed=view_button_bool widget 2 in if pressed then let new_widget=update_button_bool (const True) 3 (update_button_bool (const False) 2 widget) in if above then (new_widget,this_action) else (new_widget,id) else (widget,id)
                    Press_down->let above=above_button_a x y (vector_widget DV.! 0) in if above then (update_button_bool (const True) 3 (update_button_bool (const True) 2 widget),id) else (widget,id)
                _->(widget,id)
            Move {x,y}->let above=above_button_a x y (vector_widget DV.! 0) in let hovered=view_button_bool widget 1 in if above/=hovered then let new_widget=update_button_bool (const True) 3 (update_button_bool (const above) 1 widget) in if above then (new_widget,\engine->engine {request=engine.request DS.|> Set_system_cursor {system_cursor=System_cursor_pointer}}) else (new_widget,\this_engine->this_engine {request=this_engine.request DS.|> Set_system_cursor {system_cursor=System_cursor_default}}) else (widget,id)
            _->(widget,id)
        else (widget,id)
        _->EE.quick_error "button_widget_trigger" 0
    _->(widget,id)

above_button::FCT.CFloat->FCT.CFloat->Widget a b c d e->Maybe (Widget a b c d e)
above_button x y widget=case widget of
    Vector {vector_widget}->Just (update_vector_widget 1 (update_store_widget (const (above_button_a x y (vector_widget DV.! 0)))) widget)
    _->EE.quick_error "above_button" 0

above_button_a::FCT.CFloat->FCT.CFloat->Widget a b c d e->Bool
above_button_a x y widget=case widget of
    Vector_visual {arrange=first_arrange,vector_visual}->case vector_visual DV.! 1 of
        Rectangle {arrange=second_arrange,half_width,half_height}->case combine_arrange first_arrange second_arrange of
            Arrange {point,matrix}->let determinant=matrix.x_x*matrix.y_y-matrix.x_y*matrix.y_x in let new_x=x-point.x-matrix.x in let new_y=y-point.y-matrix.y in abs (matrix.x+(matrix.y_y*new_x-matrix.x_y*new_y)/determinant)<=half_width&&abs (matrix.y+(matrix.x_x*new_y-matrix.y_x*new_x)/determinant)<=half_height
        _->EE.quick_error "above_button_a" 0
    _->EE.quick_error "above_button_a" 1

view_button_bool::Widget a b c d e->Int->Bool
view_button_bool widget index=case widget of
    Vector {vector_widget}->case vector_widget DV.! index of
        Store {store}->convert store
        _->EE.quick_error "view_button_bool" 0
    _->EE.quick_error "view_button_bool" 1

view_button::Widget a b c d e->Widget a b c d e
view_button this_widget=case this_widget of
    Widget_trigger {widget}->case widget of
        Vector {vector_widget}->case vector_widget DV.! 0 of
            Vector_visual {arrange,size,vector_visual}->let hovered=get_store_widget (vector_widget DV.! 1) in let pressed=get_store_widget (vector_widget DV.! 2) in let offset=if pressed then if hovered then 6 else 4 else if hovered then 2 else 0 in Vector_visual {arrange=arrange,collect_order=(2+offset) DS.<| (1+offset) DS.<| DS.singleton 0,size=size,vector_visual=vector_visual}
            _->EE.quick_error "view_button" 0
        _->EE.quick_error "view_button" 1
    _->EE.quick_error "view_button" 2

update_button::Widget a b c d e->Maybe (Widget a b c d e)
update_button this_widget=case this_widget of
    Widget_trigger {next,widget_trigger,widget}->case widget of
        Vector {vector_widget}->if get_store_widget (vector_widget DV.! 3) then Just (Widget_trigger {next=next,widget_trigger=widget_trigger,widget=update_vector_widget 3 (update_store_widget (const False)) widget}) else Nothing
        _->EE.quick_error "update_button" 0
    _->EE.quick_error "update_button" 1

update_button_bool::(Bool->Bool)->Int->Widget a b c d e->Widget a b c d e
update_button_bool update index=update_vector_widget index (update_store_widget update)

maybe_update_collect_button::Custom_widget e=>Maybe (Border FCT.CFloat)->Projection_path->Int->Selector a->Insert_strategy->Engine b c d e f->Engine b c d e f
maybe_update_collect_button maybe_border projection_path leaf_id selector collect_strategy engine=case DFC.getCompose (functor_lookup_projection_widget projection_path (\widget->DFC.Compose {getCompose=fmap (\this_widget->(to_collect engine.u engine.v maybe_border (view_button this_widget),this_widget)) (selector_monad_update (const update_button) selector widget)}) engine) of
    Nothing->engine
    Just (submit,new_engine)->new_engine {leaf=intmap_update leaf_id (update_projection_object (collect_a submit collect_strategy)) new_engine.leaf}

maybe_collect_update_button::Custom_widget e=>Maybe (Border FCT.CFloat)->Projection_path->Int->Selector a->Insert_strategy->Engine b c d e f->Engine b c d e f
maybe_collect_update_button maybe_border projection_path leaf_id selector collect_strategy engine=let (update,maybe_engine)=DFC.getCompose (functor_lookup_projection_widget projection_path (\widget->DFC.Compose {getCompose=(intmap_update leaf_id (update_projection_object (collect_a (to_collect engine.u engine.v maybe_border (view_button widget)) collect_strategy)),selector_monad_update (const update_button) selector widget)}) engine) in case maybe_engine of
    Nothing->engine
    Just new_engine->new_engine {leaf=update new_engine.leaf}

collect_button::Custom_widget e=>Maybe (Border FCT.CFloat)->Projection_path->Int->Selector a->Insert_strategy->Engine b c d e f->Engine b c d e f
collect_button maybe_border projection_path leaf_id selector collect_strategy engine=engine {leaf=intmap_update leaf_id (update_projection_object (selector_update (const (collect_a (to_collect engine.u engine.v maybe_border (view_button (lookup_projection_widget projection_path engine))) collect_strategy)) selector)) engine.leaf}