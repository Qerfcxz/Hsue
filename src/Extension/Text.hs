{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Extension.Text where

import Engine.Collector
import Engine.Container
import Engine.Operation
import Engine.Projection
import Engine.Selector
import Engine.Type
import qualified Error.Error as EE
import qualified Data.Functor.Compose as DFC
import qualified Data.Sequence as DS
import qualified Data.Vector as DV
import qualified Foreign.C.Types as FCT

scroll_text::FCT.CFloat->Widget a b c d e->(Widget a b c d e,Bool)
scroll_text scroll widget=case widget of
    Text {origin,matrix,half_width,half_height,y,min_y,max_y,article,charset,locked}->let new_y=max min_y (min (max min_y max_y) (y+scroll)) in if y==new_y then (widget,False) else (Text {origin=origin,matrix=matrix,half_width=half_width,half_height=half_height,y=new_y,min_y=min_y,max_y=max_y,article=article,charset=charset,locked=locked},True)
    _->EE.quick_error "scroll_text" 0

scroll_top_text::Widget a b c d e->(Widget a b c d e,Bool)
scroll_top_text widget=case widget of
    Text {origin,matrix,half_width,half_height,y,min_y,max_y,article,charset,locked}->if y==min_y then (widget,False) else (Text {origin=origin,matrix=matrix,half_width=half_width,half_height=half_height,y=min_y,min_y=min_y,max_y=max_y,article=article,charset=charset,locked=locked},True)
    _->EE.quick_error "scroll_top_text" 0

scroll_bottom_text::Widget a b c d e->(Widget a b c d e,Bool)
scroll_bottom_text widget=case widget of
    Text {origin,matrix,half_width,half_height,y,min_y,max_y,article,charset,locked}->let new_max_y=max min_y max_y in if y==new_max_y then (widget,False) else (Text {origin=origin,matrix=matrix,half_width=half_width,half_height=half_height,y=new_max_y,min_y=min_y,max_y=max_y,article=article,charset=charset,locked=locked},True)
    _->EE.quick_error "scroll_bottom_text" 0

click_text::FCT.CFloat->FCT.CFloat->Widget a b c d e->Bool
click_text x y widget=case widget of
    Text {origin,matrix,half_width,half_height}->let determinant=matrix.x_x*matrix.y_y-matrix.x_y*matrix.y_x in let new_x=x-origin.x-matrix.x in let new_y=y-origin.y-matrix.y in abs ((matrix.y_y*new_x-matrix.x_y*new_y)/determinant)<=half_width&&abs ((matrix.x_x*new_y-matrix.y_x*new_x)/determinant)<=half_height
    _->EE.quick_error "click_text" 0

create_text_trigger_request::(Event a->Engine b a c d e->Maybe Int)->(Event a->Engine b a c d e->Widget b a c d e->Widget b a c d e)->Widget_request b a c d e->Widget_request b a c d e
create_text_trigger_request next logic text_request=Widget_trigger_request {next=next,widget_trigger=create_text_trigger_request_a logic,widget_request=Vector_request {index=0,vector_widget_request=DS.singleton text_request DS.|> Store_request {store=convert False} DS.|> Store_request {store=convert True}}}

create_text_trigger_request_a::(Event a->Engine b a c d e->Widget b a c d e->Widget b a c d e)->Event a->Engine b a c d e->Widget b a c d e->(Widget b a c d e,Engine b a c d e->Engine b a c d e)
create_text_trigger_request_a logic event engine widget=(logic event engine widget,id)

view_text_trigger::(Widget a b c d e->Widget a b c d e)->Widget a b c d e->Widget a b c d e
view_text_trigger view this_widget=case this_widget of
    Widget_trigger {widget}->case widget of
        Vector {vector_widget}->view (vector_widget DV.! 0)
        _->EE.quick_error "view_text_trigger" 0
    _->EE.quick_error "view_text_trigger" 1

update_text_trigger::Widget a b c d e->Maybe (Widget a b c d e)
update_text_trigger this_widget=case this_widget of
    Widget_trigger {next,widget_trigger,widget}->case widget of
        Vector {vector_widget}->case vector_widget DV.! 2 of
            Store {store}->if convert store then Just (Widget_trigger {next=next,widget_trigger=widget_trigger,widget=update_vector_widget 2 (update_store_widget (const False)) widget}) else Nothing
            _->EE.quick_error "update_text_trigger" 0
        _->EE.quick_error "update_text_trigger" 1
    _->EE.quick_error "update_text_trigger" 2

view_text_trigger_bool::Widget a b c d e->Bool->Bool
view_text_trigger_bool this_widget which=case this_widget of
    Widget_trigger {widget}->case widget of
        Vector {vector_widget}->case vector_widget DV.! (if which then 2 else 1) of
            Store {store}->convert store
            _->EE.quick_error "view_text_trigger_bool" 0
        _->EE.quick_error "view_text_trigger_bool" 1
    _->EE.quick_error "view_text_trigger_bool" 2

update_text_trigger_bool::(Bool->Bool)->Bool->Widget a b c d e->Widget a b c d e
update_text_trigger_bool update which this_widget=case this_widget of
    Widget_trigger {next,widget_trigger,widget}->let index=if which then 2 else 1 in Widget_trigger {next=next,widget_trigger=widget_trigger,widget=update_vector_widget index (update_store_widget update) widget}
    _->EE.quick_error "update_text_trigger_bool" 0

click_text_trigger::FCT.CFloat->FCT.CFloat->Widget a b c d e->Maybe (Widget a b c d e)
click_text_trigger x y this_widget=case this_widget of
    Widget_trigger {next,widget_trigger,widget}->case widget of
        Vector {vector_widget}->Just (Widget_trigger {next=next,widget_trigger=widget_trigger,widget=update_vector_widget 1 (update_store_widget (const (click_text x y (vector_widget DV.! 0)))) widget})
        _->EE.quick_error "click_text_trigger" 0
    _->EE.quick_error "click_text_trigger" 1

collect_text_trigger::Custom_widget d=>(Widget a b c d e->Widget a b c d e)->Maybe (Border FCT.CFloat)->Projection_path->Int->Selector f->Insert_strategy->Engine a b c d e->Engine a b c d e
collect_text_trigger view maybe_border projection_path leaf_id selector collect_strategy engine=engine {leaf=intmap_update leaf_id (update_projection_object (selector_update (const (collect_a (DS.singleton (to_collect engine.u engine.v maybe_border (view_text_trigger view (lookup_projection_widget projection_path engine)))) collect_strategy)) selector)) engine.leaf}

maybe_update_collect_text_trigger::Custom_widget d=>(Widget a b c d e->Widget a b c d e)->Maybe (Border FCT.CFloat)->Projection_path->Int->Selector f->Insert_strategy->Engine a b c d e->Engine a b c d e
maybe_update_collect_text_trigger view maybe_border projection_path leaf_id selector collect_strategy engine=case DFC.getCompose (functor_lookup_projection_widget projection_path (\widget->DFC.Compose {getCompose=fmap (\this_widget->(to_collect engine.u engine.v maybe_border (view_text_trigger view this_widget),this_widget)) (selector_monad_update (const update_text_trigger) selector widget)}) engine) of
    Nothing->engine
    Just (submit,new_engine)->new_engine {leaf=intmap_update leaf_id (update_projection_object (collect_a (DS.singleton submit) collect_strategy)) new_engine.leaf}

maybe_collect_update_text_trigger::Custom_widget d=>(Widget a b c d e->Widget a b c d e)->Maybe (Border FCT.CFloat)->Projection_path->Int->Selector f->Insert_strategy->Engine a b c d e->Engine a b c d e
maybe_collect_update_text_trigger view maybe_border projection_path leaf_id selector collect_strategy engine=let (update,maybe_engine)=DFC.getCompose (functor_lookup_projection_widget projection_path (\widget->DFC.Compose {getCompose=(intmap_update leaf_id (update_projection_object (collect_a (DS.singleton (to_collect engine.u engine.v maybe_border (view_text_trigger view widget))) collect_strategy)),selector_monad_update (const update_text_trigger) selector widget)}) engine) in case maybe_engine of
    Nothing->engine
    Just new_engine->new_engine {leaf=update new_engine.leaf}