{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Engine.Projection where

import Engine.Other
import Engine.Type
import qualified Data.Foldable as DF
import qualified Data.IntMap as DIM
import qualified Data.Sequence as DS

create_image::Bool->Int->Engine a->Engine a
create_image widget_type widget_id engine=if widget_type then engine {active=intmap_update widget_id (ancestry_update_active_projection (`create_image_a` engine)) engine.active} else engine {inactive=intmap_update widget_id (ancestry_update_inactive_projection (`create_image_a` engine)) engine.inactive}

create_image_a::DS.Seq Int->Engine a->Projection (Widget a)->Projection (Widget a)
create_image_a ancestry engine projection=case projection of
    Without {object}->With {object=object,image=do_widget_transform ancestry engine object}
    _->error "create_image_a: error 1"

create_image_safe::Bool->Int->Engine a->Engine a
create_image_safe widget_type widget_id engine=if widget_type then engine {active=DIM.adjust (ancestry_update_active_projection (`create_image_safe_a` engine)) widget_id engine.active} else engine {inactive=DIM.adjust (ancestry_update_inactive_projection (`create_image_safe_a` engine)) widget_id engine.inactive}

create_image_safe_a::DS.Seq Int->Engine a->Projection (Widget a)->Projection (Widget a)
create_image_safe_a ancestry engine projection=case projection of
    Without {object}->With {object=object,image=do_widget_transform ancestry engine object}
    With {object}->With {object=object,image=do_widget_transform ancestry engine object}

remove_image::Bool->Int->Engine a->Engine a
remove_image widget_type widget_id engine=if widget_type then engine {active=intmap_update widget_id (update_active_projection remove_image_a) engine.active} else engine {inactive=intmap_update widget_id (update_inactive_projection remove_image_a) engine.inactive}

remove_image_a::Projection a->Projection a
remove_image_a projection=case projection of
    With {object}->Without {object=object}
    _->error "remove_image_a: error 1"

remove_image_safe::Bool->Int->Engine a->Engine a
remove_image_safe widget_type widget_id engine=if widget_type then engine {active=DIM.adjust (update_active_projection remove_image_safe_a) widget_id engine.active} else engine {inactive=DIM.adjust (update_inactive_projection remove_image_safe_a) widget_id engine.inactive}

remove_image_safe_a::Projection a->Projection a
remove_image_safe_a projection=case projection of
    With {object}->Without {object=object}
    _->projection

update_active_projection::(Projection (Widget a)->Projection (Widget a))->Active a->Active a
update_active_projection update active=case active of
    Active {ancestry,projection,next}->Active {ancestry=ancestry,projection=update projection,next=next}

update_inactive_projection::(Projection (Widget a)->Projection (Widget a))->Inactive a->Inactive a
update_inactive_projection update inactive=case inactive of
    Inactive {ancestry,projection}->Inactive {ancestry=ancestry,projection=update projection}

ancestry_update_active_projection::(DS.Seq Int->Projection (Widget a)->Projection (Widget a))->Active a->Active a
ancestry_update_active_projection update active=case active of
    Active {ancestry,projection,next}->Active {ancestry=ancestry,projection=update ancestry projection,next=next}

ancestry_update_inactive_projection::(DS.Seq Int->Projection (Widget a)->Projection (Widget a))->Inactive a->Inactive a
ancestry_update_inactive_projection update inactive=case inactive of
    Inactive {ancestry,projection}->Inactive {ancestry=ancestry,projection=update ancestry projection}

do_widget_transform::DS.Seq Int->Engine a->Widget a->Widget a
do_widget_transform ancestry engine widget=DF.foldr (\node_id->(intmap_lookup node_id engine.node).widget_transform engine) widget ancestry

lookup_projection::Projection_strategy->Projection a->a
lookup_projection projection_strategy=case projection_strategy of
    Object_strategy->lookup_projection_object
    Image_strategy->lookup_projection_image
    Image_safe_strategy->lookup_projection_image_safe

lookup_projection_object::Projection a->a
lookup_projection_object projection=case projection of
    Without {object}->object
    With {object}->object

lookup_projection_image::Projection a->a
lookup_projection_image projection=case projection of
    With {image}->image
    _->error "lookup_projection_image: error 1"

lookup_projection_image_safe::Projection a->a
lookup_projection_image_safe projection=case projection of
    Without {object}->object
    With {image}->image

update_object::(a->a)->Projection a->Projection a
update_object update projection=case projection of
    Without {object}->Without {object=update object}
    With {object}->Without {object=update object}

lookup_inactive_widget::Projection_path->DIM.IntMap (Inactive a)->Widget a
lookup_inactive_widget projection_path inactive=case projection_path of
    Object_path {projection_id}->lookup_projection_object (intmap_lookup projection_id inactive).projection
    Image_path {projection_id}->lookup_projection_image (intmap_lookup projection_id inactive).projection
    Image_safe_path {projection_id}->lookup_projection_image_safe (intmap_lookup projection_id inactive).projection

update_lookup_inactive_object::Projection_move->(Widget a->Widget a)->DIM.IntMap (Inactive a)->(DIM.IntMap (Inactive a),Widget a)
update_lookup_inactive_object projection_move update inactive=case projection_move of
    Object_move {consume,projection_id}->intmap_calculate projection_id (update_lookup_inactive_object_a consume update) inactive
    Image_move {projection_id}->(inactive,lookup_projection_image (intmap_lookup projection_id inactive).projection)
    Image_safe_move {projection_id}->(inactive,lookup_projection_image_safe (intmap_lookup projection_id inactive).projection)

update_lookup_inactive_object_a::Bool->(Widget a->Widget a)->Inactive a->(Inactive a,Widget a)
update_lookup_inactive_object_a consume update inactive=case inactive of
    Inactive {ancestry,projection}->case projection of
        Without {object}->(if consume then Inactive {ancestry=ancestry,projection=Without {object=update object}} else inactive,object)
        With {object}->(if consume then Inactive {ancestry=ancestry,projection=Without {object=update object}} else inactive,object)