{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Engine.Projection where

import Engine.Other
import Engine.Type
import qualified Data.Foldable as DF
import qualified Data.IntMap as DIM
import qualified Data.Sequence as DS

create_projection::Bool->Int->Engine a->Engine a
create_projection widget_type widget_id engine=if widget_type then engine {active=intmap_update widget_id (ancestry_update_projection_active (`insert_projection` engine)) engine.active} else engine {inactive=intmap_update widget_id (ancestry_update_projection_inactive (`insert_projection` engine)) engine.inactive}

create_projection_safe::Bool->Int->Engine a->Engine a
create_projection_safe widget_type widget_id engine=if widget_type then engine {active=DIM.adjust (ancestry_update_projection_active (`insert_projection_safe` engine)) widget_id engine.active} else engine {inactive=DIM.adjust (ancestry_update_projection_inactive (`insert_projection_safe` engine)) widget_id engine.inactive}

remove_projection::Bool->Int->Engine a->Engine a
remove_projection widget_type widget_id engine=if widget_type then engine {active=intmap_update widget_id (update_projection_active projection_remove) engine.active} else engine {inactive=intmap_update widget_id (update_projection_inactive projection_remove) engine.inactive}

remove_projection_safe::Bool->Int->Engine a->Engine a
remove_projection_safe widget_type widget_id engine=if widget_type then engine {active=DIM.adjust (update_projection_active projection_remove_safe) widget_id engine.active} else engine {inactive=DIM.adjust (update_projection_inactive projection_remove_safe) widget_id engine.inactive}

update_projection_active::(Projection (Widget a)->Projection (Widget a))->Active a->Active a
update_projection_active update active=case active of
    Active {ancestry,projection,next}->Active {ancestry=ancestry,projection=update projection,next=next}

update_projection_inactive::(Projection (Widget a)->Projection (Widget a))->Inactive a->Inactive a
update_projection_inactive update inactive=case inactive of
    Inactive {ancestry,projection}->Inactive {ancestry=ancestry,projection=update projection}

ancestry_update_projection_active::(DS.Seq Int->Projection (Widget a)->Projection (Widget a))->Active a->Active a
ancestry_update_projection_active update active=case active of
    Active {ancestry,projection,next}->Active {ancestry=ancestry,projection=update ancestry projection,next=next}

ancestry_update_projection_inactive::(DS.Seq Int->Projection (Widget a)->Projection (Widget a))->Inactive a->Inactive a
ancestry_update_projection_inactive update inactive=case inactive of
    Inactive {ancestry,projection}->Inactive {ancestry=ancestry,projection=update ancestry projection}

insert_projection::DS.Seq Int->Engine a->Projection (Widget a)->Projection (Widget a)
insert_projection ancestry engine projection=case projection of
    Without {object}->With {object=object,image=do_widget_transform ancestry engine object}
    _->error "insert_projection: error 1"

insert_projection_safe::DS.Seq Int->Engine a->Projection (Widget a)->Projection (Widget a)
insert_projection_safe ancestry engine projection=case projection of
    Without {object}->With {object=object,image=do_widget_transform ancestry engine object}
    With {object}->With {object=object,image=do_widget_transform ancestry engine object}

do_widget_transform::DS.Seq Int->Engine a->Widget a->Widget a
do_widget_transform ancestry engine widget=DF.foldr (\node_id->(intmap_lookup node_id engine.node).widget_transform engine) widget ancestry

projection_remove::Projection a->Projection a
projection_remove projection=case projection of
    With {object}->Without {object=object}
    _->error "projection_remove: error 1"

projection_remove_safe::Projection a->Projection a
projection_remove_safe projection=case projection of
    With {object}->Without {object=object}
    _->projection

projection_lookup::Projection_strategy->Projection a->a
projection_lookup projection_strategy=case projection_strategy of
    Object_strategy->projection_lookup_object
    Image_strategy->projection_lookup_image
    Image_safe_strategy->projection_lookup_image_safe

projection_lookup_object::Projection a->a
projection_lookup_object projection=case projection of
    Without {object}->object
    With {object}->object

projection_lookup_image::Projection a->a
projection_lookup_image projection=case projection of
    With {image}->image
    _->error "projection_lookup_image: error 1"

projection_lookup_image_safe::Projection a->a
projection_lookup_image_safe projection=case projection of
    Without {object}->object
    With {image}->image

projection_update::Projection_strategy->(a->a)->Projection a->Projection a
projection_update projection_strategy=case projection_strategy of
    Object_strategy->projection_update_object
    Image_strategy->projection_update_image
    Image_safe_strategy->projection_update_image_safe

projection_update_object::(a->a)->Projection a->Projection a
projection_update_object update projection=case projection of
    Without {object}->Without {object=update object}
    With {object,image}->With {object=update object,image=image}

projection_update_image::(a->a)->Projection a->Projection a
projection_update_image update projection=case projection of
    With {object,image}->With {object=object,image=update image}
    _->error "projection_update_image: error 1"

projection_update_image_safe::(a->a)->Projection a->Projection a
projection_update_image_safe update projection=case projection of
    Without {object}->Without {object=update object}
    With {object,image}->With {object=object,image=update image}

path_lookup_projection_inactive::Projection_path->DIM.IntMap (Inactive a)->Widget a
path_lookup_projection_inactive projection_path inactive=case projection_path of
    Object_path {projection_id}->projection_lookup_object (intmap_lookup projection_id inactive).projection
    Image_path {projection_id}->projection_lookup_image (intmap_lookup projection_id inactive).projection
    Image_safe_path {projection_id}->projection_lookup_image_safe (intmap_lookup projection_id inactive).projection

path_update_projection_inactive::Projection_path->(Widget a->Widget a)->DIM.IntMap (Inactive a)->DIM.IntMap (Inactive a)
path_update_projection_inactive projection_path update inactive=case projection_path of
    Object_path {projection_id}->intmap_update projection_id (update_projection_inactive (projection_update_object update)) inactive
    Image_path {projection_id}->intmap_update projection_id (update_projection_inactive (projection_update_image update)) inactive
    Image_safe_path {projection_id}->intmap_update projection_id (update_projection_inactive (projection_update_image_safe update)) inactive

move_update_lookup_projection_inactive::Projection_move->(Widget a->Widget a)->DIM.IntMap (Inactive a)->(DIM.IntMap (Inactive a),Widget a)
move_update_lookup_projection_inactive projection_move update inactive=case projection_move of
    Object_move {consume,projection_id}->intmap_calculate projection_id (move_update_lookup_projection_inactive_a consume update) inactive
    Image_move {projection_id}->(inactive,projection_lookup_image (intmap_lookup projection_id inactive).projection)
    Image_safe_move {projection_id}->(inactive,projection_lookup_image_safe (intmap_lookup projection_id inactive).projection)

move_update_lookup_projection_inactive_a::Bool->(Widget a->Widget a)->Inactive a->(Inactive a,Widget a)
move_update_lookup_projection_inactive_a consume update inactive=case inactive of
    Inactive {ancestry,projection}->case projection of
        Without {object}->(if consume then Inactive {ancestry=ancestry,projection=projection {object=update object}} else inactive,object)
        With {object}->(if consume then Inactive {ancestry=ancestry,projection=projection {object=update object}} else inactive,object)