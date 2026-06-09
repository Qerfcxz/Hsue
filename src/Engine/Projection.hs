{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Engine.Projection where

import Engine.Other
import Engine.Type
import qualified Data.Foldable as DF
import qualified Data.IntMap as DIM
import qualified Data.Sequence as DS

create_projection::Widget_type->Int->Engine a->Engine a
create_projection widget_type widget_id engine=case widget_type of
    Active_widget->engine {active=intmap_update widget_id (ancestry_update_projection_active (`insert_projection` engine)) engine.active}
    Free_widget->engine {free=intmap_update widget_id (ancestry_update_projection_free (`insert_projection` engine)) engine.free}
    Bound_widget->engine {bound=intmap_update widget_id (ancestry_update_projection_bound (`insert_projection` engine)) engine.bound}

create_projection_safe::Widget_type->Int->Engine a->Engine a
create_projection_safe widget_type widget_id engine=case widget_type of
    Active_widget->engine {active=DIM.adjust (ancestry_update_projection_active (`insert_projection_safe` engine)) widget_id engine.active}
    Free_widget->engine {free=DIM.adjust (ancestry_update_projection_free (`insert_projection_safe` engine)) widget_id engine.free}
    Bound_widget->engine {bound=DIM.adjust (ancestry_update_projection_bound (`insert_projection_safe` engine)) widget_id engine.bound}

remove_projection::Widget_type->Int->Engine a->Engine a
remove_projection widget_type widget_id engine=case widget_type of
    Active_widget->engine {active=intmap_update widget_id (update_projection_active projection_remove) engine.active}
    Free_widget->engine {free=intmap_update widget_id (update_projection_free projection_remove) engine.free}
    Bound_widget->engine {bound=intmap_update widget_id (update_projection_bound projection_remove) engine.bound}

remove_projection_safe::Widget_type->Int->Engine a->Engine a
remove_projection_safe widget_type widget_id engine=case widget_type of
    Active_widget->engine {active=DIM.adjust (update_projection_active projection_remove_safe) widget_id engine.active}
    Free_widget->engine {free=DIM.adjust (update_projection_free projection_remove_safe) widget_id engine.free}
    Bound_widget->engine {bound=DIM.adjust (update_projection_bound projection_remove_safe) widget_id engine.bound}

update_projection_active::(Projection (Widget a)->Projection (Widget a))->Active a->Active a
update_projection_active update active=case active of
    Active {next,ancestry,projection}->Active {next=next,ancestry=ancestry,projection=update projection}

update_projection_free::(Projection (Widget a)->Projection (Widget a))->Free a->Free a
update_projection_free update free=case free of
    Free {ancestry,projection}->Free {ancestry=ancestry,projection=update projection}

update_projection_bound::(Projection (Widget a)->Projection (Widget a))->Bound a->Bound a
update_projection_bound update bound=case bound of
    Bound {window_id,ancestry,projection}->Bound {window_id=window_id,ancestry=ancestry,projection=update projection}

ancestry_update_projection_active::(DS.Seq Int->Projection (Widget a)->Projection (Widget a))->Active a->Active a
ancestry_update_projection_active update active=case active of
    Active {next,ancestry,projection}->Active {next=next,ancestry=ancestry,projection=update ancestry projection}

ancestry_update_projection_free::(DS.Seq Int->Projection (Widget a)->Projection (Widget a))->Free a->Free a
ancestry_update_projection_free update free=case free of
    Free {ancestry,projection}->Free {ancestry=ancestry,projection=update ancestry projection}

ancestry_update_projection_bound::(DS.Seq Int->Projection (Widget a)->Projection (Widget a))->Bound a->Bound a
ancestry_update_projection_bound update bound=case bound of
    Bound {window_id,ancestry,projection}->Bound {window_id=window_id,ancestry=ancestry,projection=update ancestry projection}

do_widget_transform::DS.Seq Int->Engine a->Widget a->Widget a
do_widget_transform ancestry engine widget=DF.foldr (\node_id->(intmap_lookup node_id engine.node).widget_transform engine) widget ancestry

insert_projection::DS.Seq Int->Engine a->Projection (Widget a)->Projection (Widget a)
insert_projection ancestry engine projection=case projection of
    Without {object}->With {object=object,image=do_widget_transform ancestry engine object}
    _->error "insert_projection: error 1"

insert_projection_safe::DS.Seq Int->Engine a->Projection (Widget a)->Projection (Widget a)
insert_projection_safe ancestry engine projection=case projection of
    Without {object}->With {object=object,image=do_widget_transform ancestry engine object}
    With {object}->With {object=object,image=do_widget_transform ancestry engine object}

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

path_lookup_projection_bound::Projection_path->DIM.IntMap (Bound a)->Widget a
path_lookup_projection_bound projection_path bound=case projection_path of
    Object_path {projection_id}->projection_lookup_object (intmap_lookup projection_id bound).projection
    Image_path {projection_id}->projection_lookup_image (intmap_lookup projection_id bound).projection
    Image_safe_path {projection_id}->projection_lookup_image_safe (intmap_lookup projection_id bound).projection

path_update_projection_free::Projection_path->(Widget a->Widget a)->DIM.IntMap (Free a)->DIM.IntMap (Free a)
path_update_projection_free projection_path update free=case projection_path of
    Object_path {projection_id}->intmap_update projection_id (update_projection_free (projection_update_object update)) free
    Image_path {projection_id}->intmap_update projection_id (update_projection_free (projection_update_image update)) free
    Image_safe_path {projection_id}->intmap_update projection_id (update_projection_free (projection_update_image_safe update)) free

move_update_lookup_projection_free::Projection_move->(Widget a->Widget a)->DIM.IntMap (Free a)->(DIM.IntMap (Free a),Widget a)
move_update_lookup_projection_free projection_move update free=case projection_move of
    Object_move {consume,projection_id}->intmap_calculate projection_id (consume_update_lookup_projection_free_a consume update) free
    Image_move {projection_id}->(free,projection_lookup_image (intmap_lookup projection_id free).projection)
    Image_safe_move {projection_id}->(free,projection_lookup_image_safe (intmap_lookup projection_id free).projection)

consume_update_lookup_projection_free_a::Bool->(Widget a->Widget a)->Free a->(Free a,Widget a)
consume_update_lookup_projection_free_a consume update free=case free of
    Free {ancestry,projection}->case projection of
        Without {object}->(if consume then Free {ancestry=ancestry,projection=projection {object=update object}} else free,object)
        With {object}->(if consume then Free {ancestry=ancestry,projection=projection {object=update object}} else free,object)