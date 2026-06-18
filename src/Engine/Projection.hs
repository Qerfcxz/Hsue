{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Engine.Projection where

import Engine.Other
import Engine.Type
import qualified Data.Foldable as DF
import qualified Data.IntMap as DIM
import qualified Data.Sequence as DS
import qualified Data.Tuple as DT

create_image::Int->Engine a->Engine a
create_image leaf_id engine=engine {leaf=intmap_update leaf_id (create_projection_image engine) engine.leaf}

create_image_safe::Int->Engine a->Engine a
create_image_safe leaf_id engine=engine {leaf=DIM.adjust (create_projection_image_safe engine) leaf_id engine.leaf}

remove_image::Int->Engine a->Engine a
remove_image leaf_id engine=engine {leaf=intmap_update leaf_id remove_projection_image engine.leaf}

remove_image_safe::Int->Engine a->Engine a
remove_image_safe leaf_id engine=engine {leaf=DIM.adjust remove_projection_image_safe leaf_id engine.leaf}

do_widget_transform::DS.Seq Int->Engine a->Widget a->Widget a
do_widget_transform ancestry_id engine widget=DF.foldr (\node_id->(intmap_lookup node_id engine.node).widget_transform engine) widget ancestry_id

create_projection_image::Engine a->Projection a->Projection a
create_projection_image engine projection=case projection of
    Without {ancestry_id,object}->With {ancestry_id=ancestry_id,object=object,image=do_widget_transform ancestry_id engine object}
    _->error "create_projection_image: error 1"

create_projection_image_safe::Engine a->Projection a->Projection a
create_projection_image_safe engine projection=case projection of
    Without {ancestry_id,object}->With {ancestry_id=ancestry_id,object=object,image=do_widget_transform ancestry_id engine object}
    With {ancestry_id,object}->With {ancestry_id=ancestry_id,object=object,image=do_widget_transform ancestry_id engine object}

remove_projection_image::Projection a->Projection a
remove_projection_image projection=case projection of
    With {ancestry_id,object}->Without {ancestry_id=ancestry_id,object=object}
    _->error "remove_projection_image: error 1"

remove_projection_image_safe::Projection a->Projection a
remove_projection_image_safe projection=case projection of
    With {ancestry_id,object}->Without {ancestry_id=ancestry_id,object=object}
    _->projection

insert_projection_object::Widget a->Projection a->Projection a
insert_projection_object widget projection=case projection of
    Without {ancestry_id}->Without {ancestry_id=ancestry_id,object=widget}
    With {ancestry_id}->Without {ancestry_id=ancestry_id,object=widget}

update_projection_object::(Widget a->Widget a)->Projection a->Projection a
update_projection_object update projection=case projection of
    Without {ancestry_id,object}->Without {ancestry_id=ancestry_id,object=update object}
    With {ancestry_id,object}->Without {ancestry_id=ancestry_id,object=update object}

functor_update_projection_object::Functor b=>(Widget a->b (Widget a))->Projection a->b (Projection a)
functor_update_projection_object update projection=case projection of
    Without {ancestry_id,object}->fmap (\this_object->Without {ancestry_id=ancestry_id,object=this_object}) (update object)
    With {ancestry_id,object}->fmap (\this_object->Without {ancestry_id=ancestry_id,object=this_object}) (update object)

lookup_projection::Projection_strategy->Projection a->Widget a
lookup_projection projection_strategy=case projection_strategy of
    Object_strategy->lookup_projection_object
    Image_strategy->lookup_projection_image
    Image_safe_strategy->lookup_projection_image_safe

lookup_projection_object::Projection a->Widget a
lookup_projection_object projection=case projection of
    Without {object}->object
    With {object}->object

lookup_projection_image::Projection a->Widget a
lookup_projection_image projection=case projection of
    With {image}->image
    _->error "lookup_projection_image: error 1"

lookup_projection_image_safe::Projection a->Widget a
lookup_projection_image_safe projection=case projection of
    Without {object}->object
    With {image}->image

lookup_projection_widget::Projection_path->Engine a->Widget a
lookup_projection_widget projection_path engine=case projection_path of
    Object_path {leaf_id}->lookup_projection_object (intmap_lookup leaf_id engine.leaf)
    Image_path {leaf_id}->lookup_projection_image (intmap_lookup leaf_id engine.leaf)
    Image_safe_path {leaf_id}->lookup_projection_image_safe (intmap_lookup leaf_id engine.leaf)

update_lookup_projection_widget::Projection_path->(Widget a->Widget a)->Engine a->(Engine a,Widget a)
update_lookup_projection_widget projection_path update engine=case projection_path of
    Object_path {leaf_id}->let (widget,leaf)=intmap_functor_update leaf_id (DT.swap . update_lookup_projection_widget_a update) engine.leaf in (engine {leaf=leaf},widget)
    Image_path {leaf_id}->(engine,lookup_projection_image (intmap_lookup leaf_id engine.leaf))
    Image_safe_path {leaf_id}->(engine,lookup_projection_image_safe (intmap_lookup leaf_id engine.leaf))

update_lookup_projection_widget_a::(Widget a->Widget a)->Projection a->(Projection a,Widget a)
update_lookup_projection_widget_a update projection=case projection of
    Without {ancestry_id,object}->(Without {ancestry_id=ancestry_id,object=update object},object)
    With {ancestry_id,object}->(Without {ancestry_id=ancestry_id,object=update object},object)

functor_lookup_projection_widget::Functor b=>Projection_path->(Widget a->b (Widget a))->Engine a->b (Engine a)
functor_lookup_projection_widget projection_path update engine=case projection_path of
    Object_path {leaf_id}->fmap (\leaf->engine {leaf=leaf}) (intmap_functor_update leaf_id (functor_update_projection_object update) engine.leaf)
    _->fmap (const engine) (update (lookup_projection_widget projection_path engine))