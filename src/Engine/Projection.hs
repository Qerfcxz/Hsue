{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Engine.Projection where

import Engine.Container
import Engine.Type
import qualified Error.Error as EE
import qualified Data.Foldable as DF
import qualified Data.IntMap as DIM
import qualified Data.Sequence as DS
import qualified Data.Tuple as DT

create_image::Int->Event a->Engine b a c d e->Engine b a c d e
create_image leaf_id event engine=engine {leaf=intmap_update leaf_id (create_projection_image event engine) engine.leaf}

create_image_safe::Int->Event a->Engine b a c d e->Engine b a c d e
create_image_safe leaf_id event engine=engine {leaf=DIM.adjust (create_projection_image_safe event engine) leaf_id engine.leaf}

remove_image::Int->Engine a b c d e->Engine a b c d e
remove_image leaf_id engine=engine {leaf=intmap_update leaf_id remove_projection_image engine.leaf}

remove_image_safe::Int->Engine a b c d e->Engine a b c d e
remove_image_safe leaf_id engine=engine {leaf=DIM.adjust remove_projection_image_safe leaf_id engine.leaf}

do_widget_transform::DS.Seq Int->Event a->Engine b a c d e->Widget b a c d e->Widget b a c d e
do_widget_transform ancestry_id event engine widget=DF.foldr (\node_id->(intmap_lookup node_id engine.node).widget_transform event engine) widget ancestry_id

create_projection_image::Event a->Engine b a c d e->Projection b a c d e->Projection b a c d e
create_projection_image event engine projection=case projection of
    Without {ancestry_id,object}->With {ancestry_id=ancestry_id,object=object,image=do_widget_transform ancestry_id event engine object}
    _->EE.quick_error "create_projection_image" 0

create_projection_image_safe::Event a->Engine b a c d e->Projection b a c d e->Projection b a c d e
create_projection_image_safe event engine projection=case projection of
    Without {ancestry_id,object}->With {ancestry_id=ancestry_id,object=object,image=do_widget_transform ancestry_id event engine object}
    With {ancestry_id,object}->With {ancestry_id=ancestry_id,object=object,image=do_widget_transform ancestry_id event engine object}

remove_projection_image::Projection a b c d e->Projection a b c d e
remove_projection_image projection=case projection of
    With {ancestry_id,object}->Without {ancestry_id=ancestry_id,object=object}
    _->EE.quick_error "remove_projection_image" 0

remove_projection_image_safe::Projection a b c d e->Projection a b c d e
remove_projection_image_safe projection=case projection of
    With {ancestry_id,object}->Without {ancestry_id=ancestry_id,object=object}
    _->projection

insert_projection_object::Widget a b c d e->Projection a b c d e->Projection a b c d e
insert_projection_object widget projection=case projection of
    Without {ancestry_id}->Without {ancestry_id=ancestry_id,object=widget}
    With {ancestry_id}->Without {ancestry_id=ancestry_id,object=widget}

update_projection_object::(Widget a b c d e->Widget a b c d e)->Projection a b c d e->Projection a b c d e
update_projection_object update projection=case projection of
    Without {ancestry_id,object}->Without {ancestry_id=ancestry_id,object=update object}
    With {ancestry_id,object}->Without {ancestry_id=ancestry_id,object=update object}

functor_update_projection_object::Functor f=>(Widget a b c d e->f (Widget a b c d e))->Projection a b c d e->f (Projection a b c d e)
functor_update_projection_object update projection=case projection of
    Without {ancestry_id,object}->fmap (\this_object->Without {ancestry_id=ancestry_id,object=this_object}) (update object)
    With {ancestry_id,object}->fmap (\this_object->Without {ancestry_id=ancestry_id,object=this_object}) (update object)

lookup_projection::Projection_strategy->Projection a b c d e->Widget a b c d e
lookup_projection projection_strategy=case projection_strategy of
    Object_strategy->lookup_projection_object
    Image_strategy->lookup_projection_image
    Image_safe_strategy->lookup_projection_image_safe

lookup_projection_object::Projection a b c d e->Widget a b c d e
lookup_projection_object projection=case projection of
    Without {object}->object
    With {object}->object

lookup_projection_image::Projection a b c d e->Widget a b c d e
lookup_projection_image projection=case projection of
    With {image}->image
    _->EE.quick_error "lookup_projection_image" 0

lookup_projection_image_safe::Projection a b c d e->Widget a b c d e
lookup_projection_image_safe projection=case projection of
    Without {object}->object
    With {image}->image

lookup_projection_widget::Projection_path->Engine a b c d e->Widget a b c d e
lookup_projection_widget projection_path engine=case projection_path of
    Object_path {leaf_id}->lookup_projection_object (intmap_lookup leaf_id engine.leaf)
    Image_path {leaf_id}->lookup_projection_image (intmap_lookup leaf_id engine.leaf)
    Image_safe_path {leaf_id}->lookup_projection_image_safe (intmap_lookup leaf_id engine.leaf)

update_lookup_projection_widget::Projection_path->(Widget a b c d e->Widget a b c d e)->Engine a b c d e->(Engine a b c d e,Widget a b c d e)
update_lookup_projection_widget projection_path update engine=case projection_path of
    Object_path {leaf_id}->let (widget,leaf)=intmap_functor_update leaf_id (DT.swap . update_lookup_projection_widget_a update) engine.leaf in (engine {leaf=leaf},widget)
    Image_path {leaf_id}->(engine,lookup_projection_image (intmap_lookup leaf_id engine.leaf))
    Image_safe_path {leaf_id}->(engine,lookup_projection_image_safe (intmap_lookup leaf_id engine.leaf))

update_lookup_projection_widget_a::(Widget a b c d e->Widget a b c d e)->Projection a b c d e->(Projection a b c d e,Widget a b c d e)
update_lookup_projection_widget_a update projection=case projection of
    Without {ancestry_id,object}->(Without {ancestry_id=ancestry_id,object=update object},object)
    With {ancestry_id,object}->(Without {ancestry_id=ancestry_id,object=update object},object)

functor_lookup_projection_widget::Functor f=>Projection_path->(Widget a b c d e->f (Widget a b c d e))->Engine a b c d e->f (Engine a b c d e)
functor_lookup_projection_widget projection_path update engine=case projection_path of
    Object_path {leaf_id}->fmap (\leaf->engine {leaf=leaf}) (intmap_functor_update leaf_id (functor_update_projection_object update) engine.leaf)
    _->fmap (const engine) (update (lookup_projection_widget projection_path engine))

{-# INLINE create_image #-}
{-# INLINE create_image_safe #-}
{-# INLINE remove_image #-}
{-# INLINE remove_image_safe #-}
{-# INLINE do_widget_transform #-}
{-# INLINE create_projection_image #-}
{-# INLINE create_projection_image_safe #-}
{-# INLINE remove_projection_image #-}
{-# INLINE remove_projection_image_safe #-}
{-# INLINE insert_projection_object #-}
{-# INLINE update_projection_object #-}
{-# INLINE functor_update_projection_object #-}
{-# INLINE lookup_projection #-}
{-# INLINE lookup_projection_object #-}
{-# INLINE lookup_projection_image #-}
{-# INLINE lookup_projection_image_safe #-}
{-# INLINE lookup_projection_widget #-}
{-# INLINE update_lookup_projection_widget #-}
{-# INLINE update_lookup_projection_widget_a #-}
{-# INLINE functor_lookup_projection_widget #-}