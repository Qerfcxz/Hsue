{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Engine.Projection where

import Engine.Container
import Engine.Type
import qualified Error.Function as EF
import qualified Error.Type as ET
import qualified Data.Foldable as DF
import qualified Data.Sequence as DS
import qualified Data.Tuple as DT

create_image::ET.Has_call_stack=>Bool->Int->Event a->Engine a->Engine a
create_image strict_conflict leaf_id event engine=engine {leaf=int_map_update engine.strict_exist leaf_id (create_projection_image strict_conflict event engine) engine.leaf}

remove_image::ET.Has_call_stack=>Bool->Int->Engine a->Engine a
remove_image strict_exist leaf_id engine=engine {leaf=int_map_update engine.strict_exist leaf_id (remove_projection_image strict_exist) engine.leaf}

do_widget_transform::ET.Has_call_stack=>DS.Seq Int->Event a->Engine a->Widget a->Widget a
do_widget_transform ancestry_id event engine widget=DF.foldr (\node_id->(int_map_lookup node_id engine.node).widget_transform event engine) widget ancestry_id

create_projection_image::ET.Has_call_stack=>Bool->Event a->Engine a->Projection a->Projection a
create_projection_image strict_conflict event engine projection=case projection of
    Without {ancestry_id,object}->With {ancestry_id=ancestry_id,object=object,image=do_widget_transform ancestry_id event engine object}
    With {ancestry_id,object}->if strict_conflict then EF.empty_error else With {ancestry_id=ancestry_id,object=object,image=do_widget_transform ancestry_id event engine object}

remove_projection_image::ET.Has_call_stack=>Bool->Projection a->Projection a
remove_projection_image strict_exist projection=case projection of
    Without {}->if strict_exist then EF.empty_error else projection
    With {ancestry_id,object}->Without {ancestry_id=ancestry_id,object=object}

insert_projection_object::ET.Has_call_stack=>Widget a->Projection a->Projection a
insert_projection_object widget projection=case projection of
    Without {ancestry_id}->Without {ancestry_id=ancestry_id,object=widget}
    With {ancestry_id}->Without {ancestry_id=ancestry_id,object=widget}

update_projection_object::ET.Has_call_stack=>(Widget a->Widget a)->Projection a->Projection a
update_projection_object update projection=case projection of
    Without {ancestry_id,object}->Without {ancestry_id=ancestry_id,object=update object}
    With {ancestry_id,object}->Without {ancestry_id=ancestry_id,object=update object}

functor_update_projection_object::ET.Has_call_stack=>Functor b=>(Widget a->b (Widget a))->Projection a->b (Projection a)
functor_update_projection_object update projection=case projection of
    Without {ancestry_id,object}->fmap (\this_object->Without {ancestry_id=ancestry_id,object=this_object}) (update object)
    With {ancestry_id,object}->fmap (\this_object->Without {ancestry_id=ancestry_id,object=this_object}) (update object)

lookup_projection::ET.Has_call_stack=>Projection_strategy->Projection a->Widget a
lookup_projection projection_strategy=case projection_strategy of
    Object_strategy->lookup_projection_object
    Image_strategy {strict_exist}->lookup_projection_image strict_exist

lookup_projection_object::ET.Has_call_stack=>Projection a->Widget a
lookup_projection_object projection=case projection of
    Without {object}->object
    With {object}->object

lookup_projection_image::ET.Has_call_stack=>Bool->Projection a->Widget a
lookup_projection_image strict_exist projection=case projection of
    Without {object}->if strict_exist then EF.empty_error else object
    With {image}->image

lookup_projection_ancestry_id::ET.Has_call_stack=>Projection a->DS.Seq Int
lookup_projection_ancestry_id projection=case projection of
    Without {ancestry_id}->ancestry_id
    With {ancestry_id}->ancestry_id

lookup_projection_widget::ET.Has_call_stack=>Projection_path->Engine a->Widget a
lookup_projection_widget projection_path engine=case projection_path of
    Object_path {leaf_id}->lookup_projection_object (int_map_lookup leaf_id engine.leaf)
    Image_path {leaf_id,strict_exist}->lookup_projection_image strict_exist (int_map_lookup leaf_id engine.leaf)

update_lookup_projection_widget::ET.Has_call_stack=>Projection_path->(Widget a->Widget a)->Engine a->(Engine a,Widget a)
update_lookup_projection_widget projection_path update engine=case projection_path of
    Object_path {leaf_id}->let (widget,leaf)=int_map_functor_update leaf_id (DT.swap . update_lookup_projection_widget_a update) engine.leaf in (engine {leaf=leaf},widget)
    Image_path {leaf_id,strict_exist}->(engine,lookup_projection_image strict_exist (int_map_lookup leaf_id engine.leaf))

update_lookup_projection_widget_a::ET.Has_call_stack=>(Widget a->Widget a)->Projection a->(Projection a,Widget a)
update_lookup_projection_widget_a update projection=case projection of
    Without {ancestry_id,object}->(Without {ancestry_id=ancestry_id,object=update object},object)
    With {ancestry_id,object}->(Without {ancestry_id=ancestry_id,object=update object},object)

functor_lookup_projection_widget::ET.Has_call_stack=>Functor b=>Projection_path->(Widget a->b (Widget a))->Engine a->b (Engine a)
functor_lookup_projection_widget projection_path update engine=case projection_path of
    Object_path {leaf_id}->fmap (\leaf->engine {leaf=leaf}) (int_map_functor_update leaf_id (functor_update_projection_object update) engine.leaf)
    Image_path {}->fmap (const engine) (update (lookup_projection_widget projection_path engine))

lookup_move_leaf_id::ET.Has_call_stack=>Projection_move->Int
lookup_move_leaf_id projection_move=case projection_move of
    Object_move {leaf_id}->leaf_id
    Image_move {leaf_id}->leaf_id

{-# INLINE create_image #-}
{-# INLINE remove_image #-}
{-# INLINE do_widget_transform #-}
{-# INLINE create_projection_image #-}
{-# INLINE remove_projection_image #-}
{-# INLINE insert_projection_object #-}
{-# INLINE update_projection_object #-}
{-# INLINE functor_update_projection_object #-}
{-# INLINE lookup_projection #-}
{-# INLINE lookup_projection_object #-}
{-# INLINE lookup_projection_image #-}
{-# INLINE lookup_projection_ancestry_id #-}
{-# INLINE lookup_projection_widget #-}
{-# INLINE update_lookup_projection_widget #-}
{-# INLINE update_lookup_projection_widget_a #-}
{-# INLINE functor_lookup_projection_widget #-}
{-# INLINE lookup_move_leaf_id #-}