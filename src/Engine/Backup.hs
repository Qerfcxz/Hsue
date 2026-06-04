{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Engine.Backup where

import Engine.Other
import Engine.Type
import qualified Data.Foldable as DF
import qualified Data.IntMap as DIM
import qualified Data.Sequence as DS

create_backup::Widget_type->Int->Engine a->Engine a
create_backup widget_type widget_id engine=case widget_type of
    Active_widget->engine {active=intmap_update widget_id (\Active {next,ancestry,backup}->Active {next=next,ancestry=ancestry,backup=insert_backup ancestry engine backup}) engine.active}
    Free_widget->engine {free=intmap_update widget_id (\Free {ancestry,backup}->Free {ancestry=ancestry,backup=insert_backup ancestry engine backup}) engine.free}
    Bound_widget->engine {bound=intmap_update widget_id (\Bound {window_id,ancestry,backup}->Bound {window_id=window_id,ancestry=ancestry,backup=insert_backup ancestry engine backup}) engine.bound}

safe_create_backup::Widget_type->Int->Engine a->Engine a
safe_create_backup widget_type widget_id engine=case widget_type of
    Active_widget->engine {active=DIM.adjust (\Active {next,ancestry,backup}->Active {next=next,ancestry=ancestry,backup=safe_insert_backup ancestry engine backup}) widget_id engine.active}
    Free_widget->engine {free=DIM.adjust (\Free {ancestry,backup}->Free {ancestry=ancestry,backup=safe_insert_backup ancestry engine backup}) widget_id engine.free}
    Bound_widget->engine {bound=DIM.adjust (\Bound {window_id,ancestry,backup}->Bound {window_id=window_id,ancestry=ancestry,backup=safe_insert_backup ancestry engine backup}) widget_id engine.bound}

remove_backup::Widget_type->Int->Engine a->Engine a
remove_backup widget_type widget_id engine=case widget_type of
    Active_widget->engine {active=intmap_update widget_id (\Active {next,ancestry,backup}->Active {next=next,ancestry=ancestry,backup=backup_remove backup}) engine.active}
    Free_widget->engine {free=intmap_update widget_id (\Free {ancestry,backup}->Free {ancestry=ancestry,backup=backup_remove backup}) engine.free}
    Bound_widget->engine {bound=intmap_update widget_id (\Bound {window_id,ancestry,backup}->Bound {window_id=window_id,ancestry=ancestry,backup=backup_remove backup}) engine.bound}

safe_remove_backup::Widget_type->Int->Engine a->Engine a
safe_remove_backup widget_type widget_id engine=case widget_type of
    Active_widget->engine {active=DIM.adjust (\Active {next,ancestry,backup}->Active {next=next,ancestry=ancestry,backup=backup_remove backup}) widget_id engine.active}
    Free_widget->engine {free=DIM.adjust (\Free {ancestry,backup}->Free {ancestry=ancestry,backup=backup_remove backup}) widget_id engine.free}
    Bound_widget->engine {bound=DIM.adjust (\Bound {window_id,ancestry,backup}->Bound {window_id=window_id,ancestry=ancestry,backup=backup_remove backup}) widget_id engine.bound}

do_widget_transform::DS.Seq Int->Engine a->Widget a->Widget a
do_widget_transform ancestry engine widget=DF.foldr (\node_id->(intmap_lookup node_id engine.node).widget_transform engine) widget ancestry

insert_backup::DS.Seq Int->Engine a->Backup (Widget a)->Backup (Widget a)
insert_backup ancestry engine backup=case backup of
    Single {one}->Double {one=one,two=do_widget_transform ancestry engine one}
    _->error "insert_backup: error 1"

safe_insert_backup::DS.Seq Int->Engine a->Backup (Widget a)->Backup (Widget a)
safe_insert_backup ancestry engine backup=case backup of
    Single {one}->Double {one=one,two=do_widget_transform ancestry engine one}
    Double {one}->Double {one=one,two=do_widget_transform ancestry engine one}

backup_remove::Backup a->Backup a
backup_remove backup=case backup of
    Double {one}->Single {one=one}
    _->error "backup_remove: error 1"

safe_backup_remove::Backup a->Backup a
safe_backup_remove backup=case backup of
    Double {one}->Single {one=one}
    _->backup

backup_lookup::Backup_strategy->Backup a->a
backup_lookup strategy=case strategy of
    One->backup_lookup_one
    Two->backup_lookup_two
    Safe_two->backup_lookup_safe_two

backup_lookup_one::Backup a->a
backup_lookup_one backup=case backup of
    Single {one}->one
    Double {one}->one

backup_lookup_two::Backup a->a
backup_lookup_two backup=case backup of
    Double {two}->two
    _->error "backup_lookup_two: error 1"

backup_lookup_safe_two::Backup a->a
backup_lookup_safe_two backup=case backup of
    Single {one}->one
    Double {two}->two

backup_update::Backup_strategy->(a->a)->Backup a->Backup a
backup_update strategy=case strategy of
    One->backup_update_one
    Two->backup_update_two
    Safe_two->backup_update_safe_two

backup_update_one::(a->a)->Backup a->Backup a
backup_update_one update backup=case backup of
    Single {one}->Single {one=update one}
    Double {one,two}->Double {one=update one,two=two}

backup_update_two::(a->a)->Backup a->Backup a
backup_update_two update backup=case backup of
    Double {one,two}->Double {one=one,two=update two}
    _->error "backup_update_two: error 1"

backup_update_safe_two::(a->a)->Backup a->Backup a
backup_update_safe_two update backup=case backup of
    Single {one}->Single {one=update one}
    Double {one,two}->Double {one=one,two=update two}

lookup_bound_backup::Backup_path->DIM.IntMap (Bound a)->Widget a
lookup_bound_backup backup_path bound=case backup_path of
    One_path {backup_id}->backup_lookup_one (intmap_lookup backup_id bound).backup
    Two_path {backup_id}->backup_lookup_two (intmap_lookup backup_id bound).backup
    Safe_two_path {backup_id}->backup_lookup_safe_two (intmap_lookup backup_id bound).backup

update_free_backup::Backup_path->(Widget a->Widget a)->DIM.IntMap (Free a)->DIM.IntMap (Free a)
update_free_backup backup_path update free=case backup_path of
    One_path {backup_id}->intmap_update backup_id (update_free_backup_a (backup_update_one update)) free
    Two_path {backup_id}->intmap_update backup_id (update_free_backup_a (backup_update_two update)) free
    Safe_two_path {backup_id}->intmap_update backup_id (update_free_backup_a (backup_update_safe_two update)) free

update_free_backup_a::(Backup (Widget a)->Backup (Widget a))->Free a->Free a
update_free_backup_a update free=case free of
    Free {ancestry,backup}->Free {ancestry=ancestry,backup=update backup}

whether_update_lookup_free_backup::Bool->Backup_path->(Widget a->Widget a)->DIM.IntMap (Free a)->(DIM.IntMap (Free a),Widget a)
whether_update_lookup_free_backup whether backup_path update free=case backup_path of
    One_path {backup_id}->intmap_calculate backup_id (whether_update_lookup_free_backup_one whether update) free
    Two_path {backup_id}->intmap_calculate backup_id (whether_update_lookup_free_backup_two whether update) free
    Safe_two_path {backup_id}->intmap_calculate backup_id (whether_update_lookup_free_backup_safe_two whether update) free

whether_update_lookup_free_backup_one::Bool->(Widget a->Widget a)->Free a->(Free a,Widget a)
whether_update_lookup_free_backup_one whether update free=case free of
    Free {ancestry,backup}->case backup of
        Single {one}->(if whether then Free {ancestry=ancestry,backup=backup {one=update one}} else free,one)
        Double {one}->(if whether then Free {ancestry=ancestry,backup=backup {one=update one}} else free,one)

whether_update_lookup_free_backup_two::Bool->(Widget a->Widget a)->Free a->(Free a,Widget a)
whether_update_lookup_free_backup_two whether update free=case free of
    Free {ancestry,backup}->case backup of
        Double {two}->(if whether then Free {ancestry=ancestry,backup=backup {two=update two}} else free,two)
        _->error "whether_update_lookup_free_backup_two: error 1"

whether_update_lookup_free_backup_safe_two::Bool->(Widget a->Widget a)->Free a->(Free a,Widget a)
whether_update_lookup_free_backup_safe_two whether update free=case free of
    Free {ancestry,backup}->case backup of
        Single {one}->(if whether then Free {ancestry=ancestry,backup=backup {one=update one}} else free,one)
        Double {two}->(if whether then Free {ancestry=ancestry,backup=backup {two=update two}} else free,two)