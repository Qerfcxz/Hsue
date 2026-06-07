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
    Active_widget->engine {active=intmap_update widget_id (ancestry_update_backup_active (`insert_backup` engine)) engine.active}
    Free_widget->engine {free=intmap_update widget_id (ancestry_update_backup_free (`insert_backup` engine)) engine.free}
    Bound_widget->engine {bound=intmap_update widget_id (ancestry_update_backup_bound (`insert_backup` engine)) engine.bound}

safe_create_backup::Widget_type->Int->Engine a->Engine a
safe_create_backup widget_type widget_id engine=case widget_type of
    Active_widget->engine {active=DIM.adjust (ancestry_update_backup_active (`safe_insert_backup` engine)) widget_id engine.active}
    Free_widget->engine {free=DIM.adjust (ancestry_update_backup_free (`safe_insert_backup` engine)) widget_id engine.free}
    Bound_widget->engine {bound=DIM.adjust (ancestry_update_backup_bound (`safe_insert_backup` engine)) widget_id engine.bound}

remove_backup::Widget_type->Int->Engine a->Engine a
remove_backup widget_type widget_id engine=case widget_type of
    Active_widget->engine {active=intmap_update widget_id (update_backup_active backup_remove) engine.active}
    Free_widget->engine {free=intmap_update widget_id (update_backup_free backup_remove) engine.free}
    Bound_widget->engine {bound=intmap_update widget_id (update_backup_bound backup_remove) engine.bound}

safe_remove_backup::Widget_type->Int->Engine a->Engine a
safe_remove_backup widget_type widget_id engine=case widget_type of
    Active_widget->engine {active=DIM.adjust (update_backup_active safe_backup_remove) widget_id engine.active}
    Free_widget->engine {free=DIM.adjust (update_backup_free safe_backup_remove) widget_id engine.free}
    Bound_widget->engine {bound=DIM.adjust (update_backup_bound safe_backup_remove) widget_id engine.bound}

update_backup_active::(Backup (Widget a)->Backup (Widget a))->Active a->Active a
update_backup_active update active=case active of
    Active {next,ancestry,backup}->Active {next=next,ancestry=ancestry,backup=update backup}

update_backup_free::(Backup (Widget a)->Backup (Widget a))->Free a->Free a
update_backup_free update free=case free of
    Free {ancestry,backup}->Free {ancestry=ancestry,backup=update backup}

update_backup_bound::(Backup (Widget a)->Backup (Widget a))->Bound a->Bound a
update_backup_bound update bound=case bound of
    Bound {window_id,ancestry,backup}->Bound {window_id=window_id,ancestry=ancestry,backup=update backup}

ancestry_update_backup_active::(DS.Seq Int->Backup (Widget a)->Backup (Widget a))->Active a->Active a
ancestry_update_backup_active update active=case active of
    Active {next,ancestry,backup}->Active {next=next,ancestry=ancestry,backup=update ancestry backup}

ancestry_update_backup_free::(DS.Seq Int->Backup (Widget a)->Backup (Widget a))->Free a->Free a
ancestry_update_backup_free update free=case free of
    Free {ancestry,backup}->Free {ancestry=ancestry,backup=update ancestry backup}

ancestry_update_backup_bound::(DS.Seq Int->Backup (Widget a)->Backup (Widget a))->Bound a->Bound a
ancestry_update_backup_bound update bound=case bound of
    Bound {window_id,ancestry,backup}->Bound {window_id=window_id,ancestry=ancestry,backup=update ancestry backup}

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
backup_lookup backup_strategy=case backup_strategy of
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
backup_update backup_strategy=case backup_strategy of
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

path_lookup_backup_bound::Backup_path->DIM.IntMap (Bound a)->Widget a
path_lookup_backup_bound backup_path bound=case backup_path of
    One_path {backup_id}->backup_lookup_one (intmap_lookup backup_id bound).backup
    Two_path {backup_id}->backup_lookup_two (intmap_lookup backup_id bound).backup
    Safe_two_path {backup_id}->backup_lookup_safe_two (intmap_lookup backup_id bound).backup

path_update_backup_free::Backup_path->(Widget a->Widget a)->DIM.IntMap (Free a)->DIM.IntMap (Free a)
path_update_backup_free backup_path update free=case backup_path of
    One_path {backup_id}->intmap_update backup_id (update_backup_free (backup_update_one update)) free
    Two_path {backup_id}->intmap_update backup_id (update_backup_free (backup_update_two update)) free
    Safe_two_path {backup_id}->intmap_update backup_id (update_backup_free (backup_update_safe_two update)) free

consume_update_lookup_backup_free::Bool->Backup_path->(Widget a->Widget a)->DIM.IntMap (Free a)->(DIM.IntMap (Free a),Widget a)
consume_update_lookup_backup_free consume backup_path update free=case backup_path of
    One_path {backup_id}->intmap_calculate backup_id (consume_update_lookup_backup_free_one consume update) free
    Two_path {backup_id}->intmap_calculate backup_id (consume_update_lookup_backup_free_two consume update) free
    Safe_two_path {backup_id}->intmap_calculate backup_id (consume_update_lookup_backup_free_safe_two consume update) free

consume_update_lookup_backup_free_one::Bool->(Widget a->Widget a)->Free a->(Free a,Widget a)
consume_update_lookup_backup_free_one consume update free=case free of
    Free {ancestry,backup}->case backup of
        Single {one}->(if consume then Free {ancestry=ancestry,backup=backup {one=update one}} else free,one)
        Double {one}->(if consume then Free {ancestry=ancestry,backup=backup {one=update one}} else free,one)

consume_update_lookup_backup_free_two::Bool->(Widget a->Widget a)->Free a->(Free a,Widget a)
consume_update_lookup_backup_free_two consume update free=case free of
    Free {ancestry,backup}->case backup of
        Double {two}->(if consume then Free {ancestry=ancestry,backup=backup {two=update two}} else free,two)
        _->error "consume_update_lookup_backup_free_two: error 1"

consume_update_lookup_backup_free_safe_two::Bool->(Widget a->Widget a)->Free a->(Free a,Widget a)
consume_update_lookup_backup_free_safe_two consume update free=case free of
    Free {ancestry,backup}->case backup of
        Single {one}->(if consume then Free {ancestry=ancestry,backup=backup {one=update one}} else free,one)
        Double {two}->(if consume then Free {ancestry=ancestry,backup=backup {two=update two}} else free,two)