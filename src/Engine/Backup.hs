{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Engine.Backup where

import Engine.Other
import Engine.Type
import qualified Data.Foldable as DF
import qualified Data.Sequence as DS

create_backup::Widget_type->Int->Engine a->Engine a
create_backup widget_type widget_id engine=case widget_type of
    Active_widget->engine {active=intmap_update widget_id (\Active {next,ancestry,backup}->Active {next=next,ancestry=ancestry,backup=backup_insert_widget ancestry engine backup}) engine.active}
    Free_widget->engine {free=intmap_update widget_id (\Free {ancestry,backup}->Free {ancestry=ancestry,backup=backup_insert_widget ancestry engine backup}) engine.free}
    Bound_widget->engine {bound=intmap_update widget_id (\Bound {window_id,ancestry,backup}->Bound {window_id=window_id,ancestry=ancestry,backup=backup_insert_widget ancestry engine backup}) engine.bound}

do_widget_transform::DS.Seq Int->Engine a->Widget a->Widget a
do_widget_transform ancestry engine widget=DF.foldr (\node_id->(intmap_lookup node_id engine.node).widget_transform engine) widget ancestry

backup_insert_widget::DS.Seq Int->Engine a->Backup (Widget a)->Backup (Widget a)
backup_insert_widget ancestry engine backup=case backup of
    Single {one}->Double {one=one,two=do_widget_transform ancestry engine one}
    _->error "backup_insert_widget: error 1"

backup_lookup::Backup_strategy->Backup a->a
backup_lookup strategy backup=case strategy of
    One->case backup of
        Single {one}->one
        Double {one}->one
    Two->case backup of
        Double {two}->two
        _->error "backup_lookup: error 1"
    Safe_two->case backup of
        Single {one}->one
        Double {two}->two

backup_update::Backup_strategy->(a->a)->Backup a->Backup a
backup_update strategy update backup=case strategy of
    One->case backup of
        Single {one}->Single {one=update one}
        Double {one,two}->Double {one=update one,two=two}
    Two->case backup of
        Double {one,two}->Double {one=one,two=update two}
        _->error "backup_update: error 1"
    Safe_two->case backup of
        Single {one}->Single {one=update one}
        Double {one,two}->Double {one=one,two=update two}

backup_update_lookup_whether::Bool->Backup_strategy->(a->a)->Backup a->(Backup a,a)
backup_update_lookup_whether consume strategy update backup=case strategy of
    One->case backup of
        Single {one}->(if consume then Single {one=update one} else backup,one)
        Double {one,two}->(if consume then Double {one=update one,two=two} else backup,one)
    Two->case backup of
        Double {one,two}->(if consume then Double {one=one,two=update two} else backup,two)
        _->error "backup_update_lookup_whether: error 1"
    Safe_two->case backup of
        Single {one}->(if consume then Single {one=update one} else backup,one)
        Double {one,two}->(if consume then Double {one=one,two=update two} else backup,two)