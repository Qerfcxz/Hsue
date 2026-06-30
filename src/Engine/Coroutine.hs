{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Engine.Coroutine where

import Engine.Container
import Engine.Type
import qualified Data.IntMap as DIM
import qualified Data.IntSet as DIS
import qualified Data.Sequence as DS

init_coroutine_state::Widget a->Coroutine_state a
init_coroutine_state widget=Coroutine_state {widget=widget,variable=DIM.empty,program_counter=DS.singleton (Program_counter {code_index=0,clone_index=0})}

to_coroutine::Serial_coroutine a ()->Coroutine a
to_coroutine serial_coroutine=serial_coroutine.coroutine

lift_coroutine::Coroutine a->Serial_coroutine a ()
lift_coroutine coroutine=Serial_coroutine {coroutine=coroutine,value=()}

do_empty::Parallel_coroutine a ()
do_empty=Parallel_coroutine {multiple_coroutine=DS.empty,value=()}

do_done::Serial_coroutine a ()
do_done=lift_coroutine Done

do_emit::(Engine a->Widget a->(Widget a,Engine a))->Serial_coroutine a ()
do_emit emit=lift_coroutine (Emit {emit=emit})

do_wait::Dynamic_int a->Serial_coroutine a ()
do_wait dynamic_int=lift_coroutine (Wait {dynamic_int=dynamic_int})

do_forever::Serial_coroutine a ()->Serial_coroutine a ()
do_forever serial_coroutine=lift_coroutine (Forever {coroutine=serial_coroutine.coroutine})

do_fork::Parallel_coroutine a ()->Serial_coroutine a ()
do_fork parallel_coroutine=lift_coroutine (Fork {multiple_coroutine=parallel_coroutine.multiple_coroutine})

do_while::Dynamic_bool a->Serial_coroutine a ()->Serial_coroutine a ()
do_while dynamic_bool serial_coroutine=lift_coroutine (While {dynamic_bool=dynamic_bool,coroutine=serial_coroutine.coroutine})

do_repeat::Dynamic_int a->Serial_coroutine a ()->Serial_coroutine a ()
do_repeat dynamic_int serial_coroutine=lift_coroutine (Repeat {dynamic_int=dynamic_int,coroutine=serial_coroutine.coroutine})

do_case::Dynamic_int a->Parallel_coroutine a ()->Serial_coroutine a ()
do_case dynamic_int parallel_coroutine=lift_coroutine (Case {dynamic_int=dynamic_int,multiple_coroutine=parallel_coroutine.multiple_coroutine})

do_clone::Int->Serial_coroutine a ()->Serial_coroutine a ()
do_clone int serial_coroutine=lift_coroutine (Clone {int=int,coroutine=serial_coroutine.coroutine})

do_if::Dynamic_bool a->Serial_coroutine a ()->Serial_coroutine a ()->Serial_coroutine a ()
do_if dynamic_bool first_serial_coroutine second_serial_coroutine=lift_coroutine (If {dynamic_bool=dynamic_bool,first_coroutine=first_serial_coroutine.coroutine,second_coroutine=second_serial_coroutine.coroutine})

do_pause::Dynamic_bool a->Serial_coroutine a ()->Serial_coroutine a ()
do_pause dynamic_bool serial_coroutine=lift_coroutine (do_pause_a dynamic_bool serial_coroutine.coroutine)

do_pause_a::Dynamic_bool a->Coroutine a->Coroutine a
do_pause_a this_dynamic_bool this_coroutine=let wait=Wait {dynamic_int=dynamic_int_from_integer 1} in let while=While {dynamic_bool=this_dynamic_bool,coroutine=wait} in case this_coroutine of
    Done->Done
    Emit {}->Then {first_coroutine=while,second_coroutine=this_coroutine}
    Wait {dynamic_int}->Repeat {dynamic_int=dynamic_int,coroutine=Then {first_coroutine=while,second_coroutine=wait}}
    Forever {coroutine}->Forever {coroutine=do_pause_a this_dynamic_bool coroutine}
    Fork {multiple_coroutine}->Fork {multiple_coroutine=fmap (do_pause_a this_dynamic_bool) multiple_coroutine}
    While {dynamic_bool,coroutine}->Then {first_coroutine=while,second_coroutine=While {dynamic_bool=dynamic_bool,coroutine=Then {first_coroutine=do_pause_a this_dynamic_bool coroutine,second_coroutine=while}}}
    Repeat {dynamic_int,coroutine}->Then {first_coroutine=while,second_coroutine=Repeat {dynamic_int=dynamic_int,coroutine=do_pause_a this_dynamic_bool coroutine}}
    Case {dynamic_int,multiple_coroutine}->Then {first_coroutine=while,second_coroutine=Case {dynamic_int=dynamic_int,multiple_coroutine=fmap (do_pause_a this_dynamic_bool) multiple_coroutine}}
    Clone {int,coroutine}->Clone {int=int,coroutine=do_pause_a this_dynamic_bool coroutine}
    Then {first_coroutine,second_coroutine}->Then {first_coroutine=do_pause_a this_dynamic_bool first_coroutine,second_coroutine=do_pause_a this_dynamic_bool second_coroutine}
    If {dynamic_bool,first_coroutine,second_coroutine}->Then {first_coroutine=while,second_coroutine=If {dynamic_bool=dynamic_bool,first_coroutine=do_pause_a this_dynamic_bool first_coroutine,second_coroutine=do_pause_a this_dynamic_bool second_coroutine}}
    Dynamic_clone {dynamic_int,int,coroutine}->Then {first_coroutine=while,second_coroutine=Dynamic_clone {dynamic_int=dynamic_int,int=int,coroutine=do_pause_a this_dynamic_bool coroutine}}

do_branch::Serial_coroutine a ()->Parallel_coroutine a ()
do_branch serial_coroutine=case serial_coroutine.coroutine of
    Done->Parallel_coroutine {multiple_coroutine=DS.empty,value=()}
    _->Parallel_coroutine {multiple_coroutine=DS.singleton serial_coroutine.coroutine,value=()}

from_coroutine::Coroutine a->(DIM.IntMap (Linear_coroutine a),Int)
from_coroutine coroutine=let (linear_coroutine,int_index,code_index)=from_coroutine_a coroutine 1 0 0 DIM.empty in (DIM.insert code_index Linear_end linear_coroutine,int_index)

from_coroutine_a::Coroutine a->Int->Int->Int->DIM.IntMap (Linear_coroutine a)->(DIM.IntMap (Linear_coroutine a),Int,Int)
from_coroutine_a this_coroutine clone_number code_index int_index linear_coroutine=case this_coroutine of
    Done->(linear_coroutine,int_index,code_index)
    Emit {emit}->(DIM.insert code_index (Linear_emit {emit=emit}) linear_coroutine,int_index,code_index+1)
    Wait {dynamic_int}->(DIM.insert (code_index+1) (Linear_wait {int_index=int_index}) (DIM.insert code_index (Linear_dynamic_int {int_index=int_index,dynamic_int=dynamic_int}) linear_coroutine),int_index+clone_number,code_index+2)
    Forever {coroutine}->let (new_linear_coroutine,new_int_index,new_code_index)=from_coroutine_a coroutine clone_number code_index int_index linear_coroutine in (DIM.insert new_code_index (Linear_jump {code_index=code_index}) new_linear_coroutine,new_int_index,new_code_index+1)
    Fork {multiple_coroutine}->case multiple_coroutine of
        DS.Empty->(linear_coroutine,int_index,code_index)
        coroutine DS.:<| other_coroutine->let new_code_index=code_index+1 in let int=DS.length other_coroutine in let (new_linear_coroutine,new_int_index,new_new_code_index)=from_coroutine_a coroutine clone_number (new_code_index+int) (int_index+clone_number) linear_coroutine in let (final_linear_coroutine,final_int_index,final_code_index)=fork_coroutine other_coroutine DIS.empty new_code_index clone_number (new_new_code_index+1) new_int_index (DIM.insert code_index (Linear_int {int_index=int_index,int=int}) new_linear_coroutine) in (DIM.insert final_code_index (Linear_fork_kill {int_index=int_index}) final_linear_coroutine,final_int_index,final_code_index+1)
    While {dynamic_bool,coroutine}->let (new_linear_coroutine,new_int_index,new_code_index)=from_coroutine_a coroutine clone_number (code_index+1) int_index linear_coroutine in (DIM.insert new_code_index (Linear_jump {code_index=code_index}) (DIM.insert code_index (Linear_false_jump {code_index=new_code_index+1,dynamic_bool=dynamic_bool}) new_linear_coroutine),new_int_index,new_code_index+1)
    Repeat {dynamic_int,coroutine}->let new_code_index=code_index+2 in let (new_linear_coroutine,new_int_index,new_new_code_index)=from_coroutine_a coroutine clone_number new_code_index (int_index+clone_number) linear_coroutine in let new_new_new_code_index=new_new_code_index+1 in (DIM.insert new_new_code_index (Linear_one_more_jump {int_index=int_index,code_index=new_code_index}) (DIM.insert (code_index+1) (Linear_one_less_jump {int_index=int_index,code_index=new_new_new_code_index}) (DIM.insert code_index (Linear_dynamic_int {int_index=int_index,dynamic_int=dynamic_int}) new_linear_coroutine)),new_int_index,new_new_new_code_index)
    Case {dynamic_int,multiple_coroutine}->let coroutine_number=DS.length multiple_coroutine in if coroutine_number==0 then (linear_coroutine,int_index,code_index) else let (new_linear_coroutine,new_int_index,new_code_index,case_code_index,jump_code_index)=case_coroutine multiple_coroutine DIS.empty DIM.empty 0 clone_number (code_index+2) (int_index+clone_number) (int_index+clone_number) linear_coroutine in let (final_linear_coroutine,final_code_index)=build_binary_search case_code_index coroutine_number 0 new_code_index int_index new_linear_coroutine in (DIM.insert (code_index+1) (Linear_jump {code_index=new_code_index}) (DIM.insert code_index (Linear_dynamic_int {int_index=int_index,dynamic_int=dynamic_int}) (DIS.foldl' (\this_linear_coroutine this_code_index->DIM.insert this_code_index (Linear_jump {code_index=final_code_index}) this_linear_coroutine) final_linear_coroutine jump_code_index)),new_int_index,final_code_index)
    Clone {int,coroutine}->let new_int=int-1 in if new_int<0 then (linear_coroutine,int_index,code_index) else let (new_linear_coroutine,new_int_index,new_code_index)=from_coroutine_a coroutine (int*clone_number) (code_index+1) (int_index+clone_number) linear_coroutine in (DIM.insert new_code_index (Linear_clone_kill {int_index=int_index,clone_number=clone_number}) (DIM.insert code_index (Linear_clone {int_index=int_index,clone_number=clone_number,int=new_int}) new_linear_coroutine),new_int_index,new_code_index+1)
    Then {first_coroutine,second_coroutine}->let (first_linear_coroutine,first_int_index,first_code_index)=from_coroutine_a first_coroutine clone_number code_index int_index linear_coroutine in let (second_linear_coroutine,second_int_index,second_code_index)=from_coroutine_a second_coroutine clone_number first_code_index int_index first_linear_coroutine in (second_linear_coroutine,max first_int_index second_int_index,second_code_index)
    If {dynamic_bool,first_coroutine,second_coroutine}->let (first_linear_coroutine,first_int_index,first_code_index)=from_coroutine_a first_coroutine clone_number (code_index+1) int_index linear_coroutine in let new_code_index=first_code_index+1 in let (second_linear_coroutine,second_int_index,second_code_index)=from_coroutine_a second_coroutine clone_number new_code_index int_index first_linear_coroutine in (DIM.insert first_code_index (Linear_jump {code_index=second_code_index}) (DIM.insert code_index (Linear_false_jump {code_index=new_code_index,dynamic_bool=dynamic_bool}) second_linear_coroutine),max first_int_index second_int_index,second_code_index)
    Dynamic_clone {dynamic_int,int,coroutine}->let new_int=int-1 in if new_int<0 then (linear_coroutine,int_index,code_index) else let (new_linear_coroutine,new_int_index,new_code_index)=from_coroutine_a coroutine (int*clone_number) (code_index+1) (int_index+clone_number) linear_coroutine in (DIM.insert new_code_index (Linear_clone_kill {int_index=int_index,clone_number=clone_number}) (DIM.insert code_index (Linear_dynamic_clone {int_index=int_index,code_index=new_code_index+1,clone_number=clone_number,dynamic_int=dynamic_int,int=new_int}) new_linear_coroutine),new_int_index,new_code_index+1)

fork_coroutine::DS.Seq (Coroutine a)->DIS.IntSet->Int->Int->Int->Int->DIM.IntMap (Linear_coroutine a)->(DIM.IntMap (Linear_coroutine a),Int,Int)
fork_coroutine multiple_coroutine jump_code_index fork_code_index clone_number code_index int_index linear_coroutine=case multiple_coroutine of
    DS.Empty->let new_code_index=code_index-1 in (DIS.foldl' (\this_linear_coroutine this_code_index->DIM.insert this_code_index (Linear_jump {code_index=new_code_index}) this_linear_coroutine) linear_coroutine jump_code_index,int_index,new_code_index)
    coroutine DS.:<| other_coroutine->let (new_linear_coroutine,new_int_index,new_code_index)=from_coroutine_a coroutine clone_number code_index int_index linear_coroutine in fork_coroutine other_coroutine (DIS.insert (code_index-1) jump_code_index) (fork_code_index+1) clone_number (new_code_index+1) new_int_index (DIM.insert fork_code_index (Linear_fork {code_index=code_index}) new_linear_coroutine)

case_coroutine::DS.Seq (Coroutine a)->DIS.IntSet->DIM.IntMap Int->Int->Int->Int->Int->Int->DIM.IntMap (Linear_coroutine a)->(DIM.IntMap (Linear_coroutine a),Int,Int,DIM.IntMap Int,DIS.IntSet)
case_coroutine multiple_coroutine jump_code_index case_code_index int clone_number code_index int_index max_int_index linear_coroutine=case multiple_coroutine of
    DS.Empty->(linear_coroutine,max_int_index,code_index,case_code_index,jump_code_index)
    coroutine DS.:<| other_coroutine->let (new_linear_coroutine,new_int_index,new_code_index)=from_coroutine_a coroutine clone_number code_index int_index linear_coroutine in case_coroutine other_coroutine (DIS.insert new_code_index jump_code_index) (DIM.insert int code_index case_code_index) (int+1) clone_number (new_code_index+1) int_index (max new_int_index max_int_index) new_linear_coroutine

build_binary_search::DIM.IntMap Int->Int->Int->Int->Int->DIM.IntMap (Linear_coroutine a)->(DIM.IntMap (Linear_coroutine a),Int)
build_binary_search case_code_index coroutine_number int code_index int_index linear_coroutine=case coroutine_number of
    1->(DIM.insert code_index (Linear_jump {code_index=intmap_lookup int case_code_index}) linear_coroutine,code_index+1)
    _->let half_coroutine_number=div coroutine_number 2 in let new_int=int+half_coroutine_number in let (right_linear_coroutine,right_code_index)=build_binary_search case_code_index (coroutine_number-half_coroutine_number) new_int (code_index+1) int_index linear_coroutine in let (left_linear_coroutine,left_code_index)=build_binary_search case_code_index half_coroutine_number int right_code_index int_index right_linear_coroutine in (DIM.insert code_index (Linear_less_jump {int_index=int_index,code_index=right_code_index,int=new_int}) left_linear_coroutine,left_code_index)

run_coroutine::DIM.IntMap (Linear_coroutine a)->DS.Seq Program_counter->DIM.IntMap Int->Engine a->Widget a->(Widget a,Engine a,DIM.IntMap Int,DS.Seq Program_counter)
run_coroutine linear_coroutine progress variable engine widget=case progress of
    DS.Empty->(widget,engine,variable,DS.Empty)
    (program_counter DS.:<| other_program_counter)->case program_counter of
        Program_counter {code_index,clone_index}->run_coroutine_a linear_coroutine clone_index code_index DS.empty DS.empty other_program_counter variable engine widget

run_coroutine_a::DIM.IntMap (Linear_coroutine a)->Int->Int->DS.Seq Program_counter->DS.Seq Program_counter->DS.Seq Program_counter->DIM.IntMap Int->Engine a->Widget a->(Widget a,Engine a,DIM.IntMap Int,DS.Seq Program_counter)
run_coroutine_a linear_coroutine clone_index this_code_index survived_progress newborn_progress progress variable engine widget=case intmap_lookup this_code_index linear_coroutine of
    Linear_end->run_coroutine_b linear_coroutine survived_progress newborn_progress progress variable engine widget
    Linear_emit {emit}->let (new_widget,new_engine)=emit engine widget in run_coroutine_a linear_coroutine clone_index (this_code_index+1) survived_progress newborn_progress progress variable new_engine new_widget
    Linear_wait {int_index}->let (new_variable,int)=intmap_update_lookup (int_index+clone_index) (\this_int->this_int-1) variable in if 0<int then run_coroutine_b linear_coroutine (survived_progress DS.|> Program_counter {code_index=this_code_index,clone_index=clone_index}) newborn_progress progress new_variable engine widget else run_coroutine_a linear_coroutine clone_index (this_code_index+1) survived_progress newborn_progress progress new_variable engine widget
    Linear_fork_kill {int_index}->let (new_variable,int)=intmap_update_lookup (int_index+clone_index) (\this_int->this_int-1) variable in if 0<int then run_coroutine_b linear_coroutine survived_progress newborn_progress progress new_variable engine widget else run_coroutine_a linear_coroutine clone_index (this_code_index+1) survived_progress newborn_progress progress new_variable engine widget
    Linear_fork {code_index}->run_coroutine_a linear_coroutine clone_index (this_code_index+1) survived_progress (newborn_progress DS.|> Program_counter {code_index=code_index,clone_index=clone_index}) progress variable engine widget
    Linear_jump {code_index}->run_coroutine_a linear_coroutine clone_index code_index survived_progress newborn_progress progress variable engine widget
    Linear_one_less_jump {int_index,code_index}->if intmap_lookup (int_index+clone_index) variable<1 then run_coroutine_a linear_coroutine clone_index code_index survived_progress newborn_progress progress variable engine widget else run_coroutine_a linear_coroutine clone_index (this_code_index+1) survived_progress newborn_progress progress variable engine widget
    Linear_one_more_jump {int_index,code_index}->let (new_variable,int)=intmap_update_lookup (int_index+clone_index) (\this_int->this_int-1) variable in if 1<int then run_coroutine_a linear_coroutine clone_index code_index survived_progress newborn_progress progress new_variable engine widget else run_coroutine_a linear_coroutine clone_index (this_code_index+1) survived_progress newborn_progress progress new_variable engine widget
    Linear_clone_kill {int_index,clone_number}->let new_clone_index=mod clone_index clone_number in let (new_variable,int)=intmap_update_lookup (int_index+new_clone_index) (\this_int->this_int-1) variable in if 0<int then run_coroutine_b linear_coroutine survived_progress newborn_progress progress new_variable engine widget else run_coroutine_a linear_coroutine new_clone_index (this_code_index+1) survived_progress newborn_progress progress new_variable engine widget
    Linear_dynamic_int {int_index,dynamic_int}->run_coroutine_a linear_coroutine clone_index (this_code_index+1) survived_progress newborn_progress progress (DIM.insert (int_index+clone_index) (dynamic_int.dynamic_int engine widget) variable) engine widget
    Linear_int {int_index,int}->run_coroutine_a linear_coroutine clone_index (this_code_index+1) survived_progress newborn_progress progress (DIM.insert (int_index+clone_index) int variable) engine widget
    Linear_false_jump {code_index,dynamic_bool}->if dynamic_bool.dynamic_bool engine widget then run_coroutine_a linear_coroutine clone_index (this_code_index+1) survived_progress newborn_progress progress variable engine widget else run_coroutine_a linear_coroutine clone_index code_index survived_progress newborn_progress progress variable engine widget
    Linear_less_jump {int_index,code_index,int}->if intmap_lookup (int_index+clone_index) variable<int then run_coroutine_a linear_coroutine clone_index code_index survived_progress newborn_progress progress variable engine widget else run_coroutine_a linear_coroutine clone_index (this_code_index+1) survived_progress newborn_progress progress variable engine widget
    Linear_clone {int_index,clone_number,int}->let new_code_index=this_code_index+1 in run_coroutine_a linear_coroutine clone_index new_code_index survived_progress (run_coroutine_c int clone_number (clone_index+clone_number) new_code_index newborn_progress) progress (DIM.insert (int_index+clone_index) int variable) engine widget
    Linear_dynamic_clone {int_index,code_index,clone_number,dynamic_int,int}->let new_int=dynamic_int.dynamic_int engine widget-1 in if new_int<0 then run_coroutine_a linear_coroutine clone_index code_index survived_progress newborn_progress progress variable engine widget else if int<new_int then error "run_coroutine_a: error 1" else let new_code_index=this_code_index+1 in run_coroutine_a linear_coroutine clone_index new_code_index survived_progress (run_coroutine_c new_int clone_number (clone_index+clone_number) new_code_index newborn_progress) progress (DIM.insert (int_index+clone_index) new_int variable) engine widget

run_coroutine_b::DIM.IntMap (Linear_coroutine a)->DS.Seq Program_counter->DS.Seq Program_counter->DS.Seq Program_counter->DIM.IntMap Int->Engine a->Widget a->(Widget a,Engine a,DIM.IntMap Int,DS.Seq Program_counter)
run_coroutine_b linear_coroutine survived_progress newborn_progress progress variable engine widget=case newborn_progress of
    DS.Empty->case progress of
        DS.Empty->(widget,engine,variable,survived_progress)
        (program_counter DS.:<| other_program_counter)->case program_counter of
            Program_counter {code_index,clone_index}->run_coroutine_a linear_coroutine clone_index code_index survived_progress DS.Empty other_program_counter variable engine widget
    (program_counter DS.:<| other_program_counter)->case program_counter of
        Program_counter {code_index,clone_index}->run_coroutine_a linear_coroutine clone_index code_index survived_progress DS.Empty (other_program_counter DS.>< progress) variable engine widget

run_coroutine_c::Int->Int->Int->Int->DS.Seq Program_counter->DS.Seq Program_counter
run_coroutine_c int clone_number clone_index code_index newborn_progress=if int<1 then newborn_progress else run_coroutine_c (int-1) clone_number (clone_index+clone_number) code_index (newborn_progress DS.|> Program_counter {code_index=code_index,clone_index=clone_index})