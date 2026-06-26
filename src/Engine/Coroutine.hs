{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Engine.Coroutine where

import Engine.Container
import Engine.Type
import qualified Data.IntMap as DIM
import qualified Data.IntSet as DIS
import qualified Data.Sequence as DS

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
    Clone {int,coroutine}->let new_int=int-1 in if new_int<0 then (linear_coroutine,int_index,code_index) else let (new_linear_coroutine,new_int_index,new_code_index)=from_coroutine_a coroutine (int*clone_number) (code_index+1) (int_index+clone_number) linear_coroutine in (DIM.insert new_code_index (Linear_clone_kill {int_index=int_index,clone_number=clone_number}) (DIM.insert code_index (Linear_clone {int_index=int_index,clone_number=clone_number,int=new_int}) new_linear_coroutine),new_int_index,new_code_index+1)
    Then {first_coroutine,second_coroutine}->let (first_linear_coroutine,first_int_index,first_code_index)=from_coroutine_a first_coroutine clone_number code_index int_index linear_coroutine in let (second_linear_coroutine,second_int_index,second_code_index)=from_coroutine_a second_coroutine clone_number first_code_index int_index first_linear_coroutine in (second_linear_coroutine,max first_int_index second_int_index,second_code_index)
    If {dynamic_bool,first_coroutine,second_coroutine}->let (first_linear_coroutine,first_int_index,first_code_index)=from_coroutine_a first_coroutine clone_number (code_index+1) int_index linear_coroutine in let new_code_index=first_code_index+1 in let (second_linear_coroutine,second_int_index,second_code_index)=from_coroutine_a second_coroutine clone_number new_code_index int_index first_linear_coroutine in (DIM.insert first_code_index (Linear_jump {code_index=second_code_index}) (DIM.insert code_index (Linear_false_jump {code_index=new_code_index,dynamic_bool=dynamic_bool}) second_linear_coroutine),max first_int_index second_int_index,second_code_index)

fork_coroutine::DS.Seq (Coroutine a)->DIS.IntSet->Int->Int->Int->Int->DIM.IntMap (Linear_coroutine a)->(DIM.IntMap (Linear_coroutine a),Int,Int)
fork_coroutine multiple_coroutine jump_code_index fork_code_index clone_number code_index int_index linear_coroutine=case multiple_coroutine of
    DS.Empty->let new_code_index=code_index-1 in (DIS.foldl' (\this_linear_coroutine this_code_index->DIM.insert this_code_index (Linear_jump {code_index=new_code_index}) this_linear_coroutine) linear_coroutine jump_code_index,int_index,new_code_index)
    coroutine DS.:<| other_coroutine->let (new_linear_coroutine,new_int_index,new_code_index)=from_coroutine_a coroutine clone_number code_index int_index linear_coroutine in fork_coroutine other_coroutine (DIS.insert (code_index-1) jump_code_index) (fork_code_index+1) clone_number (new_code_index+1) new_int_index (DIM.insert fork_code_index (Linear_fork {code_index=code_index}) new_linear_coroutine)

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
    Linear_clone_kill {int_index,clone_number}->let (new_variable,int)=intmap_update_lookup (int_index+mod clone_index clone_number) (\this_int->this_int-1) variable in if 0<int then run_coroutine_b linear_coroutine survived_progress newborn_progress progress new_variable engine widget else run_coroutine_a linear_coroutine clone_index (this_code_index+1) survived_progress newborn_progress progress new_variable engine widget
    Linear_dynamic_int {int_index,dynamic_int}->run_coroutine_a linear_coroutine clone_index (this_code_index+1) survived_progress newborn_progress progress (DIM.insert (int_index+clone_index) (dynamic_int.dynamic_int engine widget) variable) engine widget
    Linear_int {int_index,int}->run_coroutine_a linear_coroutine clone_index (this_code_index+1) survived_progress newborn_progress progress (DIM.insert (int_index+clone_index) int variable) engine widget
    Linear_false_jump {code_index,dynamic_bool}->if dynamic_bool.dynamic_bool engine widget then run_coroutine_a linear_coroutine clone_index (this_code_index+1) survived_progress newborn_progress progress variable engine widget else run_coroutine_a linear_coroutine clone_index code_index survived_progress newborn_progress progress variable engine widget
    Linear_clone {int_index,clone_number,int}->let new_code_index=this_code_index+1 in run_coroutine_a linear_coroutine clone_index new_code_index survived_progress (run_coroutine_c int clone_number (clone_index+clone_number) new_code_index newborn_progress) progress (DIM.insert (int_index+clone_index) int variable) engine widget

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