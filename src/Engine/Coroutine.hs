{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Engine.Coroutine where

import Engine.Container
import Engine.Helper
import Engine.Projection
import Engine.Type
import qualified Error.Error as EE
import qualified Control.Monad.ST as CMST
import qualified Data.Foldable as DF
import qualified Data.IntMap as DIM
import qualified Data.IntSet as DIS
import qualified Data.Sequence as DS
import qualified Data.Tuple as DT
import qualified Data.Vector as DV
import qualified Data.Vector.Storable as DVS
import qualified Data.Vector.Storable.Mutable as DVSM
import qualified Data.Vector.Unboxed as DVU
import qualified Data.Vector.Unboxed.Mutable as DVUM

run_coroutine::Int->Int->DS.Seq Int->Event a->Engine b a c d e->Engine b a c d e
run_coroutine depth leaf_id index event engine=let (update,leaf)=intmap_functor_update leaf_id (functor_update_projection_object (functor_limited_update_widget depth (DT.swap . run_coroutine_a index event engine))) engine.leaf in DF.foldl' (\this_engine this_update->this_update this_engine) (engine {leaf=leaf}) update

run_coroutine_a::DS.Seq Int->Event a->Engine b a c d e->Widget b a c d e->(Widget b a c d e,DS.Seq (Engine b a c d e->Engine b a c d e))
run_coroutine_a this_index event engine widget=case widget of
    Coroutine {index,initial_min_index,min_index,initial_max_index,max_index,variable_length,user_variable_length,coroutine_state,layout,linear_coroutine,iterative}->let (update,new_coroutine_state)=DF.foldl' (\(this_update,this_coroutine_state) single_index->intmap_functor_update single_index (DT.swap . run_coroutine_b this_update event engine iterative linear_coroutine layout) this_coroutine_state) (DS.empty,coroutine_state) this_index in (Coroutine {index=index,initial_min_index=initial_min_index,min_index=min_index,initial_max_index=initial_max_index,max_index=max_index,variable_length=variable_length,user_variable_length=user_variable_length,coroutine_state=new_coroutine_state,layout=layout,linear_coroutine=linear_coroutine,iterative=iterative},update)
    _->EE.quick_error "run_coroutine_a" 0

run_coroutine_b::DS.Seq (Engine a b c d e->Engine a b c d e)->Event b->Engine a b c d e->Bool->DV.Vector (Linear_coroutine a b c d e)->DVS.Vector Layout->Coroutine_state a b c d e->(Coroutine_state a b c d e,DS.Seq (Engine a b c d e->Engine a b c d e))
run_coroutine_b update event engine iterative linear_coroutine layout coroutine_state=case coroutine_state of
    Coroutine_state {widget,variable,user_variable,program_counter,index_group,main_index_group,index_group_index,program_counter_index}->CMST.runST $ do
        new_variable<-DVU.unsafeThaw variable
        (new_widget,_,new_update,new_new_variable,new_user_variable,new_program_counter,new_index_group,new_main_index_group,new_index_group_index,new_program_counter_index)<-step_coroutine linear_coroutine program_counter_index index_group_index DS.empty DS.empty main_index_group index_group program_counter layout user_variable new_variable (for_iterative iterative) DS.empty event (if iterative then DF.foldl' (\this_engine this_update->this_update this_engine) engine update else engine) widget
        new_new_new_variable<-DVU.unsafeFreeze new_new_variable
        return (Coroutine_state {widget=new_widget,variable=new_new_new_variable,user_variable=new_user_variable,program_counter=new_program_counter,index_group=new_index_group,main_index_group=new_main_index_group,index_group_index=new_index_group_index,program_counter_index=new_program_counter_index},update DS.>< new_update)

for_iterative::Bool->(Engine a b c d e->Engine a b c d e)->Engine a b c d e->Engine a b c d e
for_iterative iterative update engine=if iterative then update engine else engine

init_coroutine_state::Int->Int->Widget a b c d e->Coroutine_state a b c d e
init_coroutine_state variable_length user_variable_length widget=Coroutine_state {widget=widget,variable=DVU.replicate variable_length 0,user_variable=DVU.replicate user_variable_length 0,program_counter=DIM.singleton 0 (Program_counter {code_index=0,clone_index=0}),index_group=DIM.empty,main_index_group=DS.singleton 0,index_group_index=0,program_counter_index=1}

to_coroutine::DS.Seq (Coroutine a b c d e)->Coroutine a b c d e
to_coroutine coroutine_sequence=case coroutine_sequence of
    DS.Empty->Done
    coroutine DS.:<| other_coroutine->if DS.null other_coroutine then coroutine else Then {coroutine_sequence=coroutine_sequence}

lift_coroutine::Coroutine a b c d e->Raw_coroutine a b c d e ()
lift_coroutine coroutine=Raw_coroutine {iterator=lift_coroutine_a coroutine}

lift_coroutine_a::Coroutine a b c d e->Int->(Int,DS.Seq (Coroutine a b c d e),())
lift_coroutine_a coroutine int=(int,DS.singleton coroutine,())

do_empty::Raw_coroutine a b c d e ()
do_empty=Raw_coroutine {iterator=do_empty_a}

do_empty_a::Int->(Int,DS.Seq (Coroutine a b c d e),())
do_empty_a int=(int,DS.empty,())

do_declare::Raw_coroutine a b c d e Int
do_declare=Raw_coroutine {iterator=do_declare_a}

do_declare_a::Int->(Int,DS.Seq (Coroutine a b c d e),Int)
do_declare_a int=(int+1,DS.empty,int)

do_emit::(Event a->Engine b a c d e->Widget b a c d e->(Widget b a c d e,Engine b a c d e->Engine b a c d e))->Raw_coroutine b a c d e ()
do_emit emit=lift_coroutine (Emit {emit=emit})

do_wait::Dynamic_int a b c d e->Raw_coroutine a b c d e ()
do_wait dynamic_int=lift_coroutine (Wait {dynamic_int=dynamic_int})

do_forever::Raw_coroutine a b c d e ()->Raw_coroutine a b c d e ()
do_forever raw_coroutine=Raw_coroutine {iterator=raw_coroutine_unary_operator (\coroutine_sequence->DS.singleton (Forever {coroutine=to_coroutine coroutine_sequence})) raw_coroutine.iterator}

do_then::Raw_coroutine a b c d e ()->Raw_coroutine a b c d e ()
do_then raw_coroutine=Raw_coroutine {iterator=raw_coroutine_unary_generator (\coroutine other_coroutine coroutine_sequence->if DS.null other_coroutine then DS.singleton coroutine else DS.singleton (Then {coroutine_sequence=coroutine_sequence})) raw_coroutine.iterator}

do_while::Dynamic_bool a b c d e->Raw_coroutine a b c d e ()->Raw_coroutine a b c d e ()
do_while dynamic_bool raw_coroutine=Raw_coroutine {iterator=raw_coroutine_unary_operator (\coroutine_sequence->DS.singleton (While {dynamic_bool=dynamic_bool,coroutine=to_coroutine coroutine_sequence})) raw_coroutine.iterator}

do_pause::Dynamic_bool a b c d e->Raw_coroutine a b c d e ()->Raw_coroutine a b c d e ()
do_pause dynamic_bool raw_coroutine=Raw_coroutine {iterator=raw_coroutine_unary_operator (\coroutine_sequence->DS.singleton (Pause {dynamic_bool=dynamic_bool,coroutine=to_coroutine coroutine_sequence})) raw_coroutine.iterator}

do_skip::Dynamic_bool a b c d e->Raw_coroutine a b c d e ()->Raw_coroutine a b c d e ()
do_skip dynamic_bool raw_coroutine=Raw_coroutine {iterator=raw_coroutine_unary_operator (\coroutine_sequence->DS.singleton (Skip {dynamic_bool=dynamic_bool,coroutine=to_coroutine coroutine_sequence})) raw_coroutine.iterator}

do_assign::Dynamic_int a b c d e->Int->Raw_coroutine a b c d e ()
do_assign dynamic_int int=lift_coroutine (Assign {dynamic_int=dynamic_int,int=int})

do_repeat::Dynamic_int a b c d e->Raw_coroutine a b c d e ()->Raw_coroutine a b c d e ()
do_repeat dynamic_int raw_coroutine=Raw_coroutine {iterator=raw_coroutine_unary_operator (\coroutine_sequence->DS.singleton (Repeat {dynamic_int=dynamic_int,coroutine=to_coroutine coroutine_sequence})) raw_coroutine.iterator}

do_clone::Int->Raw_coroutine a b c d e ()->Raw_coroutine a b c d e ()
do_clone int raw_coroutine=Raw_coroutine {iterator=raw_coroutine_unary_operator (\coroutine_sequence->DS.singleton (Clone {int=int,coroutine=to_coroutine coroutine_sequence})) raw_coroutine.iterator}

do_if::Dynamic_bool a b c d e->Raw_coroutine a b c d e ()->Raw_coroutine a b c d e ()->Raw_coroutine a b c d e ()
do_if dynamic_bool first_raw_coroutine second_raw_coroutine=Raw_coroutine {iterator=raw_coroutine_binary_operator (\first_coroutine_sequence second_coroutine_sequence->DS.singleton (If {dynamic_bool=dynamic_bool,first_coroutine=to_coroutine first_coroutine_sequence,second_coroutine=to_coroutine second_coroutine_sequence})) first_raw_coroutine.iterator second_raw_coroutine.iterator}

do_dynamic_clone::Dynamic_int a b c d e->Int->Raw_coroutine a b c d e ()->Raw_coroutine a b c d e ()
do_dynamic_clone dynamic_int int raw_coroutine=Raw_coroutine {iterator=raw_coroutine_unary_operator (\coroutine_sequence->DS.singleton (Dynamic_clone {dynamic_int=dynamic_int,int=int,coroutine=to_coroutine coroutine_sequence})) raw_coroutine.iterator}

do_case::Dynamic_int a b c d e->Raw_coroutine a b c d e ()->Raw_coroutine a b c d e ()
do_case dynamic_int raw_coroutine=Raw_coroutine {iterator=raw_coroutine_unary_generator (\coroutine other_coroutine coroutine_sequence->let int=DS.length other_coroutine in if int==0 then DS.singleton coroutine else DS.singleton (Case {dynamic_int=dynamic_int,int=int+1,coroutine_sequence=coroutine_sequence})) raw_coroutine.iterator}

do_fork::Raw_coroutine a b c d e ()->Raw_coroutine a b c d e ()
do_fork raw_coroutine=Raw_coroutine {iterator=raw_coroutine_unary_generator (\coroutine other_coroutine _->let int=DS.length other_coroutine in if int==0 then DS.singleton coroutine else DS.singleton (Fork {int=int,coroutine=coroutine,coroutine_sequence=other_coroutine})) raw_coroutine.iterator}

do_race::Dynamic_int a b c d e->Int->Raw_coroutine a b c d e ()->Raw_coroutine a b c d e ()
do_race dynamic_int int raw_coroutine=Raw_coroutine {iterator=raw_coroutine_unary_generator (\coroutine other_coroutine coroutine_sequence->let new_int=DS.length other_coroutine in if new_int==0 then DS.singleton coroutine else DS.singleton (Race {dynamic_int=dynamic_int,first_int=int,second_int=new_int+1,coroutine_sequence=coroutine_sequence})) raw_coroutine.iterator}

raw_coroutine_unary_generator::(Coroutine a b c d e->DS.Seq (Coroutine a b c d e)->DS.Seq (Coroutine a b c d e)->DS.Seq (Coroutine a b c d e))->(Int->(Int,DS.Seq (Coroutine a b c d e),()))->Int->(Int,DS.Seq (Coroutine a b c d e),())
raw_coroutine_unary_generator generator iterator int=let (new_int,coroutine_sequence,_)=iterator int in case coroutine_sequence of
    DS.Empty->(new_int,DS.empty,())
    coroutine DS.:<| other_coroutine->(new_int,generator coroutine other_coroutine coroutine_sequence,())

raw_coroutine_unary_operator::(DS.Seq (Coroutine a b c d e)->DS.Seq (Coroutine a b c d e))->(Int->(Int,DS.Seq (Coroutine a b c d e),()))->Int->(Int,DS.Seq (Coroutine a b c d e),())
raw_coroutine_unary_operator operator iterator int=let (new_int,coroutine_sequence,_)=iterator int in (new_int,operator coroutine_sequence,())

raw_coroutine_binary_operator::(DS.Seq (Coroutine a b c d e)->DS.Seq (Coroutine a b c d e)->DS.Seq (Coroutine a b c d e))->(Int->(Int,DS.Seq (Coroutine a b c d e),()))->(Int->(Int,DS.Seq (Coroutine a b c d e),()))->Int->(Int,DS.Seq (Coroutine a b c d e),())
raw_coroutine_binary_operator operator first_iterator second_iterator int=let (first_int,first_coroutine_sequence,_)=first_iterator int in let (second_int,second_coroutine_sequence,_)=second_iterator first_int in (second_int,operator first_coroutine_sequence second_coroutine_sequence,())

from_coroutine::Coroutine a b c d e->Int->(DV.Vector (Linear_coroutine a b c d e),DVS.Vector Layout,Int,Int)
from_coroutine coroutine int=CMST.runST $ do
    layout<-DVSM.replicate int (Layout {address=0,size=0})
    (linear_coroutine,new_layout,int_index,user_int_index,code_index)<-from_coroutine_a coroutine 1 0 0 0 layout DIM.empty
    new_new_layout<-DVS.unsafeFreeze new_layout
    return (DV.generate (code_index+1) (\this_code_index->if code_index==this_code_index then Linear_end else intmap_lookup this_code_index linear_coroutine),new_new_layout,int_index,user_int_index)

from_coroutine_a::Coroutine a b c d e->Int->Int->Int->Int->DVSM.MVector f Layout->DIM.IntMap (Linear_coroutine a b c d e)->CMST.ST f (DIM.IntMap (Linear_coroutine a b c d e),DVSM.MVector f Layout,Int,Int,Int)
from_coroutine_a this_coroutine clone_number code_index user_int_index int_index layout linear_coroutine=case this_coroutine of
    Done->return (linear_coroutine,layout,int_index,user_int_index,code_index)
    Emit {emit}->return (DIM.insert code_index (Linear_emit {emit=emit}) linear_coroutine,layout,int_index,user_int_index,code_index+1)
    Wait {dynamic_int}->return (DIM.insert (code_index+1) (Linear_wait {int_index=int_index}) (DIM.insert code_index (Linear_dynamic_int {int_index=int_index,dynamic_int=dynamic_int}) linear_coroutine),layout,int_index+clone_number,user_int_index,code_index+2)
    Forever {coroutine}->do
        (new_linear_coroutine,new_layout,new_int_index,new_user_int_index,new_code_index)<-from_coroutine_a coroutine clone_number code_index user_int_index int_index layout linear_coroutine
        return (DIM.insert new_code_index (Linear_jump {code_index=code_index}) new_linear_coroutine,new_layout,new_int_index,new_user_int_index,new_code_index+1)
    Then {coroutine_sequence}->then_coroutine coroutine_sequence clone_number code_index user_int_index int_index layout linear_coroutine
    While {dynamic_bool,coroutine}->do
        (new_linear_coroutine,new_layout,new_int_index,new_user_int_index,new_code_index)<-from_coroutine_a coroutine clone_number (code_index+1) user_int_index int_index layout linear_coroutine
        let end_code_index=new_code_index+1 in return (DIM.insert new_code_index (Linear_jump {code_index=code_index}) (DIM.insert code_index (Linear_false_jump {code_index=end_code_index,dynamic_bool=dynamic_bool}) new_linear_coroutine),new_layout,new_int_index,new_user_int_index,end_code_index)
    Pause {dynamic_bool,coroutine}->let body_code_index=code_index+2 in let countdown_int_index=int_index+clone_number in do
        (new_linear_coroutine,new_layout,new_int_index,new_user_int_index,new_code_index)<-from_coroutine_a coroutine clone_number body_code_index user_int_index (int_index+2*clone_number) layout linear_coroutine
        let loop_code_index=new_code_index+1 in let yield_code_index=loop_code_index+3 in let jump_code_index=loop_code_index+4 in return (DIM.insert jump_code_index (Linear_kill_group {int_index=int_index,int=1}) (DIM.insert yield_code_index (Linear_yield {code_index=loop_code_index}) (DIM.insert (loop_code_index+2) (Linear_wake {int_index=int_index}) (DIM.insert (loop_code_index+1) (Linear_true_jump {code_index=yield_code_index,dynamic_bool=dynamic_bool}) (DIM.insert loop_code_index (Linear_less_jump {int_index=countdown_int_index,code_index=jump_code_index,int=1}) (DIM.insert new_code_index (Linear_countdown {int_index=countdown_int_index}) (DIM.insert (code_index+1) (Linear_jump {code_index=loop_code_index}) (DIM.insert code_index (Linear_create_group {first_int_index=int_index,second_int_index=countdown_int_index,group_code_index=DIM.singleton 0 body_code_index,int=1}) new_linear_coroutine))))))),new_layout,new_int_index,new_user_int_index,loop_code_index+5)
    Skip {dynamic_bool,coroutine}->let body_code_index=code_index+2 in let countdown_int_index=int_index+clone_number in do
        (new_linear_coroutine,new_layout,new_int_index,new_user_int_index,new_code_index)<-from_coroutine_a coroutine clone_number body_code_index user_int_index (int_index+2*clone_number) layout linear_coroutine
        let loop_code_index=new_code_index+1 in let jump_code_index=loop_code_index+4 in return (DIM.insert jump_code_index (Linear_kill_group {int_index=int_index,int=1}) (DIM.insert (loop_code_index+3) (Linear_yield {code_index=loop_code_index}) (DIM.insert (loop_code_index+2) (Linear_wake {int_index=int_index}) (DIM.insert (loop_code_index+1) (Linear_true_jump {code_index=jump_code_index,dynamic_bool=dynamic_bool}) (DIM.insert loop_code_index (Linear_less_jump {int_index=countdown_int_index,code_index=jump_code_index,int=1}) (DIM.insert new_code_index (Linear_countdown {int_index=countdown_int_index}) (DIM.insert (code_index+1) (Linear_jump {code_index=loop_code_index}) (DIM.insert code_index (Linear_create_group {first_int_index=int_index,second_int_index=countdown_int_index,group_code_index=DIM.singleton 0 body_code_index,int=1}) new_linear_coroutine))))))),new_layout,new_int_index,new_user_int_index,loop_code_index+5)
    Assign {dynamic_int,int}->do
        single_layout<-DVSM.read layout int
        case single_layout of
            Layout {address,size}->if size==0
                then do
                    DVSM.write layout int (Layout {address=user_int_index,size=clone_number})
                    return (DIM.insert code_index (Linear_assign {user_int_index=user_int_index,clone_number=clone_number,dynamic_int=dynamic_int}) linear_coroutine,layout,int_index,user_int_index+clone_number,code_index+1)
                else return (DIM.insert code_index (Linear_assign {user_int_index=address,clone_number=size,dynamic_int=dynamic_int}) linear_coroutine,layout,int_index,user_int_index,code_index+1)
    Repeat {dynamic_int,coroutine}->let loop_code_index=code_index+2 in do
        (new_linear_coroutine,new_layout,new_int_index,new_user_int_index,new_code_index)<-from_coroutine_a coroutine clone_number loop_code_index user_int_index (int_index+clone_number) layout linear_coroutine
        let end_code_index=new_code_index+1 in return (DIM.insert new_code_index (Linear_one_more_jump {int_index=int_index,code_index=loop_code_index}) (DIM.insert (code_index+1) (Linear_one_less_jump {int_index=int_index,code_index=end_code_index}) (DIM.insert code_index (Linear_dynamic_int {int_index=int_index,dynamic_int=dynamic_int}) new_linear_coroutine)),new_layout,new_int_index,new_user_int_index,end_code_index)
    Clone {int,coroutine}->let new_int=int-1 in if new_int<0 then return (linear_coroutine,layout,int_index,user_int_index,code_index) else do
        (new_linear_coroutine,new_layout,new_int_index,new_user_int_index,new_code_index)<-from_coroutine_a coroutine (int*clone_number) (code_index+1) user_int_index (int_index+clone_number) layout linear_coroutine
        return (DIM.insert new_code_index (Linear_kill_clone {int_index=int_index,clone_number=clone_number}) (DIM.insert code_index (Linear_clone {int_index=int_index,clone_number=clone_number,int=new_int}) new_linear_coroutine),new_layout,new_int_index,new_user_int_index,new_code_index+1)
    If {dynamic_bool,first_coroutine,second_coroutine}->do
        (first_linear_coroutine,first_layout,first_int_index,first_user_int_index,first_code_index)<-from_coroutine_a first_coroutine clone_number (code_index+1) user_int_index int_index layout linear_coroutine
        let new_code_index=first_code_index+1
        (second_linear_coroutine,second_layout,second_int_index,second_user_int_index,second_code_index)<-from_coroutine_a second_coroutine clone_number new_code_index first_user_int_index int_index first_layout first_linear_coroutine
        return (DIM.insert first_code_index (Linear_jump {code_index=second_code_index}) (DIM.insert code_index (Linear_false_jump {code_index=new_code_index,dynamic_bool=dynamic_bool}) second_linear_coroutine),second_layout,max first_int_index second_int_index,max first_user_int_index second_user_int_index,second_code_index)
    Dynamic_clone {dynamic_int,int,coroutine}->let new_int=int-1 in if new_int<0 then return (linear_coroutine,layout,int_index,user_int_index,code_index) else do
        (new_linear_coroutine,new_layout,new_int_index,new_user_int_index,new_code_index)<-from_coroutine_a coroutine (int*clone_number) (code_index+1) user_int_index (int_index+clone_number) layout linear_coroutine
        let kill_code_index=new_code_index+1 in return (DIM.insert new_code_index (Linear_kill_clone {int_index=int_index,clone_number=clone_number}) (DIM.insert code_index (Linear_dynamic_clone {int_index=int_index,code_index=kill_code_index,clone_number=clone_number,dynamic_int=dynamic_int,int=new_int}) new_linear_coroutine),new_layout,new_int_index,new_user_int_index,kill_code_index)
    Case {dynamic_int,int,coroutine_sequence}->do
        (new_linear_coroutine,new_layout,new_int_index,new_user_int_index,new_code_index,single_linear_coroutine)<-case_coroutine DIM.empty DIS.empty coroutine_sequence int 0 (int_index+clone_number) clone_number (code_index+2) user_int_index int_index layout linear_coroutine
        return (DIM.insert (code_index+1) single_linear_coroutine (DIM.insert code_index (Linear_dynamic_int {int_index=int_index,dynamic_int=dynamic_int}) new_linear_coroutine),new_layout,new_int_index,new_user_int_index,new_code_index)
    Fork {int,coroutine,coroutine_sequence}->let body_code_index=code_index+1 in do
        (new_linear_coroutine,new_layout,new_int_index,new_user_int_index,new_code_index)<-from_coroutine_a coroutine clone_number (body_code_index+int) user_int_index (int_index+clone_number) layout linear_coroutine
        (final_linear_coroutine,final_layout,final_int_index,final_user_int_index,final_code_index)<-fork_coroutine DIS.empty coroutine_sequence body_code_index clone_number (new_code_index+1) new_user_int_index new_int_index new_layout (DIM.insert code_index (Linear_int {int_index=int_index,int=int}) new_linear_coroutine)
        return (DIM.insert final_code_index (Linear_kill_fork {int_index=int_index}) final_linear_coroutine,final_layout,final_int_index,final_user_int_index,final_code_index+1)
    Race {dynamic_int,first_int,second_int,coroutine_sequence}->let countdown_int_index=int_index+clone_number in do
        (new_linear_coroutine,new_layout,new_int_index,new_user_int_index,new_code_index,group_code_index)<-race_coroutine coroutine_sequence 0 countdown_int_index clone_number (code_index+2) user_int_index (int_index+2*clone_number) layout linear_coroutine
        let kill_code_index=new_code_index+3 in return (DIM.insert kill_code_index (Linear_kill_group {int_index=int_index,int=second_int}) (DIM.insert (new_code_index+2) (Linear_yield {code_index=new_code_index}) (DIM.insert (new_code_index+1) (Linear_wake_group {int_index=int_index,dynamic_int=dynamic_int,int=second_int}) (DIM.insert new_code_index (Linear_less_jump {int_index=countdown_int_index,code_index=kill_code_index,int=second_int-first_int+1}) (DIM.insert (code_index+1) (Linear_jump {code_index=new_code_index}) (DIM.insert code_index (Linear_create_group {first_int_index=int_index,second_int_index=countdown_int_index,group_code_index=group_code_index,int=second_int}) new_linear_coroutine))))),new_layout,new_int_index,new_user_int_index,new_code_index+4)

then_coroutine::DS.Seq (Coroutine a b c d e)->Int->Int->Int->Int->DVSM.MVector f Layout->DIM.IntMap (Linear_coroutine a b c d e)->CMST.ST f (DIM.IntMap (Linear_coroutine a b c d e),DVSM.MVector f Layout,Int,Int,Int)
then_coroutine coroutine_sequence clone_number code_index user_int_index int_index layout linear_coroutine=case coroutine_sequence of
    DS.Empty->return (linear_coroutine,layout,int_index,user_int_index,code_index)
    coroutine DS.:<| other_coroutine->do
        (new_linear_coroutine,new_layout,new_int_index,new_user_int_index,new_code_index)<-from_coroutine_a coroutine clone_number code_index user_int_index int_index layout linear_coroutine
        then_coroutine other_coroutine clone_number new_code_index new_user_int_index new_int_index new_layout new_linear_coroutine

case_coroutine::DIM.IntMap Int->DIS.IntSet->DS.Seq (Coroutine a b c d e)->Int->Int->Int->Int->Int->Int->Int->DVSM.MVector f Layout->DIM.IntMap (Linear_coroutine a b c d e)->CMST.ST f (DIM.IntMap (Linear_coroutine a b c d e),DVSM.MVector f Layout,Int,Int,Int,Linear_coroutine a b c d e)
case_coroutine case_code_index jump_code_index coroutine_sequence int index max_int_index clone_number code_index user_int_index int_index layout linear_coroutine=case coroutine_sequence of
    DS.Empty->let (new_linear_coroutine,new_code_index)=case_coroutine_a case_code_index int 0 code_index int_index linear_coroutine in return (DIS.foldl' (\this_linear_coroutine this_code_index->DIM.insert this_code_index (Linear_jump {code_index=new_code_index}) this_linear_coroutine) new_linear_coroutine jump_code_index,layout,max_int_index,user_int_index,new_code_index,Linear_jump {code_index=code_index})
    coroutine DS.:<| other_coroutine->do
        (new_linear_coroutine,new_layout,new_int_index,new_user_int_index,new_code_index)<-from_coroutine_a coroutine clone_number code_index user_int_index (int_index+clone_number) layout linear_coroutine
        case_coroutine (DIM.insert index code_index case_code_index) (DIS.insert new_code_index jump_code_index) other_coroutine int (index+1) (max new_int_index max_int_index) clone_number (new_code_index+1) (max user_int_index new_user_int_index) int_index new_layout new_linear_coroutine

case_coroutine_a::DIM.IntMap Int->Int->Int->Int->Int->DIM.IntMap (Linear_coroutine a b c d e)->(DIM.IntMap (Linear_coroutine a b c d e),Int)
case_coroutine_a case_code_index int index code_index int_index linear_coroutine=case int of
    1->(DIM.insert code_index (Linear_jump {code_index=intmap_lookup index case_code_index}) linear_coroutine,code_index+1)
    _->let half_int=div int 2 in let new_int=index+half_int in let (right_linear_coroutine,right_code_index)=case_coroutine_a case_code_index (int-half_int) new_int (code_index+1) int_index linear_coroutine in let (left_linear_coroutine,left_code_index)=case_coroutine_a case_code_index half_int index right_code_index int_index right_linear_coroutine in (DIM.insert code_index (Linear_less_jump {int_index=int_index,code_index=right_code_index,int=new_int}) left_linear_coroutine,left_code_index)

fork_coroutine::DIS.IntSet->DS.Seq (Coroutine a b c d e)->Int->Int->Int->Int->Int->DVSM.MVector f Layout->DIM.IntMap (Linear_coroutine a b c d e)->CMST.ST f (DIM.IntMap (Linear_coroutine a b c d e),DVSM.MVector f Layout,Int,Int,Int)
fork_coroutine jump_code_index coroutine_sequence fork_code_index clone_number code_index user_int_index int_index layout linear_coroutine=case coroutine_sequence of
    DS.Empty->let new_code_index=code_index-1 in return (DIS.foldl' (\this_linear_coroutine this_code_index->DIM.insert this_code_index (Linear_jump {code_index=new_code_index}) this_linear_coroutine) linear_coroutine jump_code_index,layout,int_index,user_int_index,new_code_index)
    coroutine DS.:<| other_coroutine->do
        (new_linear_coroutine,new_layout,new_int_index,new_user_int_index,new_code_index)<-from_coroutine_a coroutine clone_number code_index user_int_index int_index layout linear_coroutine
        fork_coroutine (DIS.insert (code_index-1) jump_code_index) other_coroutine (fork_code_index+1) clone_number (new_code_index+1) new_user_int_index new_int_index new_layout (DIM.insert fork_code_index (Linear_fork {code_index=code_index}) new_linear_coroutine)

race_coroutine::DS.Seq (Coroutine a b c d e)->Int->Int->Int->Int->Int->Int->DVSM.MVector f Layout->DIM.IntMap (Linear_coroutine a b c d e)->CMST.ST f (DIM.IntMap (Linear_coroutine a b c d e),DVSM.MVector f Layout,Int,Int,Int,DIM.IntMap Int)
race_coroutine coroutine_sequence index countdown_int_index clone_number code_index user_int_index int_index layout linear_coroutine=case coroutine_sequence of
    DS.Empty->return (linear_coroutine,layout,int_index,user_int_index,code_index,DIM.empty)
    coroutine DS.:<| other_coroutine->do
        (new_linear_coroutine,new_layout,new_int_index,new_user_int_index,new_code_index)<-from_coroutine_a coroutine clone_number code_index user_int_index int_index layout linear_coroutine
        (final_linear_coroutine,final_layout,final_int_index,final_user_int_index,final_code_index,group_code_index)<-race_coroutine other_coroutine (index+1) countdown_int_index clone_number (new_code_index+1) new_user_int_index (max int_index new_int_index) new_layout (DIM.insert new_code_index (Linear_countdown {int_index=countdown_int_index}) new_linear_coroutine)
        return (final_linear_coroutine,final_layout,final_int_index,final_user_int_index,final_code_index,DIM.insert index code_index group_code_index)

step_coroutine::DV.Vector (Linear_coroutine a b c d e)->Int->Int->DS.Seq Int->DS.Seq Int->DS.Seq Int->DIM.IntMap (DS.Seq Int)->DIM.IntMap Program_counter->DVS.Vector Layout->DVU.Vector Int->DVUM.MVector f Int->((Engine a b c d e->Engine a b c d e)->Engine a b c d e->Engine a b c d e)->DS.Seq (Engine a b c d e->Engine a b c d e)->Event b->Engine a b c d e->Widget a b c d e->CMST.ST f (Widget a b c d e,Engine a b c d e,DS.Seq (Engine a b c d e->Engine a b c d e),DVUM.MVector f Int,DVU.Vector Int,DIM.IntMap Program_counter,DIM.IntMap (DS.Seq Int),DS.Seq Int,Int,Int)
step_coroutine linear_coroutine program_counter_index index_group_index survived_main_index_group newborn_main_index_group main_index_group index_group program_counter layout user_variable variable updater update event engine widget=case newborn_main_index_group of
    DS.Empty->case main_index_group of
        DS.Empty->return (widget,engine,update,variable,user_variable,program_counter,index_group,survived_main_index_group,index_group_index,program_counter_index)
        (main_index DS.:<| other_main_index)->let (new_program_counter,single_program_counter)=intmap_delete_lookup main_index program_counter in step_coroutine_a linear_coroutine main_index single_program_counter.clone_index single_program_counter.code_index program_counter_index index_group_index survived_main_index_group DS.empty other_main_index index_group new_program_counter layout user_variable variable updater update event engine widget
    (main_index DS.:<| other_main_index)->let (new_program_counter,single_program_counter)=intmap_delete_lookup main_index program_counter in step_coroutine_a linear_coroutine main_index single_program_counter.clone_index single_program_counter.code_index program_counter_index index_group_index survived_main_index_group DS.empty (other_main_index DS.>< main_index_group) index_group new_program_counter layout user_variable variable updater update event engine widget

step_coroutine_a::DV.Vector (Linear_coroutine a b c d e)->Int->Int->Int->Int->Int->DS.Seq Int->DS.Seq Int->DS.Seq Int->DIM.IntMap (DS.Seq Int)->DIM.IntMap Program_counter->DVS.Vector Layout->DVU.Vector Int->DVUM.MVector f Int->((Engine a b c d e->Engine a b c d e)->Engine a b c d e->Engine a b c d e)->DS.Seq (Engine a b c d e->Engine a b c d e)->Event b->Engine a b c d e->Widget a b c d e->CMST.ST f (Widget a b c d e,Engine a b c d e,DS.Seq (Engine a b c d e->Engine a b c d e),DVUM.MVector f Int,DVU.Vector Int,DIM.IntMap Program_counter,DIM.IntMap (DS.Seq Int),DS.Seq Int,Int,Int)
step_coroutine_a linear_coroutine main_index clone_index this_code_index program_counter_index index_group_index survived_main_index_group newborn_main_index_group main_index_group index_group program_counter layout user_variable variable updater update event engine widget=case linear_coroutine DV.! this_code_index of
    Linear_end->step_coroutine linear_coroutine program_counter_index index_group_index survived_main_index_group newborn_main_index_group main_index_group index_group program_counter layout user_variable variable updater update event engine widget
    Linear_emit {emit}->let (new_widget,new_update)=emit event engine widget in step_coroutine_a linear_coroutine main_index clone_index (this_code_index+1) program_counter_index index_group_index survived_main_index_group newborn_main_index_group main_index_group index_group program_counter layout user_variable variable updater (update DS.|> new_update) event (updater new_update engine) new_widget
    Linear_wait {int_index}->do
        int<-DVUM.read variable (int_index+clone_index)
        DVUM.write variable (int_index+clone_index) (int-1)
        if 0<int then step_coroutine linear_coroutine program_counter_index index_group_index (survived_main_index_group DS.|> main_index) newborn_main_index_group main_index_group index_group (intmap_insert main_index (Program_counter {code_index=this_code_index,clone_index=clone_index}) program_counter) layout user_variable variable updater update event engine widget else step_coroutine_a linear_coroutine main_index clone_index (this_code_index+1) program_counter_index index_group_index survived_main_index_group newborn_main_index_group main_index_group index_group program_counter layout user_variable variable updater update event engine widget
    Linear_kill_fork {int_index}->do
        int<-DVUM.read variable (int_index+clone_index)
        DVUM.write variable (int_index+clone_index) (int-1)
        if 0<int then step_coroutine linear_coroutine program_counter_index index_group_index survived_main_index_group newborn_main_index_group main_index_group index_group program_counter layout user_variable variable updater update event engine widget else step_coroutine_a linear_coroutine main_index clone_index (this_code_index+1) program_counter_index index_group_index survived_main_index_group newborn_main_index_group main_index_group index_group program_counter layout user_variable variable updater update event engine widget
    Linear_countdown {int_index}->do
        DVUM.modify variable (\int->int-1) (int_index+clone_index)
        step_coroutine linear_coroutine program_counter_index index_group_index survived_main_index_group newborn_main_index_group main_index_group index_group program_counter layout user_variable variable updater update event engine widget
    Linear_wake {int_index}->do
        int<-DVUM.read variable (int_index+clone_index)
        let (new_index_group,maybe_single_index_group)=intmap_delete_maybe_lookup int index_group in case maybe_single_index_group of
            Nothing->step_coroutine_a linear_coroutine main_index clone_index (this_code_index+1) program_counter_index index_group_index survived_main_index_group newborn_main_index_group main_index_group index_group program_counter layout user_variable variable updater update event engine widget
            Just single_index_group->do
                (new_widget,new_engine,new_update,new_variable,new_user_variable,new_program_counter,new_new_index_group,new_survived_main_index_group,new_index_group_index,new_program_counter_index)<-step_coroutine linear_coroutine program_counter_index index_group_index DS.empty DS.empty single_index_group new_index_group program_counter layout user_variable variable updater DS.empty event engine widget
                step_coroutine_a linear_coroutine main_index clone_index (this_code_index+1) new_program_counter_index new_index_group_index survived_main_index_group newborn_main_index_group main_index_group (if DS.null new_survived_main_index_group then new_new_index_group else DIM.insert int new_survived_main_index_group new_new_index_group) new_program_counter layout new_user_variable new_variable updater (update DS.>< new_update) event new_engine new_widget
    Linear_fork {code_index}->step_coroutine_a linear_coroutine main_index clone_index (this_code_index+1) (program_counter_index+1) index_group_index survived_main_index_group (newborn_main_index_group DS.|> program_counter_index) main_index_group index_group (intmap_insert program_counter_index (Program_counter {code_index=code_index,clone_index=clone_index}) program_counter) layout user_variable variable updater update event engine widget
    Linear_yield {code_index}->step_coroutine linear_coroutine program_counter_index index_group_index (survived_main_index_group DS.|> main_index) newborn_main_index_group main_index_group index_group (intmap_insert main_index (Program_counter {code_index=code_index,clone_index=clone_index}) program_counter) layout user_variable variable updater update event engine widget
    Linear_jump {code_index}->step_coroutine_a linear_coroutine main_index clone_index code_index program_counter_index index_group_index survived_main_index_group newborn_main_index_group main_index_group index_group program_counter layout user_variable variable updater update event engine widget
    Linear_one_less_jump {int_index,code_index}->do
        int<-DVUM.read variable (int_index+clone_index)
        if int<1 then step_coroutine_a linear_coroutine main_index clone_index code_index program_counter_index index_group_index survived_main_index_group newborn_main_index_group main_index_group index_group program_counter layout user_variable variable updater update event engine widget else step_coroutine_a linear_coroutine main_index clone_index (this_code_index+1) program_counter_index index_group_index survived_main_index_group newborn_main_index_group main_index_group index_group program_counter layout user_variable variable updater update event engine widget
    Linear_one_more_jump {int_index,code_index}->do
        int<-DVUM.read variable (int_index+clone_index)
        DVUM.write variable (int_index+clone_index) (int-1)
        if 1<int then step_coroutine_a linear_coroutine main_index clone_index code_index program_counter_index index_group_index survived_main_index_group newborn_main_index_group main_index_group index_group program_counter layout user_variable variable updater update event engine widget else step_coroutine_a linear_coroutine main_index clone_index (this_code_index+1) program_counter_index index_group_index survived_main_index_group newborn_main_index_group main_index_group index_group program_counter layout user_variable variable updater update event engine widget
    Linear_kill_clone {int_index,clone_number}->let new_clone_index=mod clone_index clone_number in do
        int<-DVUM.read variable (int_index+new_clone_index)
        DVUM.write variable (int_index+new_clone_index) (int-1)
        if 0<int then step_coroutine linear_coroutine program_counter_index index_group_index survived_main_index_group newborn_main_index_group main_index_group index_group program_counter layout user_variable variable updater update event engine widget else step_coroutine_a linear_coroutine main_index new_clone_index (this_code_index+1) program_counter_index index_group_index survived_main_index_group newborn_main_index_group main_index_group index_group program_counter layout user_variable variable updater update event engine widget
    Linear_dynamic_int {int_index,dynamic_int}->do
        DVUM.write variable (int_index+clone_index) (dynamic_int.dynamic_int (user_variable_getter user_variable layout clone_index) event engine widget)
        step_coroutine_a linear_coroutine main_index clone_index (this_code_index+1) program_counter_index index_group_index survived_main_index_group newborn_main_index_group main_index_group index_group program_counter layout user_variable variable updater update event engine widget
    Linear_int {int_index,int}->do
        DVUM.write variable (int_index+clone_index) int
        step_coroutine_a linear_coroutine main_index clone_index (this_code_index+1) program_counter_index index_group_index survived_main_index_group newborn_main_index_group main_index_group index_group program_counter layout user_variable variable updater update event engine widget
    Linear_kill_group {int_index,int}->do
        this_int<-DVUM.read variable (int_index+clone_index)
        let (new_program_counter,new_index_group)=run_kill_group int 0 this_int index_group program_counter in step_coroutine_a linear_coroutine main_index clone_index (this_code_index+1) program_counter_index index_group_index survived_main_index_group newborn_main_index_group main_index_group new_index_group new_program_counter layout user_variable variable updater update event engine widget
    Linear_true_jump {code_index,dynamic_bool}->if dynamic_bool.dynamic_bool (user_variable_getter user_variable layout clone_index) event engine widget then step_coroutine_a linear_coroutine main_index clone_index code_index program_counter_index index_group_index survived_main_index_group newborn_main_index_group main_index_group index_group program_counter layout user_variable variable updater update event engine widget else step_coroutine_a linear_coroutine main_index clone_index (this_code_index+1) program_counter_index index_group_index survived_main_index_group newborn_main_index_group main_index_group index_group program_counter layout user_variable variable updater update event engine widget
    Linear_false_jump {code_index,dynamic_bool}->if dynamic_bool.dynamic_bool (user_variable_getter user_variable layout clone_index) event engine widget then step_coroutine_a linear_coroutine main_index clone_index (this_code_index+1) program_counter_index index_group_index survived_main_index_group newborn_main_index_group main_index_group index_group program_counter layout user_variable variable updater update event engine widget else step_coroutine_a linear_coroutine main_index clone_index code_index program_counter_index index_group_index survived_main_index_group newborn_main_index_group main_index_group index_group program_counter layout user_variable variable updater update event engine widget
    Linear_less_jump {int_index,code_index,int}->do
        this_int<-DVUM.read variable (int_index+clone_index)
        if this_int<int then step_coroutine_a linear_coroutine main_index clone_index code_index program_counter_index index_group_index survived_main_index_group newborn_main_index_group main_index_group index_group program_counter layout user_variable variable updater update event engine widget else step_coroutine_a linear_coroutine main_index clone_index (this_code_index+1) program_counter_index index_group_index survived_main_index_group newborn_main_index_group main_index_group index_group program_counter layout user_variable variable updater update event engine widget
    Linear_clone {int_index,clone_number,int}->do
        DVUM.write variable (int_index+clone_index) int
        let new_code_index=this_code_index+1 in let (new_program_counter,new_newborn_main_index_group,new_program_counter_index)=run_clone int clone_number (clone_index+clone_number) new_code_index program_counter_index newborn_main_index_group program_counter in step_coroutine_a linear_coroutine main_index clone_index new_code_index new_program_counter_index index_group_index survived_main_index_group new_newborn_main_index_group main_index_group index_group new_program_counter layout user_variable variable updater update event engine widget
    Linear_wake_group {int_index,dynamic_int,int}->let new_int=dynamic_int.dynamic_int (user_variable_getter user_variable layout clone_index) event engine widget in if new_int<0||int<=new_int then EE.quick_error "step_coroutine_a" 0 else do
        this_int<-DVUM.read variable (int_index+clone_index)
        let new_index_group_index=this_int+new_int in let (new_index_group,maybe_single_index_group)=intmap_delete_maybe_lookup new_index_group_index index_group in case maybe_single_index_group of
            Nothing->step_coroutine_a linear_coroutine main_index clone_index (this_code_index+1) program_counter_index index_group_index survived_main_index_group newborn_main_index_group main_index_group index_group program_counter layout user_variable variable updater update event engine widget
            Just single_index_group->do
                (new_widget,new_engine,new_update,new_variable,new_user_variable,new_program_counter,new_new_index_group,new_survived_main_index_group,new_new_index_group_index,new_program_counter_index)<-step_coroutine linear_coroutine program_counter_index index_group_index DS.empty DS.empty single_index_group new_index_group program_counter layout user_variable variable updater DS.empty event engine widget
                step_coroutine_a linear_coroutine main_index clone_index (this_code_index+1) new_program_counter_index new_new_index_group_index survived_main_index_group newborn_main_index_group main_index_group (if DS.null new_survived_main_index_group then new_new_index_group else DIM.insert new_index_group_index new_survived_main_index_group new_new_index_group) new_program_counter layout new_user_variable new_variable updater (update DS.>< new_update) event new_engine new_widget
    Linear_assign {user_int_index,clone_number,dynamic_int}->do
        new_user_variable<-DVU.unsafeThaw user_variable
        DVUM.write new_user_variable (user_int_index+mod clone_index clone_number) (dynamic_int.dynamic_int (user_variable_getter user_variable layout clone_index) event engine widget)
        new_new_user_variable<-DVU.unsafeFreeze new_user_variable
        step_coroutine_a linear_coroutine main_index clone_index (this_code_index+1) program_counter_index index_group_index survived_main_index_group newborn_main_index_group main_index_group index_group program_counter layout new_new_user_variable variable updater update event engine widget
    Linear_create_group {first_int_index,second_int_index,group_code_index,int}->do
        DVUM.write variable (first_int_index+clone_index) index_group_index
        DVUM.write variable (second_int_index+clone_index) int
        let (new_program_counter,new_index_group,new_program_counter_index)=run_create_group group_code_index int 0 clone_index program_counter_index index_group_index index_group program_counter in step_coroutine_a linear_coroutine main_index clone_index (this_code_index+1) new_program_counter_index (index_group_index+int) survived_main_index_group newborn_main_index_group main_index_group new_index_group new_program_counter layout user_variable variable updater update event engine widget
    Linear_dynamic_clone {int_index,code_index,clone_number,dynamic_int,int}->let new_int=dynamic_int.dynamic_int (user_variable_getter user_variable layout clone_index) event engine widget-1 in if new_int<0 then step_coroutine_a linear_coroutine main_index clone_index code_index program_counter_index index_group_index survived_main_index_group newborn_main_index_group main_index_group index_group program_counter layout user_variable variable updater update event engine widget else if int<new_int then EE.quick_error "step_coroutine_a" 1 else let new_code_index=this_code_index+1 in let (new_program_counter,new_newborn_main_index_group,new_program_counter_index)=run_clone new_int clone_number (clone_index+clone_number) new_code_index program_counter_index newborn_main_index_group program_counter in do
        DVUM.write variable (int_index+clone_index) new_int
        step_coroutine_a linear_coroutine main_index clone_index new_code_index new_program_counter_index index_group_index survived_main_index_group new_newborn_main_index_group main_index_group index_group new_program_counter layout user_variable variable updater update event engine widget

user_variable_getter::DVU.Vector Int->DVS.Vector Layout->Int->Int->Int
user_variable_getter user_variable layout clone_index int=case layout DVS.! int of
    Layout {address,size}->user_variable DVU.! (address+mod clone_index size)

run_kill_group::Int->Int->Int->DIM.IntMap (DS.Seq Int)->DIM.IntMap Program_counter->(DIM.IntMap Program_counter,DIM.IntMap (DS.Seq Int))
run_kill_group int index index_group_index index_group program_counter=if int<=index then (program_counter,index_group) else let (new_index_group,maybe_single_index_group)=intmap_delete_maybe_lookup (index_group_index+index) index_group in case maybe_single_index_group of
    Nothing->run_kill_group int (index+1) index_group_index new_index_group program_counter
    Just single_index_group->run_kill_group int (index+1) index_group_index new_index_group (DF.foldl' (flip DIM.delete) program_counter single_index_group)

run_create_group::DIM.IntMap Int->Int->Int->Int->Int->Int->DIM.IntMap (DS.Seq Int)->DIM.IntMap Program_counter->(DIM.IntMap Program_counter,DIM.IntMap (DS.Seq Int),Int)
run_create_group group_code_index int index clone_index program_counter_index index_group_index index_group program_counter=if int<=index then (program_counter,index_group,program_counter_index) else run_create_group group_code_index int (index+1) clone_index (program_counter_index+1) index_group_index (intmap_insert (index_group_index+index) (DS.singleton program_counter_index) index_group) (intmap_insert program_counter_index (Program_counter {code_index=intmap_lookup index group_code_index,clone_index=clone_index}) program_counter)

run_clone::Int->Int->Int->Int->Int->DS.Seq Int->DIM.IntMap Program_counter->(DIM.IntMap Program_counter,DS.Seq Int,Int)
run_clone int clone_number clone_index code_index program_counter_index main_index_group program_counter=if int<1 then (program_counter,main_index_group,program_counter_index) else run_clone (int-1) clone_number (clone_index+clone_number) code_index (program_counter_index+1) (main_index_group DS.|> program_counter_index) (intmap_insert program_counter_index (Program_counter {code_index=code_index,clone_index=clone_index}) program_counter)