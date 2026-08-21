{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Engine.Coroutine where

import Engine.Container
import Engine.Projection
import Engine.Selector
import Engine.Type
import Engine.Underlying
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

run_coroutine::Int->Selector a->DS.Seq Int->Event b->Engine c b d e f->Engine c b d e f
run_coroutine leaf_id selector index event engine=let (update,leaf)=intmap_functor_update leaf_id (functor_update_projection_object (selector_monad_update (const (DT.swap . run_coroutine_a index event engine)) selector)) engine.leaf in DF.foldl' (\this_engine this_update->this_update this_engine) (engine {leaf=leaf}) update

run_coroutine_a::DS.Seq Int->Event a->Engine b a c d e->Widget b a c d e->(Widget b a c d e,DS.Seq (Engine b a c d e->Engine b a c d e))
run_coroutine_a this_index event engine widget=case widget of
    Coroutine {index,initial_min_index,min_index,initial_max_index,max_index,variable_size,user_variable_size,layout_size,coroutine_state,layout,linear_coroutine,iterative}->let (update,new_coroutine_state)=DF.foldl' (\(this_update,this_coroutine_state) single_index->intmap_functor_update single_index (\this_this_coroutine_state->run_coroutine_b event engine iterative linear_coroutine layout this_this_coroutine_state layout_size this_update) this_coroutine_state) (DS.empty,coroutine_state) this_index in (Coroutine {index=index,initial_min_index=initial_min_index,min_index=min_index,initial_max_index=initial_max_index,max_index=max_index,variable_size=variable_size,user_variable_size=user_variable_size,layout_size=layout_size,coroutine_state=new_coroutine_state,layout=layout,linear_coroutine=linear_coroutine,iterative=iterative},update)
    _->EE.quick_error "run_coroutine_a" 0

run_coroutine_b::Event a->Engine b a c d e->Bool->DV.Vector (Linear_coroutine b a c d e)->DVS.Vector Layout->Coroutine_state b a c d e->Int->DS.Seq (Engine b a c d e->Engine b a c d e)->(DS.Seq (Engine b a c d e->Engine b a c d e),Coroutine_state b a c d e)
run_coroutine_b event engine iterative linear_coroutine layout coroutine_state layout_size update=case coroutine_state of
    Coroutine_state {widget,variable,user_variable,program_counter,index_group,main_index_group,index_group_index,program_counter_index}->CMST.runST $ do
        new_variable<-DVU.unsafeThaw variable
        (new_widget,_,new_update,new_new_variable,new_user_variable,new_program_counter,new_index_group,new_main_index_group,new_index_group_index,new_program_counter_index)<-step_coroutine linear_coroutine layout_size program_counter_index index_group_index DS.empty DS.empty main_index_group index_group program_counter layout user_variable new_variable (for_iterative iterative) DS.empty event (if iterative then DF.foldl' (\this_engine this_update->this_update this_engine) engine update else engine) widget
        new_new_new_variable<-DVU.unsafeFreeze new_new_variable
        return (update DS.>< new_update,Coroutine_state {widget=new_widget,variable=new_new_new_variable,user_variable=new_user_variable,program_counter=new_program_counter,index_group=new_index_group,main_index_group=new_main_index_group,index_group_index=new_index_group_index,program_counter_index=new_program_counter_index})

for_iterative::Bool->(Engine a b c d e->Engine a b c d e)->Engine a b c d e->Engine a b c d e
for_iterative iterative update engine=if iterative then update engine else engine

init_coroutine_state::Int->Int->Widget a b c d e->Coroutine_state a b c d e
init_coroutine_state variable_size user_variable_size widget=Coroutine_state {widget=widget,variable=DVU.replicate variable_size 0,user_variable=DVU.replicate user_variable_size 0,program_counter=DIM.singleton 0 (Program_counter {code_index=0,clone_index=0}),index_group=DIM.empty,main_index_group=DS.singleton 0,index_group_index=1,program_counter_index=1}

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
from_coroutine coroutine layout_size=CMST.runST $ do
    layout<-DVSM.replicate layout_size (Layout {address=0,size=0})
    (linear_coroutine,new_layout,int_index,user_int_index,code_index,_)<-from_coroutine_a coroutine layout_size 1 0 0 0 layout DIM.empty
    new_new_layout<-DVS.unsafeFreeze new_layout
    return (DV.generate (code_index+1) (\this_code_index->if code_index==this_code_index then Linear_end else intmap_lookup this_code_index linear_coroutine),new_new_layout,int_index,user_int_index)

from_coroutine_a::Coroutine a b c d e->Int->Int->Int->Int->Int->DVSM.MVector f Layout->DIM.IntMap (Linear_coroutine a b c d e)->CMST.ST f (DIM.IntMap (Linear_coroutine a b c d e),DVSM.MVector f Layout,Int,Int,Int,DIM.IntMap Int)
from_coroutine_a this_coroutine layout_size clone_number code_index user_int_index int_index layout linear_coroutine=case this_coroutine of
    Done->return (linear_coroutine,layout,int_index,user_int_index,code_index,DIM.empty)
    Emit {emit}->return (DIM.insert code_index (Linear_emit {emit=emit}) linear_coroutine,layout,int_index,user_int_index,code_index+1,DIM.empty)
    Wait {dynamic_int}->return (DIM.insert (code_index+1) (Linear_wait {int_index=int_index}) (DIM.insert code_index (Linear_dynamic_int {int_index=int_index,dynamic_int=dynamic_int}) linear_coroutine),layout,int_index+clone_number,user_int_index,code_index+2,DIM.empty)
    Forever {coroutine}->do
        (new_linear_coroutine,new_layout,new_int_index,new_user_int_index,new_code_index,group_int_index)<-from_coroutine_a coroutine layout_size clone_number code_index user_int_index int_index layout linear_coroutine
        return (DIM.insert new_code_index (Linear_jump {code_index=code_index}) new_linear_coroutine,new_layout,new_int_index,new_user_int_index,new_code_index+1,group_int_index)
    Then {coroutine_sequence}->then_coroutine coroutine_sequence layout_size clone_number code_index user_int_index int_index layout linear_coroutine
    While {dynamic_bool,coroutine}->do
        (new_linear_coroutine,new_layout,new_int_index,new_user_int_index,new_code_index,group_int_index)<-from_coroutine_a coroutine layout_size clone_number (code_index+1) user_int_index int_index layout linear_coroutine
        let end_code_index=new_code_index+1 in return (DIM.insert new_code_index (Linear_jump {code_index=code_index}) (DIM.insert code_index (Linear_false_jump {code_index=end_code_index,dynamic_bool=dynamic_bool}) new_linear_coroutine),new_layout,new_int_index,new_user_int_index,end_code_index,group_int_index)
    Pause {dynamic_bool,coroutine}->let body_code_index=code_index+2 in do
        (new_linear_coroutine,new_layout,new_int_index,new_user_int_index,new_code_index,group_int_index)<-from_coroutine_a coroutine layout_size clone_number body_code_index user_int_index (int_index+2*clone_number) layout linear_coroutine
        let loop_code_index=new_code_index+1 in let yield_code_index=loop_code_index+3 in let jump_code_index=loop_code_index+4 in let countdown_int_index=int_index+clone_number in let new_group_int_index=DIM.insert int_index 1 group_int_index in return (DIM.insert jump_code_index (Linear_kill {group_int_index=new_group_int_index}) (DIM.insert yield_code_index (Linear_yield {code_index=loop_code_index}) (DIM.insert (loop_code_index+2) (Linear_wake {int_index=int_index}) (DIM.insert (loop_code_index+1) (Linear_true_jump {code_index=yield_code_index,dynamic_bool=dynamic_bool}) (DIM.insert loop_code_index (Linear_less_jump {int_index=countdown_int_index,code_index=jump_code_index,int=1}) (DIM.insert new_code_index (Linear_countdown {int_index=countdown_int_index}) (DIM.insert (code_index+1) (Linear_jump {code_index=loop_code_index}) (DIM.insert code_index (Linear_create_group {first_int_index=int_index,second_int_index=countdown_int_index,group_code_index=DIM.singleton 0 body_code_index,int=1}) new_linear_coroutine))))))),new_layout,new_int_index,new_user_int_index,loop_code_index+5,new_group_int_index)
    Skip {dynamic_bool,coroutine}->let body_code_index=code_index+2 in do
        (new_linear_coroutine,new_layout,new_int_index,new_user_int_index,new_code_index,group_int_index)<-from_coroutine_a coroutine layout_size clone_number body_code_index user_int_index (int_index+2*clone_number) layout linear_coroutine
        let loop_code_index=new_code_index+1 in let jump_code_index=loop_code_index+4 in let countdown_int_index=int_index+clone_number in let new_group_int_index=DIM.insert int_index 1 group_int_index in return (DIM.insert jump_code_index (Linear_kill {group_int_index=new_group_int_index}) (DIM.insert (loop_code_index+3) (Linear_yield {code_index=loop_code_index}) (DIM.insert (loop_code_index+2) (Linear_wake {int_index=int_index}) (DIM.insert (loop_code_index+1) (Linear_true_jump {code_index=jump_code_index,dynamic_bool=dynamic_bool}) (DIM.insert loop_code_index (Linear_less_jump {int_index=countdown_int_index,code_index=jump_code_index,int=1}) (DIM.insert new_code_index (Linear_countdown {int_index=countdown_int_index}) (DIM.insert (code_index+1) (Linear_jump {code_index=loop_code_index}) (DIM.insert code_index (Linear_create_group {first_int_index=int_index,second_int_index=countdown_int_index,group_code_index=DIM.singleton 0 body_code_index,int=1}) new_linear_coroutine))))))),new_layout,new_int_index,new_user_int_index,loop_code_index+5,new_group_int_index)
    Assign {dynamic_int,int}->let new_int=catch_out 0 layout_size int in do
        single_layout<-DVSM.read layout new_int
        case single_layout of
            Layout {address,size}->if size==0
                then do
                    DVSM.write layout new_int (Layout {address=user_int_index,size=clone_number})
                    return (DIM.insert code_index (Linear_assign {user_int_index=user_int_index,clone_number=clone_number,dynamic_int=dynamic_int}) linear_coroutine,layout,int_index,user_int_index+clone_number,code_index+1,DIM.empty)
                else return (DIM.insert code_index (Linear_assign {user_int_index=address,clone_number=size,dynamic_int=dynamic_int}) linear_coroutine,layout,int_index,user_int_index,code_index+1,DIM.empty)
    Repeat {dynamic_int,coroutine}->let loop_code_index=code_index+2 in do
        (new_linear_coroutine,new_layout,new_int_index,new_user_int_index,new_code_index,group_int_index)<-from_coroutine_a coroutine layout_size clone_number loop_code_index user_int_index (int_index+clone_number) layout linear_coroutine
        let end_code_index=new_code_index+1 in return (DIM.insert new_code_index (Linear_one_more_jump {int_index=int_index,code_index=loop_code_index}) (DIM.insert (code_index+1) (Linear_one_less_jump {int_index=int_index,code_index=end_code_index}) (DIM.insert code_index (Linear_dynamic_int {int_index=int_index,dynamic_int=dynamic_int}) new_linear_coroutine)),new_layout,new_int_index,new_user_int_index,end_code_index,group_int_index)
    Clone {int,coroutine}->if int<=0 then EE.quick_error "from_coroutine_a" 0 else do
        (new_linear_coroutine,new_layout,new_int_index,new_user_int_index,new_code_index,group_int_index)<-from_coroutine_a coroutine layout_size (int*clone_number) (code_index+1) user_int_index (int_index+clone_number) layout linear_coroutine
        return (DIM.insert new_code_index (Linear_kill_clone {int_index=int_index,clone_number=clone_number}) (DIM.insert code_index (Linear_clone {int_index=int_index,clone_number=clone_number,int=int-1}) new_linear_coroutine),new_layout,new_int_index,new_user_int_index,new_code_index+1,clone_coroutine group_int_index int clone_number)
    If {dynamic_bool,first_coroutine,second_coroutine}->do
        (first_linear_coroutine,first_layout,first_int_index,first_user_int_index,first_code_index,first_group_int_index)<-from_coroutine_a first_coroutine layout_size clone_number (code_index+1) user_int_index int_index layout linear_coroutine
        let new_code_index=first_code_index+1
        (second_linear_coroutine,second_layout,second_int_index,second_user_int_index,second_code_index,second_group_int_index)<-from_coroutine_a second_coroutine layout_size clone_number new_code_index first_user_int_index first_int_index first_layout first_linear_coroutine
        return (DIM.insert first_code_index (Linear_jump {code_index=second_code_index}) (DIM.insert code_index (Linear_false_jump {code_index=new_code_index,dynamic_bool=dynamic_bool}) second_linear_coroutine),second_layout,second_int_index,second_user_int_index,second_code_index,DIM.union first_group_int_index second_group_int_index)
    Dynamic_clone {dynamic_int,int,coroutine}->if int<=0 then EE.quick_error "from_coroutine_a" 1 else do
        (new_linear_coroutine,new_layout,new_int_index,new_user_int_index,new_code_index,group_int_index)<-from_coroutine_a coroutine layout_size (int*clone_number) (code_index+1) user_int_index (int_index+clone_number) layout linear_coroutine
        let kill_code_index=new_code_index+1 in return (DIM.insert new_code_index (Linear_kill_clone {int_index=int_index,clone_number=clone_number}) (DIM.insert code_index (Linear_dynamic_clone {int_index=int_index,code_index=kill_code_index,clone_number=clone_number,dynamic_int=dynamic_int,int=int-1}) new_linear_coroutine),new_layout,new_int_index,new_user_int_index,kill_code_index,clone_coroutine group_int_index int clone_number)
    Case {dynamic_int,int,coroutine_sequence}->do
        (new_linear_coroutine,new_layout,new_int_index,new_user_int_index,new_code_index,single_linear_coroutine,group_int_index)<-case_coroutine DIM.empty DIS.empty coroutine_sequence layout_size int 0 (int_index+clone_number) clone_number (code_index+2) user_int_index int_index layout linear_coroutine
        return (DIM.insert (code_index+1) single_linear_coroutine (DIM.insert code_index (Linear_dynamic_int {int_index=int_index,dynamic_int=dynamic_int}) new_linear_coroutine),new_layout,new_int_index,new_user_int_index,new_code_index,group_int_index)
    Fork {int,coroutine,coroutine_sequence}->if int<=0 then EE.quick_error "from_coroutine_a" 2 else do
        (new_linear_coroutine,new_layout,new_int_index,new_user_int_index,new_code_index,main_group_int_index)<-from_coroutine_a coroutine layout_size clone_number (code_index+2) user_int_index (int_index+2*clone_number) layout linear_coroutine
        let loop_code_index=new_code_index
        let kill_code_index=loop_code_index+2
        let jump_code_index=kill_code_index+1
        let countdown_int_index=int_index+clone_number
        (final_linear_coroutine,final_layout,final_int_index,final_user_int_index,fork_end_code_index,fork_group_int_index,group_code_index)<-fork_coroutine coroutine_sequence layout_size 0 countdown_int_index clone_number (jump_code_index+1) new_user_int_index new_int_index new_layout new_linear_coroutine
        let all_group_int_index=DIM.insert int_index int (DIM.union main_group_int_index fork_group_int_index) in return (DIM.insert jump_code_index (Linear_jump {code_index=fork_end_code_index}) (DIM.insert kill_code_index (Linear_kill {group_int_index=all_group_int_index}) (DIM.insert (loop_code_index+1) (Linear_yield {code_index=loop_code_index}) (DIM.insert loop_code_index (Linear_less_jump {int_index=countdown_int_index,code_index=kill_code_index,int=1}) (DIM.insert (code_index+1) (Linear_jump {code_index=code_index+2}) (DIM.insert code_index (Linear_create_active_group {first_int_index=int_index,second_int_index=countdown_int_index,group_code_index=group_code_index,int=int}) final_linear_coroutine))))),final_layout,final_int_index,final_user_int_index,fork_end_code_index,all_group_int_index)
    Race {dynamic_int,first_int,second_int,coroutine_sequence}->let countdown_int_index=int_index+clone_number in do
        (new_linear_coroutine,new_layout,new_int_index,new_user_int_index,new_code_index,group_int_index,group_code_index)<-race_coroutine coroutine_sequence layout_size 0 countdown_int_index clone_number (code_index+2) user_int_index (int_index+2*clone_number) layout linear_coroutine
        let kill_code_index=new_code_index+3 in let new_group_int_index=DIM.insert int_index second_int group_int_index in return (DIM.insert kill_code_index (Linear_kill {group_int_index=new_group_int_index}) (DIM.insert (new_code_index+2) (Linear_yield {code_index=new_code_index}) (DIM.insert (new_code_index+1) (Linear_wake_group {int_index=int_index,dynamic_int=dynamic_int,int=second_int}) (DIM.insert new_code_index (Linear_less_jump {int_index=countdown_int_index,code_index=kill_code_index,int=second_int-first_int+1}) (DIM.insert (code_index+1) (Linear_jump {code_index=new_code_index}) (DIM.insert code_index (Linear_create_group {first_int_index=int_index,second_int_index=countdown_int_index,group_code_index=group_code_index,int=second_int}) new_linear_coroutine))))),new_layout,new_int_index,new_user_int_index,new_code_index+4,new_group_int_index)

then_coroutine::DS.Seq (Coroutine a b c d e)->Int->Int->Int->Int->Int->DVSM.MVector f Layout->DIM.IntMap (Linear_coroutine a b c d e)->CMST.ST f (DIM.IntMap (Linear_coroutine a b c d e),DVSM.MVector f Layout,Int,Int,Int,DIM.IntMap Int)
then_coroutine coroutine_sequence layout_size clone_number code_index user_int_index int_index layout linear_coroutine=case coroutine_sequence of
    DS.Empty->return (linear_coroutine,layout,int_index,user_int_index,code_index,DIM.empty)
    coroutine DS.:<| other_coroutine->do
        (new_linear_coroutine,new_layout,new_int_index,new_user_int_index,new_code_index,group_int_index)<-from_coroutine_a coroutine layout_size clone_number code_index user_int_index int_index layout linear_coroutine
        (final_linear_coroutine,final_layout,final_int_index,final_user_int_index,final_code_index,final_group_int_index)<-then_coroutine other_coroutine layout_size clone_number new_code_index new_user_int_index new_int_index new_layout new_linear_coroutine
        return (final_linear_coroutine,final_layout,final_int_index,final_user_int_index,final_code_index,DIM.union group_int_index final_group_int_index)

clone_coroutine::DIM.IntMap Int->Int->Int->DIM.IntMap Int
clone_coroutine group_int_index int clone_number=DIM.foldlWithKey' (\this_group_int_index int_index size->clone_coroutine_a int 0 int_index clone_number size this_group_int_index) DIM.empty group_int_index

clone_coroutine_a::Int->Int->Int->Int->Int->DIM.IntMap Int->DIM.IntMap Int
clone_coroutine_a int index int_index clone_number size this_group_int_index=if int<=index then this_group_int_index else clone_coroutine_a int (index+1) int_index clone_number size (DIM.insert (int_index+index*clone_number) size this_group_int_index)

case_coroutine::DIM.IntMap Int->DIS.IntSet->DS.Seq (Coroutine a b c d e)->Int->Int->Int->Int->Int->Int->Int->Int->DVSM.MVector f Layout->DIM.IntMap (Linear_coroutine a b c d e)->CMST.ST f (DIM.IntMap (Linear_coroutine a b c d e),DVSM.MVector f Layout,Int,Int,Int,Linear_coroutine a b c d e,DIM.IntMap Int)
case_coroutine case_code_index jump_code_index coroutine_sequence layout_size int index max_int_index clone_number code_index user_int_index int_index layout linear_coroutine=case coroutine_sequence of
    DS.Empty->let (new_linear_coroutine,new_code_index)=case_coroutine_a case_code_index int 0 code_index int_index linear_coroutine in return (DIS.foldl' (\this_linear_coroutine this_code_index->DIM.insert this_code_index (Linear_jump {code_index=new_code_index}) this_linear_coroutine) new_linear_coroutine jump_code_index,layout,max_int_index,user_int_index,new_code_index,Linear_jump {code_index=code_index},DIM.empty)
    coroutine DS.:<| other_coroutine->do
        (new_linear_coroutine,new_layout,new_int_index,new_user_int_index,new_code_index,group_int_index)<-from_coroutine_a coroutine layout_size clone_number code_index user_int_index max_int_index layout linear_coroutine
        (final_linear_coroutine,final_layout,final_int_index,final_user_int_index,final_code_index,single_linear_coroutine,final_group_int_index)<-case_coroutine (DIM.insert index code_index case_code_index) (DIS.insert new_code_index jump_code_index) other_coroutine layout_size int (index+1) new_int_index clone_number (new_code_index+1) new_user_int_index int_index new_layout new_linear_coroutine
        return (final_linear_coroutine,final_layout,final_int_index,final_user_int_index,final_code_index,single_linear_coroutine,DIM.union group_int_index final_group_int_index)

case_coroutine_a::DIM.IntMap Int->Int->Int->Int->Int->DIM.IntMap (Linear_coroutine a b c d e)->(DIM.IntMap (Linear_coroutine a b c d e),Int)
case_coroutine_a case_code_index int index code_index int_index linear_coroutine=case int of
    1->(DIM.insert code_index (Linear_jump {code_index=intmap_lookup index case_code_index}) linear_coroutine,code_index+1)
    _->let half_int=div int 2 in let new_int=index+half_int in let (right_linear_coroutine,right_code_index)=case_coroutine_a case_code_index (int-half_int) new_int (code_index+1) int_index linear_coroutine in let (left_linear_coroutine,left_code_index)=case_coroutine_a case_code_index half_int index right_code_index int_index right_linear_coroutine in (DIM.insert code_index (Linear_less_jump {int_index=int_index,code_index=right_code_index,int=new_int}) left_linear_coroutine,left_code_index)

fork_coroutine::DS.Seq (Coroutine a b c d e)->Int->Int->Int->Int->Int->Int->Int->DVSM.MVector f Layout->DIM.IntMap (Linear_coroutine a b c d e)->CMST.ST f (DIM.IntMap (Linear_coroutine a b c d e),DVSM.MVector f Layout,Int,Int,Int,DIM.IntMap Int,DIM.IntMap Int)
fork_coroutine coroutine_sequence layout_size index countdown_int_index clone_number code_index user_int_index int_index layout linear_coroutine=case coroutine_sequence of
    DS.Empty->return (linear_coroutine,layout,int_index,user_int_index,code_index,DIM.empty,DIM.empty)
    coroutine DS.:<| other_coroutine->do
        (new_linear_coroutine,new_layout,new_int_index,new_user_int_index,new_code_index,group_int_index)<-from_coroutine_a coroutine layout_size clone_number code_index user_int_index int_index layout linear_coroutine
        (final_linear_coroutine,final_layout,final_int_index,final_user_int_index,final_code_index,final_group_int_index,group_code_index)<-fork_coroutine other_coroutine layout_size (index+1) countdown_int_index clone_number (new_code_index+1) new_user_int_index (max int_index new_int_index) new_layout (DIM.insert new_code_index (Linear_countdown {int_index=countdown_int_index}) new_linear_coroutine)
        return (final_linear_coroutine,final_layout,final_int_index,final_user_int_index,final_code_index,DIM.union group_int_index final_group_int_index,DIM.insert index code_index group_code_index)

race_coroutine::DS.Seq (Coroutine a b c d e)->Int->Int->Int->Int->Int->Int->Int->DVSM.MVector f Layout->DIM.IntMap (Linear_coroutine a b c d e)->CMST.ST f (DIM.IntMap (Linear_coroutine a b c d e),DVSM.MVector f Layout,Int,Int,Int,DIM.IntMap Int,DIM.IntMap Int)
race_coroutine coroutine_sequence layout_size index countdown_int_index clone_number code_index user_int_index int_index layout linear_coroutine=case coroutine_sequence of
    DS.Empty->return (linear_coroutine,layout,int_index,user_int_index,code_index,DIM.empty,DIM.empty)
    coroutine DS.:<| other_coroutine->do
        (new_linear_coroutine,new_layout,new_int_index,new_user_int_index,new_code_index,group_int_index)<-from_coroutine_a coroutine layout_size clone_number code_index user_int_index int_index layout linear_coroutine
        (final_linear_coroutine,final_layout,final_int_index,final_user_int_index,final_code_index,final_group_int_index,group_code_index)<-race_coroutine other_coroutine layout_size (index+1) countdown_int_index clone_number (new_code_index+1) new_user_int_index (max int_index new_int_index) new_layout (DIM.insert new_code_index (Linear_countdown {int_index=countdown_int_index}) new_linear_coroutine)
        return (final_linear_coroutine,final_layout,final_int_index,final_user_int_index,final_code_index,DIM.union group_int_index final_group_int_index,DIM.insert index code_index group_code_index)

step_coroutine::DV.Vector (Linear_coroutine a b c d e)->Int->Int->Int->DS.Seq Int->DS.Seq Int->DS.Seq Int->DIM.IntMap (DS.Seq Int)->DIM.IntMap Program_counter->DVS.Vector Layout->DVU.Vector Int->DVUM.MVector f Int->((Engine a b c d e->Engine a b c d e)->Engine a b c d e->Engine a b c d e)->DS.Seq (Engine a b c d e->Engine a b c d e)->Event b->Engine a b c d e->Widget a b c d e->CMST.ST f (Widget a b c d e,Engine a b c d e,DS.Seq (Engine a b c d e->Engine a b c d e),DVUM.MVector f Int,DVU.Vector Int,DIM.IntMap Program_counter,DIM.IntMap (DS.Seq Int),DS.Seq Int,Int,Int)
step_coroutine linear_coroutine layout_size program_counter_index index_group_index survived_main_index_group newborn_main_index_group main_index_group index_group program_counter layout user_variable variable updater update event engine widget=case newborn_main_index_group of
    DS.Empty->case main_index_group of
        DS.Empty->return (widget,engine,update,variable,user_variable,program_counter,index_group,survived_main_index_group,index_group_index,program_counter_index)
        (main_index DS.:<| other_main_index)->let (new_program_counter,maybe_single_program_counter)=intmap_delete_maybe_lookup main_index program_counter in case maybe_single_program_counter of
            Nothing->step_coroutine linear_coroutine layout_size program_counter_index index_group_index survived_main_index_group DS.empty other_main_index index_group new_program_counter layout user_variable variable updater update event engine widget
            Just single_program_counter->step_coroutine_a linear_coroutine layout_size main_index single_program_counter.clone_index single_program_counter.code_index program_counter_index index_group_index survived_main_index_group DS.empty other_main_index index_group new_program_counter layout user_variable variable updater update event engine widget
    (main_index DS.:<| other_main_index)->let (new_program_counter,maybe_single_program_counter)=intmap_delete_maybe_lookup main_index program_counter in case maybe_single_program_counter of
        Nothing->step_coroutine linear_coroutine layout_size program_counter_index index_group_index survived_main_index_group DS.empty (other_main_index DS.>< main_index_group) index_group new_program_counter layout user_variable variable updater update event engine widget
        Just single_program_counter->step_coroutine_a linear_coroutine layout_size main_index single_program_counter.clone_index single_program_counter.code_index program_counter_index index_group_index survived_main_index_group DS.empty (other_main_index DS.>< main_index_group) index_group new_program_counter layout user_variable variable updater update event engine widget

step_coroutine_a::DV.Vector (Linear_coroutine a b c d e)->Int->Int->Int->Int->Int->Int->DS.Seq Int->DS.Seq Int->DS.Seq Int->DIM.IntMap (DS.Seq Int)->DIM.IntMap Program_counter->DVS.Vector Layout->DVU.Vector Int->DVUM.MVector f Int->((Engine a b c d e->Engine a b c d e)->Engine a b c d e->Engine a b c d e)->DS.Seq (Engine a b c d e->Engine a b c d e)->Event b->Engine a b c d e->Widget a b c d e->CMST.ST f (Widget a b c d e,Engine a b c d e,DS.Seq (Engine a b c d e->Engine a b c d e),DVUM.MVector f Int,DVU.Vector Int,DIM.IntMap Program_counter,DIM.IntMap (DS.Seq Int),DS.Seq Int,Int,Int)
step_coroutine_a linear_coroutine layout_size main_index clone_index this_code_index program_counter_index index_group_index survived_main_index_group newborn_main_index_group main_index_group index_group program_counter layout user_variable variable updater update event engine widget=case linear_coroutine DV.! this_code_index of
    Linear_end->step_coroutine linear_coroutine layout_size program_counter_index index_group_index survived_main_index_group newborn_main_index_group main_index_group index_group program_counter layout user_variable variable updater update event engine widget
    Linear_emit {emit}->let (new_widget,new_update)=emit event engine widget in step_coroutine_a linear_coroutine layout_size main_index clone_index (this_code_index+1) program_counter_index index_group_index survived_main_index_group newborn_main_index_group main_index_group index_group program_counter layout user_variable variable updater (update DS.|> new_update) event (updater new_update engine) new_widget
    Linear_wait {int_index}->do
        int<-DVUM.read variable (int_index+clone_index)
        DVUM.write variable (int_index+clone_index) (int-1)
        if 0<int then step_coroutine linear_coroutine layout_size program_counter_index index_group_index (survived_main_index_group DS.|> main_index) newborn_main_index_group main_index_group index_group (intmap_insert main_index (Program_counter {code_index=this_code_index,clone_index=clone_index}) program_counter) layout user_variable variable updater update event engine widget else step_coroutine_a linear_coroutine layout_size main_index clone_index (this_code_index+1) program_counter_index index_group_index survived_main_index_group newborn_main_index_group main_index_group index_group program_counter layout user_variable variable updater update event engine widget
    Linear_countdown {int_index}->do
        DVUM.modify variable (\int->int-1) (int_index+clone_index)
        step_coroutine linear_coroutine layout_size program_counter_index index_group_index survived_main_index_group newborn_main_index_group main_index_group index_group program_counter layout user_variable variable updater update event engine widget
    Linear_wake {int_index}->do
        int<-DVUM.read variable (int_index+clone_index)
        let (new_index_group,maybe_single_index_group)=intmap_delete_maybe_lookup int index_group in case maybe_single_index_group of
            Nothing->step_coroutine_a linear_coroutine layout_size main_index clone_index (this_code_index+1) program_counter_index index_group_index survived_main_index_group newborn_main_index_group main_index_group index_group program_counter layout user_variable variable updater update event engine widget
            Just single_index_group->do
                (new_widget,new_engine,new_update,new_variable,new_user_variable,new_program_counter,new_new_index_group,new_survived_main_index_group,new_index_group_index,new_program_counter_index)<-step_coroutine linear_coroutine layout_size program_counter_index index_group_index DS.empty DS.empty single_index_group new_index_group program_counter layout user_variable variable updater DS.empty event engine widget
                alive<-DVUM.read new_variable (int_index+clone_index)
                if alive==(-1) then step_coroutine_a linear_coroutine layout_size main_index clone_index (this_code_index+1) new_program_counter_index new_index_group_index survived_main_index_group newborn_main_index_group main_index_group new_new_index_group (DF.foldl' (flip DIM.delete) new_program_counter new_survived_main_index_group) layout new_user_variable new_variable updater (update DS.>< new_update) event new_engine new_widget else step_coroutine_a linear_coroutine layout_size main_index clone_index (this_code_index+1) new_program_counter_index new_index_group_index survived_main_index_group newborn_main_index_group main_index_group (if DS.null new_survived_main_index_group then new_new_index_group else DIM.insert int new_survived_main_index_group new_new_index_group) new_program_counter layout new_user_variable new_variable updater (update DS.>< new_update) event new_engine new_widget
    Linear_fork {code_index}->step_coroutine_a linear_coroutine layout_size main_index clone_index (this_code_index+1) (program_counter_index+1) index_group_index survived_main_index_group (newborn_main_index_group DS.|> program_counter_index) main_index_group index_group (intmap_insert program_counter_index (Program_counter {code_index=code_index,clone_index=clone_index}) program_counter) layout user_variable variable updater update event engine widget
    Linear_yield {code_index}->step_coroutine linear_coroutine layout_size program_counter_index index_group_index (survived_main_index_group DS.|> main_index) newborn_main_index_group main_index_group index_group (intmap_insert main_index (Program_counter {code_index=code_index,clone_index=clone_index}) program_counter) layout user_variable variable updater update event engine widget
    Linear_jump {code_index}->step_coroutine_a linear_coroutine layout_size main_index clone_index code_index program_counter_index index_group_index survived_main_index_group newborn_main_index_group main_index_group index_group program_counter layout user_variable variable updater update event engine widget
    Linear_kill {group_int_index}->do
        (new_program_counter,new_index_group)<-intmap_monad_fold (\int_index int (this_program_counter,this_index_group)->run_kill_group int int_index clone_index variable this_program_counter this_index_group) group_int_index (program_counter,index_group)
        step_coroutine_a linear_coroutine layout_size main_index clone_index (this_code_index+1) program_counter_index index_group_index survived_main_index_group newborn_main_index_group main_index_group new_index_group new_program_counter layout user_variable variable updater update event engine widget
    Linear_one_less_jump {int_index,code_index}->do
        int<-DVUM.read variable (int_index+clone_index)
        if int<1 then step_coroutine_a linear_coroutine layout_size main_index clone_index code_index program_counter_index index_group_index survived_main_index_group newborn_main_index_group main_index_group index_group program_counter layout user_variable variable updater update event engine widget else step_coroutine_a linear_coroutine layout_size main_index clone_index (this_code_index+1) program_counter_index index_group_index survived_main_index_group newborn_main_index_group main_index_group index_group program_counter layout user_variable variable updater update event engine widget
    Linear_one_more_jump {int_index,code_index}->do
        int<-DVUM.read variable (int_index+clone_index)
        DVUM.write variable (int_index+clone_index) (int-1)
        if 1<int then step_coroutine_a linear_coroutine layout_size main_index clone_index code_index program_counter_index index_group_index survived_main_index_group newborn_main_index_group main_index_group index_group program_counter layout user_variable variable updater update event engine widget else step_coroutine_a linear_coroutine layout_size main_index clone_index (this_code_index+1) program_counter_index index_group_index survived_main_index_group newborn_main_index_group main_index_group index_group program_counter layout user_variable variable updater update event engine widget
    Linear_kill_clone {int_index,clone_number}->let new_clone_index=mod clone_index clone_number in do
        int<-DVUM.read variable (int_index+new_clone_index)
        DVUM.write variable (int_index+new_clone_index) (int-1)
        if 0<int then step_coroutine linear_coroutine layout_size program_counter_index index_group_index survived_main_index_group newborn_main_index_group main_index_group index_group program_counter layout user_variable variable updater update event engine widget else step_coroutine_a linear_coroutine layout_size main_index new_clone_index (this_code_index+1) program_counter_index index_group_index survived_main_index_group newborn_main_index_group main_index_group index_group program_counter layout user_variable variable updater update event engine widget
    Linear_dynamic_int {int_index,dynamic_int}->do
        DVUM.write variable (int_index+clone_index) (dynamic_int.dynamic_int (user_variable_getter user_variable layout layout_size clone_index) event engine widget)
        step_coroutine_a linear_coroutine layout_size main_index clone_index (this_code_index+1) program_counter_index index_group_index survived_main_index_group newborn_main_index_group main_index_group index_group program_counter layout user_variable variable updater update event engine widget
    Linear_true_jump {code_index,dynamic_bool}->if dynamic_bool.dynamic_bool (user_variable_getter user_variable layout layout_size clone_index) event engine widget then step_coroutine_a linear_coroutine layout_size main_index clone_index code_index program_counter_index index_group_index survived_main_index_group newborn_main_index_group main_index_group index_group program_counter layout user_variable variable updater update event engine widget else step_coroutine_a linear_coroutine layout_size main_index clone_index (this_code_index+1) program_counter_index index_group_index survived_main_index_group newborn_main_index_group main_index_group index_group program_counter layout user_variable variable updater update event engine widget
    Linear_false_jump {code_index,dynamic_bool}->if dynamic_bool.dynamic_bool (user_variable_getter user_variable layout layout_size clone_index) event engine widget then step_coroutine_a linear_coroutine layout_size main_index clone_index (this_code_index+1) program_counter_index index_group_index survived_main_index_group newborn_main_index_group main_index_group index_group program_counter layout user_variable variable updater update event engine widget else step_coroutine_a linear_coroutine layout_size main_index clone_index code_index program_counter_index index_group_index survived_main_index_group newborn_main_index_group main_index_group index_group program_counter layout user_variable variable updater update event engine widget
    Linear_less_jump {int_index,code_index,int}->do
        new_int<-DVUM.read variable (int_index+clone_index)
        if new_int<int then step_coroutine_a linear_coroutine layout_size main_index clone_index code_index program_counter_index index_group_index survived_main_index_group newborn_main_index_group main_index_group index_group program_counter layout user_variable variable updater update event engine widget else step_coroutine_a linear_coroutine layout_size main_index clone_index (this_code_index+1) program_counter_index index_group_index survived_main_index_group newborn_main_index_group main_index_group index_group program_counter layout user_variable variable updater update event engine widget
    Linear_clone {int_index,clone_number,int}->do
        DVUM.write variable (int_index+clone_index) int
        let new_code_index=this_code_index+1 in let (new_program_counter,new_newborn_main_index_group,new_program_counter_index)=run_clone int clone_number (clone_index+clone_number) new_code_index program_counter_index newborn_main_index_group program_counter in step_coroutine_a linear_coroutine layout_size main_index clone_index new_code_index new_program_counter_index index_group_index survived_main_index_group new_newborn_main_index_group main_index_group index_group new_program_counter layout user_variable variable updater update event engine widget
    Linear_wake_group {int_index,dynamic_int,int}->do
        new_int<-DVUM.read variable (int_index+clone_index)
        let new_index_group_index=new_int+catch_out 0 int (dynamic_int.dynamic_int (user_variable_getter user_variable layout layout_size clone_index) event engine widget) in let (new_index_group,maybe_single_index_group)=intmap_delete_maybe_lookup new_index_group_index index_group in case maybe_single_index_group of
            Nothing->step_coroutine_a linear_coroutine layout_size main_index clone_index (this_code_index+1) program_counter_index index_group_index survived_main_index_group newborn_main_index_group main_index_group index_group program_counter layout user_variable variable updater update event engine widget
            Just single_index_group->do
                (new_widget,new_engine,new_update,new_variable,new_user_variable,new_program_counter,new_new_index_group,new_survived_main_index_group,new_new_index_group_index,new_program_counter_index)<-step_coroutine linear_coroutine layout_size program_counter_index index_group_index DS.empty DS.empty single_index_group new_index_group program_counter layout user_variable variable updater DS.empty event engine widget
                alive<-DVUM.read new_variable (int_index+clone_index)
                if alive==(-1) then step_coroutine_a linear_coroutine layout_size main_index clone_index (this_code_index+1) new_program_counter_index new_new_index_group_index survived_main_index_group newborn_main_index_group main_index_group new_new_index_group (DF.foldl' (flip DIM.delete) new_program_counter new_survived_main_index_group) layout new_user_variable new_variable updater (update DS.>< new_update) event new_engine new_widget else step_coroutine_a linear_coroutine layout_size main_index clone_index (this_code_index+1) new_program_counter_index new_new_index_group_index survived_main_index_group newborn_main_index_group main_index_group (if DS.null new_survived_main_index_group then new_new_index_group else DIM.insert new_index_group_index new_survived_main_index_group new_new_index_group) new_program_counter layout new_user_variable new_variable updater (update DS.>< new_update) event new_engine new_widget
    Linear_assign {user_int_index,clone_number,dynamic_int}->do
        new_user_variable<-DVU.unsafeThaw user_variable
        DVUM.write new_user_variable (user_int_index+mod clone_index clone_number) (dynamic_int.dynamic_int (user_variable_getter user_variable layout layout_size clone_index) event engine widget)
        new_new_user_variable<-DVU.unsafeFreeze new_user_variable
        step_coroutine_a linear_coroutine layout_size main_index clone_index (this_code_index+1) program_counter_index index_group_index survived_main_index_group newborn_main_index_group main_index_group index_group program_counter layout new_new_user_variable variable updater update event engine widget
    Linear_create_group {first_int_index,second_int_index,group_code_index,int}->do
        DVUM.write variable (first_int_index+clone_index) index_group_index
        DVUM.write variable (second_int_index+clone_index) int
        let (new_program_counter,new_index_group,new_program_counter_index)=run_create_group group_code_index int 0 clone_index program_counter_index index_group_index index_group program_counter in step_coroutine_a linear_coroutine layout_size main_index clone_index (this_code_index+1) new_program_counter_index (index_group_index+int) survived_main_index_group newborn_main_index_group main_index_group new_index_group new_program_counter layout user_variable variable updater update event engine widget
    Linear_create_active_group {first_int_index,second_int_index,group_code_index,int}->do
        DVUM.write variable (first_int_index+clone_index) index_group_index
        DVUM.write variable (second_int_index+clone_index) int
        let (new_program_counter,new_index_group,new_program_counter_index,new_newborn_main_index_group)=run_create_active_group group_code_index int 0 clone_index program_counter_index index_group_index newborn_main_index_group index_group program_counter in step_coroutine_a linear_coroutine layout_size main_index clone_index (this_code_index+1) new_program_counter_index (index_group_index+int) survived_main_index_group new_newborn_main_index_group main_index_group new_index_group new_program_counter layout user_variable variable updater update event engine widget
    Linear_dynamic_clone {int_index,code_index,clone_number,dynamic_int,int}->let new_int=dynamic_int.dynamic_int (user_variable_getter user_variable layout layout_size clone_index) event engine widget-1 in if new_int<0 then step_coroutine_a linear_coroutine layout_size main_index clone_index code_index program_counter_index index_group_index survived_main_index_group newborn_main_index_group main_index_group index_group program_counter layout user_variable variable updater update event engine widget else if int<new_int then EE.quick_error "step_coroutine_a" 0 else do
        DVUM.write variable (int_index+clone_index) new_int
        let new_code_index=this_code_index+1 in let (new_program_counter,new_newborn_main_index_group,new_program_counter_index)=run_clone new_int clone_number (clone_index+clone_number) new_code_index program_counter_index newborn_main_index_group program_counter in step_coroutine_a linear_coroutine layout_size main_index clone_index new_code_index new_program_counter_index index_group_index survived_main_index_group new_newborn_main_index_group main_index_group index_group new_program_counter layout user_variable variable updater update event engine widget

user_variable_getter::DVU.Vector Int->DVS.Vector Layout->Int->Int->Int->Int
user_variable_getter user_variable layout layout_size clone_index int=case layout DVS.! catch_out 0 layout_size int of
    Layout {address,size}->user_variable DVU.! (address+mod clone_index size)

run_kill_group::Int->Int->Int->DVUM.MVector a Int->DIM.IntMap Program_counter->DIM.IntMap (DS.Seq Int)->CMST.ST a (DIM.IntMap Program_counter,DIM.IntMap (DS.Seq Int))
run_kill_group int int_index clone_index variable program_counter index_group=do
    new_int<-DVUM.read variable (int_index+clone_index)
    if new_int<=0 then return (program_counter,index_group) else do
        DVUM.write variable (int_index+clone_index) (-1)
        return (run_kill_group_a int 0 new_int index_group program_counter)

run_kill_group_a::Int->Int->Int->DIM.IntMap (DS.Seq Int)->DIM.IntMap Program_counter->(DIM.IntMap Program_counter,DIM.IntMap (DS.Seq Int))
run_kill_group_a int index index_group_index index_group program_counter=if int<=index then (program_counter,index_group) else let (new_index_group,maybe_single_index_group)=intmap_delete_maybe_lookup (index_group_index+index) index_group in case maybe_single_index_group of
    Nothing->run_kill_group_a int (index+1) index_group_index new_index_group program_counter
    Just single_index_group->run_kill_group_a int (index+1) index_group_index new_index_group (DF.foldl' (flip DIM.delete) program_counter single_index_group)

run_clone::Int->Int->Int->Int->Int->DS.Seq Int->DIM.IntMap Program_counter->(DIM.IntMap Program_counter,DS.Seq Int,Int)
run_clone int clone_number clone_index code_index program_counter_index main_index_group program_counter=if int<1 then (program_counter,main_index_group,program_counter_index) else run_clone (int-1) clone_number (clone_index+clone_number) code_index (program_counter_index+1) (main_index_group DS.|> program_counter_index) (intmap_insert program_counter_index (Program_counter {code_index=code_index,clone_index=clone_index}) program_counter)

run_create_group::DIM.IntMap Int->Int->Int->Int->Int->Int->DIM.IntMap (DS.Seq Int)->DIM.IntMap Program_counter->(DIM.IntMap Program_counter,DIM.IntMap (DS.Seq Int),Int)
run_create_group group_code_index int index clone_index program_counter_index index_group_index index_group program_counter=if int<=index then (program_counter,index_group,program_counter_index) else run_create_group group_code_index int (index+1) clone_index (program_counter_index+1) index_group_index (intmap_insert (index_group_index+index) (DS.singleton program_counter_index) index_group) (intmap_insert program_counter_index (Program_counter {code_index=intmap_lookup index group_code_index,clone_index=clone_index}) program_counter)

run_create_active_group::DIM.IntMap Int->Int->Int->Int->Int->Int->DS.Seq Int->DIM.IntMap (DS.Seq Int)->DIM.IntMap Program_counter->(DIM.IntMap Program_counter,DIM.IntMap (DS.Seq Int),Int,DS.Seq Int)
run_create_active_group group_code_index int index clone_index program_counter_index index_group_index newborn_main_index_group index_group program_counter=if int<=index then (program_counter,index_group,program_counter_index,newborn_main_index_group) else run_create_active_group group_code_index int (index+1) clone_index (program_counter_index+1) index_group_index (newborn_main_index_group DS.|> program_counter_index) (intmap_insert (index_group_index+index) (DS.singleton program_counter_index) index_group) (intmap_insert program_counter_index (Program_counter {code_index=intmap_lookup index group_code_index,clone_index=clone_index}) program_counter)

{-# INLINE run_coroutine #-}
{-# INLINE run_coroutine_a #-}
{-# INLINE run_coroutine_b #-}
{-# INLINE for_iterative #-}
{-# INLINE init_coroutine_state #-}
{-# INLINE to_coroutine #-}
{-# INLINE lift_coroutine #-}
{-# INLINE lift_coroutine_a #-}
{-# INLINE do_empty #-}
{-# INLINE do_empty_a #-}
{-# INLINE do_declare #-}
{-# INLINE do_declare_a #-}
{-# INLINE do_emit #-}
{-# INLINE do_wait #-}
{-# INLINE do_forever #-}
{-# INLINE do_then #-}
{-# INLINE do_while #-}
{-# INLINE do_pause #-}
{-# INLINE do_skip #-}
{-# INLINE do_assign #-}
{-# INLINE do_repeat #-}
{-# INLINE do_clone #-}
{-# INLINE do_if #-}
{-# INLINE do_dynamic_clone #-}
{-# INLINE do_case #-}
{-# INLINE do_fork #-}
{-# INLINE do_race #-}
{-# INLINE raw_coroutine_unary_generator #-}
{-# INLINE raw_coroutine_unary_operator #-}
{-# INLINE raw_coroutine_binary_operator #-}
{-# INLINE from_coroutine #-}
{-# INLINE clone_coroutine #-}
{-# INLINE clone_coroutine_a #-}
{-# INLINE user_variable_getter #-}
{-# INLINE run_kill_group #-}
{-# INLINE run_kill_group_a #-}
{-# INLINE run_clone #-}
{-# INLINE run_create_group #-}
{-# INLINE run_create_active_group #-}