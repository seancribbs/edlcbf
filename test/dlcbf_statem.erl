%%% @author Sean Cribbs <sean@72-58-236-246>
%%% @copyright (C) 2012, Sean Cribbs
%%% @doc
%%%
%%% @end
%%% Created : 13 Feb 2012 by Sean Cribbs <sean@72-58-236-246>

-module(dlcbf_statem).

-ifdef(EQC).
-include_lib("eqc/include/eqc.hrl").
-include_lib("eqc/include/eqc_statem.hrl").
-include_lib("eunit/include/eunit.hrl").

-compile(export_all).
-behaviour(eqc_statem).
-export([initial_state/0,
         command/1,
         next_state/3,
         postcondition/3,
         precondition/2
        ]).

-record(state, {
          bloom = undefined,
          keys = []
         }).

-define(TEST_ITERATIONS, 150).
-define(QC_OUT(P),
        eqc:on_output(fun(Str, Args) -> io:format(user, Str, Args) end, P)).

%% Eunit harness
eqc_test() ->
    ?assert(quickcheck(numtests(?TEST_ITERATIONS, ?QC_OUT(prop_set_membership())))).

%% Initialize the state
initial_state() ->
    #state{}.

%% Command generator, S is the state
command(S) ->
    oneof([{call, ?MODULE, new, [sub_tables(), buckets()]} || S#state.bloom == undefined ] ++
              [{call, dlcbf, add, [key(), S#state.bloom]} || S#state.bloom =/= undefined  ] ++
              [{call, dlcbf, in, [key(), S#state.bloom]} || S#state.bloom =/= undefined, S#state.keys == [] ] ++
              [{call, dlcbf, in, [maybe_key(S#state.keys), S#state.bloom]} || S#state.bloom =/= undefined, S#state.keys =/= [] ]).

key() ->
    non_empty(binary()).

sub_tables() ->
    ?SUCHTHAT(X, nat(), X >= 4).

buckets() ->
    ?LET(P, ?SUCHTHAT(X, nat(), X >= 2 andalso X =< 12), trunc(math:pow(2, P))).

maybe_key(KeyList) ->
    oneof([
           ?SUCHTHAT(X, key(), not lists:member(X, KeyList)),
           elements(KeyList)
          ]).

%% Next state transformation, S is the current state
next_state(S,V,{call,_,new,_}) ->
    S#state{bloom=V, keys=[]};
next_state(S,_V,{call,_,add,[Value, _]}) ->
    S#state{keys=[Value|S#state.keys]};
next_state(S,_V,_) ->
    S.

%% Precondition, checked before command is added to the command sequence
precondition(_S,{call,_,_,_}) ->
    true.

%% Postcondition, checked after command has been evaluated
%% OBS: S is the state before next_state(S,_,<command>)
%% postcondition(_S,{call,?MODULE,Mod,_},Res) when Mod == new orelse Mod == add ->
%%     is_binary(Res);

postcondition(_S,{call,_,add,_},ok) ->
    true;
postcondition(_S,{call,_,add,_},{ok, _}) ->
    true;
postcondition(_S,{call,_,add,_},Res) ->
    {add_returned_badvalue, Res};
postcondition(S,{call,dlcbf,in,[Key, _B]},Res) ->
    case Res == lists:member(Key, S#state.keys) of
        true -> true;
        _ -> {state_mismatched, {Key, Res}, S#state.keys}
    end;
postcondition(_S,_Call,_Res) ->
    true.

prop_set_membership() ->
    ?FORALL(Cmds,commands(?MODULE),
            begin
                {H,S,Res} = run_commands(?MODULE,Cmds),
                ?WHENFAIL(
                   io:format(user, "State: ~p\nRes: ~p\n",[S,Res]),
                   aggregate(command_names(Cmds), Res == ok))
            end).

new(A,B) ->
    case dlcbf:new(A,B) of
        {ok, Bloom} -> Bloom;
        X -> X
    end.

add(Key,B) ->
    case dlcbf:add(Key, B) of
        {ok, Bloom} -> Bloom;
        X -> X
    end.
-endif.
