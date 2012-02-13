%%% @author Sean Cribbs <sean@72-58-236-246>
%%% @copyright (C) 2012, Sean Cribbs
%%% @doc
%%%
%%% @end
%%% Created : 13 Feb 2012 by Sean Cribbs <sean@72-58-236-246>

-module(dlcbf_statem).

-include_lib("eqc/include/eqc.hrl").
-include_lib("eqc/include/eqc_statem.hrl").

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

%% Initialize the state
initial_state() ->
    #state{}.

%% Command generator, S is the state
command(S) ->
    oneof([{call, ?MODULE, new, [4, 2048]} || S#state.bloom == undefined ] ++
              [{call, dlcbf, add, [non_empty(binary()), S#state.bloom]} || S#state.bloom =/= undefined  ] ++
              [{call, dlcbf, in, [non_empty(binary()), S#state.bloom]} || S#state.bloom =/= undefined, S#state.keys == [] ] ++
              [{call, dlcbf, in, [maybe_key(S#state.keys), S#state.bloom]} || S#state.bloom =/= undefined, S#state.keys =/= [] ]).

maybe_key(KeyList) ->
    oneof([
           ?SUCHTHAT(X, non_empty(binary()), not lists:member(X, KeyList)),
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
postcondition(_S,{call,_,add,_},Res) ->
    Res == ok;
postcondition(S,{call,dlcbf,in,[Key, _B]},Res) ->
    Res == lists:member(Key, S#state.keys);
postcondition(_S,_Call,_Res) ->
    true.

prop_set_membership() ->
    ?FORALL(Cmds,commands(?MODULE),
            begin
                {H,S,Res} = run_commands(?MODULE,Cmds),
                ?WHENFAIL(
                   io:format("History: ~p\nState: ~p\nRes: ~p\n",[H,S,Res]),
                   Res == ok)
            end).

new(A,B) ->
    case dlcbf:new(A,B) of
        {ok, Bloom} -> Bloom;
        X -> X
    end.
