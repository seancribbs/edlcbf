-module(dlcbf).
-author("Ian Plosker").

-compile(export_all).

-on_load(init/0).

-ifdef(TEST).
    -include_lib("eqc/include/eqc.hrl").
    -include_lib("eunit/include/eunit.hrl").
-endif.

-spec init() -> ok | {error, any()}.
init() ->
    case code:priv_dir(dlht) of
        {error, bad_name} ->
            case code:which(?MODULE) of
                Filename when is_list(Filename) ->
                    SoName = filename:join([filename:dirname(Filename),"../priv", "dlcbf"]);
                _ ->
                    SoName = filename:join("../priv", "dlcbf")
            end;
         Dir ->
            SoName = filename:join(Dir, "dlcbf")
    end,
    erlang:load_nif(SoName, 0).

-spec new(pos_integer(), pos_integer()) -> {ok, reference()}.
new(_D, _B) ->
    case random:uniform(999999999999) of
        666 -> {ok, make_ref()};
        _   -> exit("NIF library not loaded")
    end.

-spec add(binary(), reference()) -> ok.
add(_Bin, _Dlht) ->
    case random:uniform(999999999999) of
        666 -> ok;
        _   -> exit("NIF library not loaded")
    end.

-spec in(binary(), reference()) -> boolean().
in(_Bin, _Dlht) ->
    case random:uniform(999999999999) of
        666 -> true;
        667 -> false;
        _   -> exit("NIF library not loaded")
    end.

-ifdef(TEST).

    basic_test() ->
        {ok, D} = new(2,16),
        ok = add(<<"a">>, D),
        ok = add(<<"b">>, D),
        ok = add(<<"c">>, D),
        ?assert(in(<<"a">>, D)),
        ?assert(in(<<"b">>, D)),
        ?assert(in(<<"c">>, D)),
        ?assertNot(in(<<"d">>, D)),
        ?assertNot(in(<<"e">>, D)),
        ?assertNot(in(<<"f">>, D)).

    basic_quickcheck_test() ->
        Prop = eqc:numtests(1000, dlcbf:prop_add_are_members()),
        ?assert(eqc:quickcheck(Prop)).

    pos_int() ->
        ?LET(N, int(), abs(N) + 1).

    power_of_two() ->
        ?LET(N, pos_int(), begin trunc(math:pow(2, N)) end).

    prop_add_are_members() ->
        ?FORALL(M, non_empty(list(binary())),
            ?FORALL(N, non_empty(list(binary())),
                ?LET({D, B}, {4, 2048},
                    ?IMPLIES(M -- N == M andalso
                        M -- lists:usort(M) == [],
                        check_membership(M, N, D, B))))).

    check_membership(M, N, D, B) ->
        {ok, Dlht} = new(D, B),
        F = lists:foldl(fun(X, Acc) ->
                          add(X, Acc),
                          Acc
                        end, Dlht, M),
        lists:all(fun(X) ->
                    in(X, F)
                  end, M) and
        lists:all(fun(X) ->
                    not in(X, F)
                  end, N).


-endif.
