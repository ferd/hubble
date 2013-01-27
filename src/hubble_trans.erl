-module(hubble_trans).
-export([parse_transform/2]).

parse_transform(Forms, _Options) ->
    run_forms(Forms).

%% only functions and records should contain reference to functions
%% used by hubble
run_forms([]) ->
    [];
run_forms([{function,Ln,Name,Args,Body} | Rest]) ->
    [{function,Ln,Name,Args,run_exp(Body)} | run_forms(Rest)];
run_forms([{attribute,Ln,record,{Name,Fields}} | Rest]) ->
    [{attribute,Ln,record,{Name,run_exp(Fields)}} | run_forms(Rest)];
run_forms([H|T]) ->
    [H|run_forms(T)].

run_exp([]) ->
    [];
run_exp([Exp|Rest]) ->
    [exp(Exp)|run_exp(Rest)].

%% we only change local hubble calls
exp(Exp={call,Ln0,{atom,Ln1,Name},Args}) ->
    case {Name, length(Args)} of
        {new,0} -> format_new(Exp);
        {put,N} when N >= 3 -> format_put(Exp);
        {puts,N} when N >= 2 -> format_puts(Exp);
        {up,N} when N >= 3 -> format_up(Exp);
        {ups,N} when N >= 2 -> format_ups(Exp);
        {get,N} when N >= 2 -> format_get(Exp);
        _ -> {call,Ln0,{atom,Ln1,Name},run_exp(Args)}
    end;
%% nice trick to search *every* form for what we actually handle
exp(Exp) when is_tuple(Exp) ->
    list_to_tuple([exp(X) || X <- tuple_to_list(Exp)]);
exp(Exp) when is_list(Exp) ->
    run_exp(Exp);
exp(Exp) ->
    Exp.

%% turn things into the correct external call
format_new({call,Ln,Fn,Args}) ->
    {call,Ln,
        {remote,Ln,{atom,Ln,hubble},Fn},
        Args}.

format_put({call,Ln,Fn,Args}) ->
    {call,Ln,
        {remote,Ln,{atom,Ln,hubble},Fn},
        set_args(Args)}.

format_puts({call,Ln,Fn,Args}) ->
    {call,Ln,
        {remote,Ln,{atom,Ln,hubble},Fn},
        sets_args(Args)}.
format_up({call,Ln,Fn,Args}) ->
    {call,Ln,
        {remote,Ln,{atom,Ln,hubble},Fn},
        set_args(Args)}.

format_ups({call,Ln,Fn,Args}) ->
    {call,Ln,
        {remote,Ln,{atom,Ln,hubble},Fn},
        sets_args(Args)}.

format_get({call,Ln,Fn,[HubbleExp|Args]}) ->
    {call,Ln,
        {remote,Ln,{atom,Ln,hubble},Fn},
        [exp(HubbleExp), to_cons(Args)]}.

%% function used to change a call of the form
%%  f(Hubble, part,of,a,path, Val)
%% into:
%%  f(Hubble, [part,of,a,path], Val)
set_args([TmpHubbleExp|Args]) ->
    [TmpVal|RevPath] = lists:reverse(Args),
    HubbleExp = exp(TmpHubbleExp),
    Val = exp(TmpVal),
    Path = lists:reverse(RevPath),
    [HubbleExp, to_cons(Path), Val].

%% function used to change a call of the form
%%  f(Hubble, [part,of,a,path, Val], [...])
%% into:
%%  f(Hubble, [{[part,of,a,path], Val}, ...])
sets_args([TmpHubbleExp|Args]) ->
    HubbleExp = exp(TmpHubbleExp),
    Paths = to_cons(sets_path(Args)),
    [HubbleExp, Paths].

%% change a real list of items to a cons'd list for the AST
to_cons([]) -> {nil,0};
to_cons([Exp|Exps]) ->
    Ln = try element(2,Exp) of
        N when is_integer(N) -> N;
        _ -> 0
    catch
        error:badarg ->
            0
    end,
    {cons, Ln, exp(Exp), to_cons(Exps)}.

sets_path([]) -> [];
sets_path([H|T]) ->
    {{Ln,Var}, Path} = split_cons_tail(H),
    [{tuple, Ln, [Path, Var]}|sets_path(T)].

%% we never expect a path without a value -- that wouldn't work.
%% It's better to crash for now.
split_cons_tail({cons, Ln1, Exp1, {cons,Ln2,Exp2,{nil,Ln3}}}) ->
    {{Ln2,exp(Exp2)}, {cons, Ln1, exp(Exp1), {nil,Ln3}}};
split_cons_tail({cons, Ln, Exp1, Exp2}) ->
    {Tail, List} = split_cons_tail(Exp2),
    {Tail, {cons, Ln, exp(Exp1), List}}.
