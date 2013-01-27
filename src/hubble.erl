-module(hubble).
-export([new/0,
         put/3, puts/2,
         up/3, ups/2,
         get/2]).

-type key() :: term().
-type val() :: term().
-type path() :: [key(), ...].
-type keyval() :: {key(), val() | hubble()}.
-opaque hubble() :: {'$hubble',[keyval()]}.

-export_type([key/0, val/0, path/0, hubble/0]).

-spec new() -> hubble().
new() -> {'$hubble',[]}.

-spec put(hubble(), path(), val()) -> hubble().
put({'$hubble', []}, [K], V) ->
    {'$hubble', [{K,V}]};
put({'$hubble', L}, [K], V) ->
    {'$hubble', lists:keystore(K, 1, L, {K,V})};
put({'$hubble', L}, [K|T], V) ->
    case lists:keyfind(K, 1, L) of
        false ->
            {'$hubble', lists:keystore(K, 1, L, {K, put(new(), T, V)})};
        {K,Old} ->
            {'$hubble', lists:keyreplace(K, 1, L, {K, put(Old, T, V)})}
    end.

-spec puts(hubble(), [{path(), val()}]) -> hubble().
puts(Hubble, []) -> Hubble;
puts(Hubble, [{Path,Val} | Rest]) ->
    puts(put(Hubble, Path, Val), Rest).

up({'$hubble', []}, _, _) ->
    error(badpath);
up({'$hubble', L}, [K], V) ->
    case lists:keyfind(K, 1, L) of
        false ->
            error(badpath);
        {K,_} ->
            {'$hubble', lists:keyreplace(K, 1, L, {K, V})}
    end;
up({'$hubble', L}, [K|T], V) ->
    case lists:keyfind(K, 1, L) of
        false ->
            error(badpath);
        {K,Old} ->
            {'$hubble', lists:keyreplace(K, 1, L, {K, up(Old, T, V)})}
    end.

-spec ups(hubble(), [{path(), val()}]) -> hubble().
ups(Hubble, []) -> Hubble;
ups(Hubble, [{Path,Val} | Rest]) ->
    ups(up(Hubble, Path, Val), Rest).

get({'$hubble', L}, [K]) ->
    case lists:keyfind(K, 1, L) of
        false -> undefined;
        {K,V} -> V
    end;
get({'$hubble', L}, [H|T]) ->
    {H, L2} = lists:keyfind(H, 1, L),
    get(L2,T).

