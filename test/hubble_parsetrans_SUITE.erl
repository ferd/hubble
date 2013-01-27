-module(hubble_parsetrans_SUITE).
-include_lib("common_test/include/ct.hrl").
-compile(export_all).
-compile({parse_transform, hubble_trans}).
%% recommended to avoid errors
-no_auto_import([get/1, put/2]).

all() -> [form_tests, read_write, safe_update, multi_put, multi_up, demo].

-record(testrec, {item=new(),
                  other=[put(new(),a,b,c,3)],
                  vague}).

form_tests(_) ->
    %% various record forms
    _ = #testrec{},
    _ = #testrec{item=cat},
    _ = #testrec.item,
    _ = #testrec{item=_} = #testrec{},
    _ = #testrec{vague=new()},
    %% LC & BC
    _ = [new() ||
                  _ <- [get(put(new(), a, b, c, 3), a, b, c)], 0 < new()],
    _ = << <<(term_to_binary(new()))/binary>>
            || _ <- [get(put(new(), a, b, c, 3), a, b, c)], 0 < new() >>,
    _ = [new() ||
                  <<_:1>> <= term_to_binary(get(put(new(), a, b, c, 3))),
                  0 < new()],
    _ = << <<(term_to_binary(new()))/binary>>
            || <<_:1>> <= term_to_binary(get(put(new(), a, b, c, 3), a, b, c)),
               0 < new()>>,
    %% bin
    _ = <<(term_to_binary(new()))/binary>>,
    <<1:10>> = <<1:(get(put(new(), a, 10), a))>>,
    %% blocks
    begin
        3 = get(put(new(), a,b,c, 3), a,b,c)
    end,
    %% if, after
    try
        error(fail)
    catch
        _:_ ->
            if a > b -> never;
               true -> new()
            end
    after
        new()
    end,
    try ok of
        ok -> new()
    catch _:_ -> error(should_not_fail)
    end,
    %% Case
    case new() of
        1 -> error(huh);
        _ -> new()
    end,
    %% receive
    self() ! ok,
    receive
        ok -> new()
    end,
    receive
        never -> ok
    after get(put(new(), a, 10), a) ->
            yeaaaaah
    end,
    %% fun
    _ = (fun() -> new() end)().


%% - records (declarations and use, index, field)
%% - LCs
%% - BCs
%% - bin
%% - block
%% - if
%% - case
%% - receive
%% - fun


read_write(_) ->
    Char1 = put(put(put(put(new(),
                name, karl),
                current,weapon,points, 10),
                current,weapon,name, "crappy sword"),
                stats, level, 3),
    Skills = put(put(put(new(),
                 strength, 12),
                 charisma, 7),
                 luck, 3),
    Char = put(Char1, stats,skills, Skills),
    L = [{strength, get(Char, stats,skills,strength)},
         {item, {get(Char, current,weapon,name),
                 get(put(new(), a, get(Char, current,weapon,points)), a)}},
         {damage, get(Char, current,weapon,points) *
                  get(Char, stats,skills,strength) +
                  get(Skills, luck)}],
    [{strength,12},
     {item,{"crappy sword",10}},
     {damage,123}] = L.

%% can't update undef fields
safe_update(_) ->
    Char1 = put(put(put(put(new(),
                name, karl),
                current,weapon,points, 10),
                current,weapon,name, "crappy sword"),
                stats, level, 3),
    Skills = put(put(put(new(),
                 strength, 12),
                 charisma, 7),
                 luck, 3),
    Char = put(Char1, stats,skills, Skills),
    R = make_ref(),
    R = get(up(Char,stats,skills,strength,R),stats,skills,strength),
    crash = try
        up(Char, stats,skills,beatboxing, R),
        ok
        catch error:badpath ->
            crash
    end.

multi_put(_) ->
    Char1 = put(put(put(put(new(),
                name, karl),
                current,weapon,points, 10),
                current,weapon,name, "crappy sword"),
                stats, level, 3),
    Skills = put(put(put(new(),
                 strength, 12),
                 charisma, 7),
                 luck, 3),
    Char = put(Char1, stats,skills, Skills),
    Char = puts(new(),
                [name, karl],
                [current,weapon,points, 10],
                [current,weapon,name, "crappy sword"],
                [stats,level, 3],
                [stats,skills, Skills]).

multi_up(_) ->
    Char1 = put(put(put(put(new(),
                name, karl),
                current,weapon,points, 10),
                current,weapon,name, "crappy sword"),
                stats, level, 3),
    Skills = put(put(put(new(),
                 strength, 12),
                 charisma, 7),
                 luck, 3),
    Char = put(Char1, stats,skills, Skills),
    R = make_ref(),
    Char2 = ups(Char, [stats,skills,luck, R],
                      [name, R],
                      [current,weapon,name, R]),
    R = get(Char2, stats,skills,luck),
    R = get(Char2, name),
    R = get(Char2, current,weapon,name),
    3 = get(Char2, stats,level),
    crash = try
        ups(Char, [[stats,skills,luck, R],
                   [name, R],
                   [names, R], % bad key = crash!
                   [current,weapon,name, R]]),
        ok
        catch error:badpath ->
            crash
    end.

no_parse_fully_qualified_calls(_) ->
    Char1 = hubble:put(hubble:put(hubble:put(hubble:put(hubble:new(),
                [name], karl),
                [current,weapon,points], 10),
                [current,weapon,name], "crappy sword"),
                [stats, level], 3),
    Skills = hubble:put(hubble:put(hubble:put(hubble:new(),
                 [strength], 12),
                 [charisma], 7),
                 [luck], 3),
    Char = hubble:put(Char1, [stats,skills], Skills),
    Char = hubble:puts(hubble:new(),
                [{[name], karl},
                 {[current,weapon,points], 10},
                 {[current,weapon,name], "crappy sword"},
                 {[stats, level], 3},
                 {[stats, skills], Skills}]),
    R = make_ref(),
    Char2 = hubble:ups(Char, [{[stats,skills,luck], R},
                       {[name], R},
                       {[current,weapon,name], R}]),
    R = hubble:get(Char2, [stats,skills,luck]),
    R = hubble:get(Char2, [name]),
    R = hubble:get(Char2, [current,weapon,name]),
    3 = hubble:get(Char2, [stats,level]).

demo(_) ->
    Stats = puts(new(),
                 [strength, 12],
                 [wisdom, 9],
                 [luck, 10],
                 [dexterity, 5]),
    Char = puts(new(),
                [name, <<"karl">>],
                [bio, age, 219],
                [bio, hometown, <<"The Internet">>],
                [bio, parent, father, undefined],
                [bio, parent, mother, <<"A Unicorn">>],
                [stats, Stats]),
    Char2 = put(Char, level, 1),
    Char3 = up(Char2, bio,parent,father, <<"Randalf">>),
    9 = get(Char3, stats, wisdom),
    <<"A Unicorn">> = get(Char3, bio, parent, mother),
    <<"Randalf">> = get(get(Char3, bio, parent), father).

