-module(hubble_SUITE).
-include_lib("common_test/include/ct.hrl").

-import(hubble, [new/0,
                 put/3, puts/2,
                 up/3, ups/2,
                 get/2]).

-compile(export_all).

all() -> [read_write, safe_update, multi_put, multi_up].

read_write(_) ->
    Char1 = put(put(put(put(new(),
                [name], karl),
                [current,weapon,points], 10),
                [current,weapon,name], "crappy sword"),
                [stats, level], 3),
    Skills = put(put(put(new(),
                 [strength], 12),
                 [charisma], 7),
                 [luck], 3),
    Char = put(Char1, [stats,skills], Skills),
    L = [{strength, get(Char, [stats,skills,strength])},
         {item, {get(Char, [current,weapon,name]),
                 get(Char, [current,weapon,points])}},
         {damage, get(Char, [current,weapon,points]) *
                  get(Char, [stats,skills,strength]) +
                  get(Skills, [luck])}],
    [{strength,12},
     {item,{"crappy sword",10}},
     {damage,123}] = L.

%% can't update undef fields
safe_update(_) ->
    Char1 = put(put(put(put(new(),
                [name], karl),
                [current,weapon,points], 10),
                [current,weapon,name], "crappy sword"),
                [stats, level], 3),
    Skills = put(put(put(new(),
                 [strength], 12),
                 [charisma], 7),
                 [luck], 3),
    Char = put(Char1, [stats,skills], Skills),
    R = make_ref(),
    R = get(up(Char,[stats,skills,strength],R),[stats,skills,strength]),
    crash = try
        up(Char, [stats,skills,beatboxing], R),
        ok
        catch error:badpath ->
            crash
    end.

multi_put(_) ->
    Char1 = put(put(put(put(new(),
                [name], karl),
                [current,weapon,points], 10),
                [current,weapon,name], "crappy sword"),
                [stats, level], 3),
    Skills = put(put(put(new(),
                 [strength], 12),
                 [charisma], 7),
                 [luck], 3),
    Char = put(Char1, [stats,skills], Skills),
    Char = puts(new(),
                [{[name], karl},
                 {[current,weapon,points], 10},
                 {[current,weapon,name], "crappy sword"},
                 {[stats, level], 3},
                 {[stats, skills], Skills}]).

multi_up(_) ->
    Char1 = put(put(put(put(new(),
                [name], karl),
                [current,weapon,points], 10),
                [current,weapon,name], "crappy sword"),
                [stats, level], 3),
    Skills = put(put(put(new(),
                 [strength], 12),
                 [charisma], 7),
                 [luck], 3),
    Char = put(Char1, [stats,skills], Skills),
    R = make_ref(),
    Char2 = ups(Char, [{[stats,skills,luck], R},
                       {[name], R},
                       {[current,weapon,name], R}]),
    R = get(Char2, [stats,skills,luck]),
    R = get(Char2, [name]),
    R = get(Char2, [current,weapon,name]),
    3 = get(Char2, [stats,level]),
    crash = try
        ups(Char, [{[stats,skills,luck], R},
                   {[name], R},
                   {[names], R}, % bad key = crash!
                   {[current,weapon,name], R}]),
        ok
        catch error:badpath ->
            crash
    end.
