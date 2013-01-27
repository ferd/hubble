# Hubble #

Hubble is a prototype library allowing to create, read, and update deep
Erlang data structures in a manner that would be somewhat similar to using dot
notation in languages with their roots closer to OO.

### What's a Hubble? ###

It's a terrible pun on wanting a library that lets you zoom is in your
data structure's deep space.

`Hubble` is the name given to whatever the library produces as a data structure.

At this time, it's equivalent to a recursive tuple-list of the form:

    [{Key1, [{Key2, [{Key3, Val1}, {Key4, Val2}]},
             {KeyA, Val3}]},
     {KeyB, Val4}]

Where elements are accessible by specifying how to reach them (see *Using
the library*).

## Building ##

`$ erl -make`

The library should also be inherently compatible with rebar.

## Running Tests ##

`$ erl -make && ct_run -pa ebin -suite test/* -logdir logs/`

The suites obviously use Common Test.

## Using the library ##
### Sane Mode ###

Sane mode is a more verbose mode that uses no magic. It is the recommended mode
for the library.

The 4 basic operations are:

- `hubble:new() -> Hubble`: initiates an empty data structure to be filled
  later. The data structure is called a `Hubble`.
- `hubble:put(Hubble, [Path,To,Item], Value)`: this uses a given `Hubble`
  data structure, and creates every intermediary path (if they do not exist) up
  until the last one, where `Value` is attributed to it.
- `hubble:get(Hubble, [Path,To,Item])`": this will fetch a value currently
  stored within a given path.
- `hubble:up(Hubble, [Path,To,Item], NewVal)`: Will update the value in a given
  Path to a new one, if and only if the path exists. Otherwise, the operation
  errors out.

Two shortcut operations for mass updating are provided. They're not more
efficient, just nicer to look at:

- `hubble:puts(Hubble, [{[path,1], my_value},
                        {[somewhat, "deeper",{path}], other_val}]).`
- `hubble:ups(Hubble, [{[path,1], my_value},
                       {[somewhat, "deeper",{path}], other_val}]).`

The first one does multiple `put` operations, while the latter does multiple
`up` operations.

### Sane example ###

Let's create a fictional RPG character with given statistics:

    1> Stats = hubble:puts(hubble:new(),
    1>                     [{[strength], 12},
    1>                      {[wisdom], 9},
    1>                      {[luck], 10},
    1>                      {[dexterity], 5}]).
    ...

And let's make a baseline character bio that integrates the stats:

    2> Char = hubble:puts(hubble:new(),
    2>                    [{[name], <<"karl">>},
    2>                     {[bio, age], 219},
    2>                     {[bio, hometown], <<"The Internet">>},
    2>                     {[bio, parent, father], undefined},
    2>                     {[bio, parent, mother], <<"A Unicorn">>},
    2>                     {[stats], Stats}]).
    ...

Oh yeah and let's not forget its level!

    3> Char2 = hubble:up(Char, [level], 1). 
        ** exception error: badpath
         in function  hubble:up/3 (src/hubble.erl, line 41)
    4> Char2 = hubble:put(Char, [level], 1).
        ...

We can only update fields that exist! Let's say for instance our friend karl finds
out his father was a man named Randalf. Let's update his profile:

    5> Char3 = hubble:up(Char2, [bio,parent,father], <<"Randalf">>).
    ...

That worked. We can fetch back any information by using a path, or partial one too:

    6> hubble:get(Char3, [stats,wisdom]).
    9
    7> hubble:get(Char3, [bio,parent,mother]).
    <<"A Unicorn">>
    8> hubble:get(hubble:get(Char3, [bio,parent]), [father]).
    <<"Randalf">>

And that's about it.

### insane mode ###

This is a mode that uses a parse transform to introduce syntactic sugar
for people who really can't tolerate using all these damn tokens to do
their thing.

The insane mode is enabled by using a parse transform:

    -compile({parse_transform, hubble_trans}).

This will automatically allow you to use hubble functions locally with the following
syntax:

    Hubble = new()

    put(Hubble, part,of,a,path, Val)
    up(Hubble, part,of,a,path, NewVal)

    get(Hubble, part, of, a, path)

    puts(Hubble, [part,of,a,path, Val],
                 [another,path, OtherVal]).
    ups(Hubble, [part,of,a,path, Val],
                [another,path, OtherVal]).

The functions get a variable number of arguments, which gets translated back into
the long 'sane' form. This means the example above could now be written as:

    demo() ->
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

Note that functions prefixed with `hubble:` (a fully qualified call) will *not* be
parse-transformed. This means you could make a mixed usage of both forms of
functions if you felt like it, although I have no idea why you would do that (but
hey, it's the insane mode after all!).

Oh yeah, one more precaution. The `get/1` function and `put/2` function can *never* happen naturally with the `hubble` app. These are not parse-transformed back to
anything -- they are the auto-imported functions `erlang:get/1` and `erlang:put/2`,
used to access the process dictionary. To avoid confusion and highlight errors,
consider adding:

    -no_auto_import([get/1, put/2]).

To the modules that use the parse transforms, although I firmly believe you should
suffer for your bad decisions of using them.

## Future Development ##

I would like it if it could be possible to support specifying the types of the
underlying Hubble data structure for parse transforms and regular functions.
Basically, I think it would be neat if the user could specify something like:

    -hubble(dict).

In conjunction with the parse transform to allow specifying how data is stored and
retrieved (not sure if I'd like it to have impact on the fully qualified calls too).

Alternatively, using this form, it could be interesting to allow custom
implementation of functions as a very weird callback module:

    -hubble(puts, fun ?MODULE:my_puts/3).
    -hubble(ups, fun ?MODULE:my_ups/3).

So someone could overload one or all of the functions to either support custom
data structures, or temporarily allow to patch functions the user thinks are
broken.

This would need explicit support from the library.

## Why the hell did you write that ##

I had a free day while sitting on a bus and on a plane. People were arguing on
the Erlang mailing list about a library a bit like that so I decided to write
it just to see, and flex my parse transforming muscles.

## Should I use this in production? ##

Well given the underlying representation is a list, and that `lists:key*` functions
are used to navigate around a Hubble, you have to decide if an O(n) behaviour is
suitable for you.

Under the current implementation, the first elements added to the hubble are the
first ones to be found when looking data up. This means if you have some paths you
are likely to take more often than others, it can be possible to set them in stone
when first creating the structure to get generally very fast reads and updates.

Otherwise, you may judge that simple navigation is worth the (potential) performance
hit, although a benchmark of your own should show how things go.

I would not recommend using the parse transform version of the module in production
because I bet people who will look at your code will not like that you're using
auto-imported functions with variable arities, something that could easy make you
be seen as a heretic. I will personally offer no support to code using that form
unless I find the free time to do it. I'll also offer no commitment to backwards
compatibility of the parse transformed code on that one. Maybe I'll offer sed
command to help port it. Eh.
