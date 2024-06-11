-module(jpre).

-export([
        moras/2
    ]).

-on_load(init/0).

-type opt() :: #{
        dict := unicode:unicode_binary()
    }.

-type mora() :: #{
        text := unicode:unicode_binary()
      , consonant := null | unicode:unicode_binary()
      , consonant_length := null | number()
      , vowel := unicode:unicode_binary()
      , vowel_length := number()
      , pitch := number()
    }.

-type moras() :: [mora(), ...].

init() ->
    PrivDir = code:priv_dir(?MODULE),
    LibName = "libjpre",
    erlang:load_nif(filename:append(PrivDir, LibName), 0).

-spec moras(unicode:unicode_binary(), opt()) -> [moras()].
moras(Arg1, Arg2) ->
    erlang:nif_error(nif_module_unavailable, [Arg1, Arg2]).

