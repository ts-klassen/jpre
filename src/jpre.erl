-module(jpre).

-export([
        accent_phrases/1
      , accent_phrases/2
    ]).

-export_type([
        opt/0
      , mora/0
      , accent_phrase/0
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

-type accent_phrase() :: #{
        moras := [mora()]
      , accent := non_neg_integer()
      , pause_mora := null | mora()
      , is_interrogative := boolean()
    }.

init() ->
    PrivDir = code:priv_dir(?MODULE),
    LibName = "libjpre",
    erlang:load_nif(filename:append(PrivDir, LibName), 0).

-spec accent_phrases(unicode:unicode_binary()) -> [accent_phrase()].
accent_phrases(Text) ->
    PrivDir = code:priv_dir(?MODULE),
    DictPath = filename:append(PrivDir, "naist-jdic"),
    Opt = #{
        dict => unicode:characters_to_binary(DictPath)
    },
    accent_phrases(Text, Opt).

-spec accent_phrases(unicode:unicode_binary(), opt()) -> [accent_phrase()].
accent_phrases(Arg1, Arg2) ->
    erlang:nif_error(nif_module_unavailable, [Arg1, Arg2]).

