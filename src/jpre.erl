-module(jpre).

-export([
        accent_phrases/1
      , accent_phrases/2
      , detail/1
      , detail/2
      , normalize/1
      , get_default_opt/0
    ]).

-export([
        set_num/1
      , get_num/1
      , add_num/2
    ]).

-export_type([
        opt/0
      , mora/0
      , accent_phrase/0
    ]).

-on_load(init/0).

-type opt() :: #{
        dict := unicode:unicode_binary()
      , user_dict := [[unicode:unicode_binary()]]
      , find_missing_words := boolean()
      , schedule => dirty_cpu | normal | dirty_io
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

-type detail() :: #{
        accent_phrases := [accent_phrase()]
      , missing_words := [unicode:unicode_binary()]
    }.

init() ->
    PrivDir = code:priv_dir(?MODULE),
    LibName = "libjpre",
    erlang:load_nif(filename:append(PrivDir, LibName), 0).

-spec get_default_opt() -> opt().
get_default_opt() ->
    PrivDir = code:priv_dir(?MODULE),
    DictPath = filename:append(PrivDir, "naist-jdic"),
    #{
        dict => unicode:characters_to_binary(DictPath)
      , user_dict => []
      , schedule => dirty_cpu
      , find_missing_words => false
    }.

-spec detail(unicode:unicode_binary()) -> detail().
detail(Text) ->
    detail(Text, get_default_opt()).

-spec detail(unicode:unicode_binary(), opt()) -> detail().
detail(Text, Opt=#{schedule:=normal}) ->
    normal_detail(Text, Opt);
detail(Text, Opt=#{schedule:=dirty_io}) ->
    dirty_io_detail(Text, Opt);
detail(Text, Opt) ->
    dirty_cpu_detail(Text, Opt).

-spec normal_detail(unicode:unicode_binary(), opt()) -> detail().
normal_detail(Arg1, Arg2) ->
    erlang:nif_error(nif_module_unavailable, [Arg1, Arg2]).

-spec dirty_cpu_detail(unicode:unicode_binary(), opt()) -> detail().
dirty_cpu_detail(Arg1, Arg2) ->
    erlang:nif_error(nif_module_unavailable, [Arg1, Arg2]).

-spec dirty_io_detail(unicode:unicode_binary(), opt()) -> detail().
dirty_io_detail(Arg1, Arg2) ->
    erlang:nif_error(nif_module_unavailable, [Arg1, Arg2]).

-spec accent_phrases(unicode:unicode_binary()) -> [accent_phrase()].
accent_phrases(Text) ->
    maps:get(accent_phrases, detail(Text)).

-spec accent_phrases(unicode:unicode_binary(), opt()) -> [accent_phrase()].
accent_phrases(Text, Opt) ->
    maps:get(accent_phrases, detail(Text, Opt)).


-spec normalize(unicode:unicode_binary()) -> unicode:unicode_binary().
normalize(Arg1) ->
    erlang:nif_error(nif_module_unavailable, [Arg1]).

set_num(Arg1) ->
    erlang:nif_error(nif_module_unavailable, [Arg1]).

get_num(Arg1) ->
    erlang:nif_error(nif_module_unavailable, [Arg1]).

add_num(Arg1, Arg2) ->
    erlang:nif_error(nif_module_unavailable, [Arg1, Arg2]).

