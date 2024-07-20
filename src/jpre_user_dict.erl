-module(jpre_user_dict).

-export([
        simple/1
    ]).

simple(List) when is_list(List) ->
    lists:map(fun simple/1, List);
simple({Text, Cost, Kana, Accent}) when is_integer(Cost) ->
    simple({Text, integer_to_binary(Cost), Kana, Accent});
simple({Text, Cost, Kana, Accent}) when is_integer(Accent) ->
    simple({Text, Cost, Kana, integer_to_binary(Accent)});
simple({Text, Cost, Kana, Accent}) ->
    [
        Text,
        <<"0"/utf8>>,
        <<"0"/utf8>>,
        Cost,
        <<"*"/utf8>>,
        <<"*"/utf8>>,
        <<"*"/utf8>>,
        <<"*"/utf8>>,
        <<"*"/utf8>>,
        <<"*"/utf8>>,
        <<"*"/utf8>>,
        <<"*"/utf8>>,
        Kana,
        Accent,
        <<"*"/utf8>>
    ].

