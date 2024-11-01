-module(jpre_user_dict).

-export([
        simple/1
      , cast_type/1
    ]).

cast_type([Bin|_]=Input) when is_binary(Bin) ->
    cast_type([Input]);
cast_type(Input) when is_list(Input) ->
    lists:filtermap(fun
        (List) when is_list(List), length(List) > 13 ->
            {true, lists:map(fun
                (B) when is_binary(B) ->
                    B;
                (I) when is_integer(I) ->
                    integer_to_binary(I);
                (_) ->
                    <<>>
            end, List)};
        (_) ->
            false
    end, Input);
cast_type(_) ->
    [].

simple(List) when is_list(List) ->
    lists:map(fun simple/1, List);
simple(CSV) when is_binary(CSV) ->
    binary:split(CSV, <<",">>, [global]);
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

