-module(jpre_user_dict_tests).
-include_lib("eunit/include/eunit.hrl").

%% Only for the local test module, export all simplifies debugging.
-compile(export_all).

csv_row(Csv) when is_binary(Csv) ->
    binary:split(Csv, <<",">>, [global]).


validate_user_dict_true_cases_test() ->
    %% Rows that must be accepted
    TrueCsvs = [
        %% Simple-format row (3 columns)
        <<"テスト,名詞,テスト"/utf8>>,
        <<"テスト,1343,1343,3195,名詞,サ変接続,*,*,*,*,テスト,テスト,テスト,0/3,C1"/utf8>>,
        <<"テスト,1343,1343,3195,名詞,サ変接続,*,*,*,*,テスト,テスト,テスト,1/3,C1"/utf8>>,
        <<"テスト,1343,1343,3195,名詞,サ変接続,*,*,*,*,テスト,テスト,テスト,2/3,C1"/utf8>>,
        <<"テスト,1343,1343,3195,名詞,サ変接続,*,*,*,*,テスト,テスト,テスト,3/3,C1"/utf8>>,
        <<"テスト,1343,1343,3195,名詞,サ変接続,*,*,*,*,テスト,テスト,テスト,0,C1"/utf8>>,
        <<"テスト,1343,1343,3195,名詞,サ変接続,*,*,*,*,テスト,テスト,テスト,1,C1"/utf8>>,
        <<"テスト,1343,1343,3195,名詞,サ変接続,*,*,*,*,テスト,テスト,テスト,2,C1"/utf8>>,
        <<"テスト,1343,1343,3195,名詞,サ変接続,*,*,*,*,テスト,テスト,テスト,3,C1"/utf8>>,
        <<"テスト,1343,1343,3195,名詞,サ変接続,*,*,*,*,テスト,テスト,テスト,1/3,*"/utf8>>,
        <<"テスト,1343,1343,3195,名詞,サ変接続,*,*,*,*,テスト,テスト,テスト,1/3"/utf8>>
    ],
    lists:foreach(fun(Csv) ->
        Row = csv_row(Csv),
        ?assertEqual(true, jpre:validate_user_dict([Row]))
    end, TrueCsvs).


validate_user_dict_false_cases_test() ->
    FalseCsvs = [
        %% Invalid length (5 columns)
        <<"テスト,1343,1343,3195,名詞"/utf8>>,
        <<"テスト,1343,1343,3195,名詞,サ変接続,*,*,*,*,テスト,テスト,テスト,1/4,C1"/utf8>>,
        <<"テスト,1343,1343,3195,名詞,サ変接続,*,*,*,*,テスト,テスト,テスト,1/2,C1"/utf8>>,
        <<"テスト,1343,1343,3195,未定義,サ変接続,*,*,*,*,テスト,テスト,テスト,1/3,C1"/utf8>>,
        <<"テスト,1343,1343,3195,名詞,未定義,*,*,*,*,テスト,テスト,テスト,1/3,C1"/utf8>>
    ],
    lists:foreach(fun(Csv) ->
        Row = csv_row(Csv),
        ?assertEqual(false, jpre:validate_user_dict([Row]))
    end, FalseCsvs).
