-module(jpre_batch_tests).
-include_lib("eunit/include/eunit.hrl").

batch_accent_phrases_test() ->
    Texts = [<<"テスト"/utf8>>, <<"サンプル"/utf8>>],
    Result = jpre:batch_accent_phrases(Texts),
    Expected = [[#{accent => 1,is_interrogative => false,
        moras =>
            [#{consonant => <<"t">>,consonant_length => 0.0,pitch => 0.0,
               text => <<227,131,134>>,
               vowel => <<"e">>,vowel_length => 0.0},
             #{consonant => <<"s">>,consonant_length => 0.0,pitch => 0.0,
               text => <<227,130,185>>,
               vowel => <<"U">>,vowel_length => 0.0},
             #{consonant => <<"t">>,consonant_length => 0.0,pitch => 0.0,
               text => <<227,131,136>>,
               vowel => <<"o">>,vowel_length => 0.0}],
        pause_mora => null}],
     [#{accent => 1,is_interrogative => false,
        moras =>
            [#{consonant => <<"s">>,consonant_length => 0.0,pitch => 0.0,
               text => <<227,130,181>>,
               vowel => <<"a">>,vowel_length => 0.0},
             #{consonant => null,consonant_length => null,pitch => 0.0,
               text => <<227,131,179>>,
               vowel => <<"N">>,vowel_length => 0.0},
             #{consonant => <<"p">>,consonant_length => 0.0,pitch => 0.0,
               text => <<227,131,151>>,
               vowel => <<"u">>,vowel_length => 0.0},
             #{consonant => <<"r">>,consonant_length => 0.0,pitch => 0.0,
               text => <<227,131,171>>,
               vowel => <<"u">>,vowel_length => 0.0}],
        pause_mora => null}]],
    ?assertEqual(Expected, Result).
