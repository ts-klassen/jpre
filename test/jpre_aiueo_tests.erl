-module(jpre_aiueo_tests).
-include_lib("eunit/include/eunit.hrl").

-compile([export_all]).


accent_phrases_test() ->
    Text = const_text(),
    Expected = normalize_accent_phrases(const_accent_phrases()),
    Result = normalize_accent_phrases(jpre:accent_phrases(Text)),
    ?assertEqual(length(Expected), length(Result)),
    lists:foreach(fun({Exp, Res}) ->
        ?assertEqual(Exp, Res)
    end, lists:zip(Expected, Result)).


normalize_accent_phrases(AccentPhrases) ->
    AccentPhrases1 = normalize_json(AccentPhrases),
    lists:map(fun(AccentPhrase)->
        normalize_accent_phrase(AccentPhrase)
    end, AccentPhrases1).
normalize_json(null) ->
    null;
normalize_json(true) ->
    true;
normalize_json(false) ->
    false;
normalize_json(Atom) when is_atom(Atom) ->
    atom_to_binary(Atom);
normalize_json(Map) when is_map(Map) ->
    maps:from_list(lists:map(fun({Key, Val})->
        {normalize_json(Key), normalize_json(Val)}
    end, maps:to_list(Map)));
normalize_json(List) when is_list(List) ->
    lists:map(fun(E)->
        normalize_json(E)
    end, List);
normalize_json(Other) ->
    Other.

normalize_accent_phrase(AP=#{<<"moras">>:=M,<<"pause_mora">>:=PM}) ->
    AP#{
        <<"moras">> => normalize_moras(M)
      , <<"pause_mora">> => normalize_mora(PM)
    }.
normalize_moras(Moras) ->
    lists:map(fun(Mora)->
        normalize_mora(Mora)
    end, Moras).
normalize_mora(null) ->
    null;
normalize_mora(Mora) ->
    KeyList = [
        <<"consonant_length">>
      , <<"vowel_length">>
      , <<"pitch">>
    ],
    lists:foldl(fun(Key, Mora0)->
        maps:remove(Key, Mora0)
    end, Mora, KeyList).


const_text() ->
    <<"ヴョ,ヴュ,ヴャ,ヴォ,ヴェ,ヴィ,ヴァ,ヴ,ン,ヲ,ヱ,ヰ,ワ,ロ,レ,ル,リョ,リュ,リャ,リェ,リ,ラ,ヨ,ョ,ユ,ュ,ヤ,ャ,モ,メ,ム,ミョ,ミュ,ミャ,ミェ,ミ,マ,ポ,ボ,ホ,ペ,ベ,ヘ,プ,ブ,フォ,フェ,フィ,ファ,フ,ピョ,ピュ,ピャ,ピェ,ピ,ビョ,ビュ,ビャ,ビェ,ビ,ヒョ,ヒュ,ヒャ,ヒェ,ヒ,パ,バ,ハ,ノ,ネ,ヌ,ニョ,ニュ,ニャ,ニェ,ニ,ナ,ドゥ,ド,トゥ,ト,デョ,デュ,デャ,ディ,デ,テョ,テュ,テャ,ティ,テ,ヅ,ツォ,ツェ,ツィ,ツァ,ツ,ッ,ヂ,チョ,チュ,チャ,チェ,チ,ダ,タ,ゾ,ソ,ゼ,セ,ズィ,ズ,スィ,ス,ジョ,ジュ,ジャ,ジェ,ジ,ショ,シュ,シャ,シェ,シ,ザ,サ,ゴ,コ,ゲ,ケ,グ,ク,ギョ,ギュ,ギャ,ギェ,ギ,キョ,キュ,キャ,キェ,キ,ガ,カ,オ,ォ,エ,ェ,ウォ,ウェ,ウィ,ウ,ゥ,イェ,イ,ィ,ア,ァ,グヮ,クヮ,ヮ,ヶ"/utf8>>.

const_accent_phrases() ->
    [#{<<"accent">> => 1,<<"is_interrogative">> => false,             
       <<"moras">> =>
           [#{<<"consonant">> => <<"by">>,
              <<"text">> => <<227,131,147,227,131,167>>,
              <<"vowel">> => <<"o">>}],
       <<"pause_mora">> =>
           #{<<"consonant">> => null,
             <<"text">> => <<227,128,129>>,
             <<"vowel">> => <<"pau">>}},
     #{<<"accent">> => 1,<<"is_interrogative">> => false,
       <<"moras">> =>
           [#{<<"consonant">> => <<"by">>,
              <<"text">> => <<227,131,147,227,131,165>>,
              <<"vowel">> => <<"u">>}],
       <<"pause_mora">> =>
           #{<<"consonant">> => null,
             <<"text">> => <<227,128,129>>,
             <<"vowel">> => <<"pau">>}},
     #{<<"accent">> => 1,<<"is_interrogative">> => false,
       <<"moras">> =>
           [#{<<"consonant">> => <<"by">>,
              <<"text">> => <<227,131,147,227,131,163>>,
              <<"vowel">> => <<"a">>}],
       <<"pause_mora">> =>
           #{<<"consonant">> => null,
             <<"text">> => <<227,128,129>>,
             <<"vowel">> => <<"pau">>}},
     #{<<"accent">> => 1,<<"is_interrogative">> => false,
       <<"moras">> =>
           [#{<<"consonant">> => <<"v">>,
              <<"text">> => <<227,131,180,227,130,169>>,
              <<"vowel">> => <<"o">>}],
       <<"pause_mora">> =>
           #{<<"consonant">> => null,
             <<"text">> => <<227,128,129>>,
             <<"vowel">> => <<"pau">>}},
     #{<<"accent">> => 1,<<"is_interrogative">> => false,
       <<"moras">> =>
           [#{<<"consonant">> => <<"v">>,
              <<"text">> => <<227,131,180,227,130,167>>,
              <<"vowel">> => <<"e">>}],
       <<"pause_mora">> =>
           #{<<"consonant">> => null,
             <<"text">> => <<227,128,129>>,
             <<"vowel">> => <<"pau">>}},
     #{<<"accent">> => 1,<<"is_interrogative">> => false,
       <<"moras">> =>
           [#{<<"consonant">> => <<"v">>,
              <<"text">> => <<227,131,180,227,130,163>>,
              <<"vowel">> => <<"i">>}],
       <<"pause_mora">> =>
           #{<<"consonant">> => null, 
             <<"text">> => <<227,128,129>>,
             <<"vowel">> => <<"pau">>}},
     #{<<"accent">> => 1,<<"is_interrogative">> => false,
       <<"moras">> =>
           [#{<<"consonant">> => <<"v">>,
              <<"text">> => <<227,131,180,227,130,161>>, 
              <<"vowel">> => <<"a">>}],
       <<"pause_mora">> =>
           #{<<"consonant">> => null,
             <<"text">> => <<227,128,129>>,
             <<"vowel">> => <<"pau">>}},
     #{<<"accent">> => 1,<<"is_interrogative">> => false,
       <<"moras">> =>
           [#{<<"consonant">> => <<"v">>,
              <<"text">> => <<227,131,180>>, 
              <<"vowel">> => <<"u">>}],
       <<"pause_mora">> =>
           #{<<"consonant">> => null,
             <<"text">> => <<227,128,129>>,
             <<"vowel">> => <<"pau">>}},
     #{<<"accent">> => 1,<<"is_interrogative">> => false,
       <<"moras">> =>
           [#{<<"consonant">> => null,
              <<"text">> => <<227,131,179>>,
              <<"vowel">> => <<"N">>}],
       <<"pause_mora">> =>
           #{<<"consonant">> => null,
             <<"text">> => <<227,128,129>>,
             <<"vowel">> => <<"pau">>}},
     #{<<"accent">> => 1,<<"is_interrogative">> => false,
       <<"moras">> =>
           [#{<<"consonant">> => null,
              <<"text">> => <<227,130,170>>,
              <<"vowel">> => <<"o">>}],
       <<"pause_mora">> =>
           #{<<"consonant">> => null,
             <<"text">> => <<227,128,129>>,
             <<"vowel">> => <<"pau">>}},
     #{<<"accent">> => 1,<<"is_interrogative">> => false,
       <<"moras">> =>
           [#{<<"consonant">> => null,
              <<"text">> => <<227,130,168>>,
              <<"vowel">> => <<"e">>}],
       <<"pause_mora">> =>
           #{<<"consonant">> => null,
             <<"text">> => <<227,128,129>>,
             <<"vowel">> => <<"pau">>}}, 
     #{<<"accent">> => 1,<<"is_interrogative">> => false,
       <<"moras">> =>
           [#{<<"consonant">> => null,
              <<"text">> => <<227,130,164>>,
              <<"vowel">> => <<"i">>}],
       <<"pause_mora">> =>
           #{<<"consonant">> => null, 
             <<"text">> => <<227,128,129>>,
             <<"vowel">> => <<"pau">>}},
     #{<<"accent">> => 1,<<"is_interrogative">> => false,
       <<"moras">> =>
           [#{<<"consonant">> => <<"w">>,
              <<"text">> => <<227,131,175>>,
              <<"vowel">> => <<"a">>}],
       <<"pause_mora">> =>
           #{<<"consonant">> => null, 
             <<"text">> => <<227,128,129>>,
             <<"vowel">> => <<"pau">>}},
     #{<<"accent">> => 1,<<"is_interrogative">> => false,
       <<"moras">> =>
           [#{<<"consonant">> => <<"r">>,
              <<"text">> => <<227,131,173>>,
              <<"vowel">> => <<"o">>}],
       <<"pause_mora">> =>
           #{<<"consonant">> => null, 
             <<"text">> => <<227,128,129>>,
             <<"vowel">> => <<"pau">>}},
     #{<<"accent">> => 1,<<"is_interrogative">> => false,
       <<"moras">> =>
           [#{<<"consonant">> => <<"r">>,
              <<"text">> => <<227,131,172>>,
              <<"vowel">> => <<"e">>}],
       <<"pause_mora">> =>
           #{<<"consonant">> => null, 
             <<"text">> => <<227,128,129>>,
             <<"vowel">> => <<"pau">>}},
     #{<<"accent">> => 1,<<"is_interrogative">> => false,
       <<"moras">> =>
           [#{<<"consonant">> => <<"r">>,
              <<"text">> => <<227,131,171>>,
              <<"vowel">> => <<"u">>}],
       <<"pause_mora">> =>
           #{<<"consonant">> => null, 
             <<"text">> => <<227,128,129>>,
             <<"vowel">> => <<"pau">>}},
     #{<<"accent">> => 1,<<"is_interrogative">> => false,
       <<"moras">> =>
           [#{<<"consonant">> => <<"ry">>,
              <<"text">> => <<227,131,170,227,131,167>>,
              <<"vowel">> => <<"o">>}],
       <<"pause_mora">> =>
           #{<<"consonant">> => null,
             <<"text">> => <<227,128,129>>,
             <<"vowel">> => <<"pau">>}},
     #{<<"accent">> => 1,<<"is_interrogative">> => false,
       <<"moras">> =>
           [#{<<"consonant">> => <<"ry">>,
              <<"text">> => <<227,131,170,227,131,165>>,
              <<"vowel">> => <<"u">>}],
       <<"pause_mora">> =>
           #{<<"consonant">> => null,
             <<"text">> => <<227,128,129>>,
             <<"vowel">> => <<"pau">>}},
     #{<<"accent">> => 1,<<"is_interrogative">> => false,
       <<"moras">> =>
           [#{<<"consonant">> => <<"ry">>,
              <<"text">> => <<227,131,170,227,131,163>>,
              <<"vowel">> => <<"a">>}],
       <<"pause_mora">> =>
           #{<<"consonant">> => null,
             <<"text">> => <<227,128,129>>, 
             <<"vowel">> => <<"pau">>}},
     #{<<"accent">> => 1,<<"is_interrogative">> => false,
       <<"moras">> =>
           [#{<<"consonant">> => <<"ry">>,
              <<"text">> => <<227,131,170,227,130,167>>,
              <<"vowel">> => <<"e">>}],
       <<"pause_mora">> =>
           #{<<"consonant">> => null,
             <<"text">> => <<227,128,129>>,
             <<"vowel">> => <<"pau">>}},
     #{<<"accent">> => 1,<<"is_interrogative">> => false,
       <<"moras">> =>
           [#{<<"consonant">> => <<"r">>, 
              <<"text">> => <<227,131,170>>,
              <<"vowel">> => <<"i">>}],
       <<"pause_mora">> =>
           #{<<"consonant">> => null,
             <<"text">> => <<227,128,129>>,
             <<"vowel">> => <<"pau">>}},
     #{<<"accent">> => 1,<<"is_interrogative">> => false,
       <<"moras">> =>
           [#{<<"consonant">> => <<"r">>, 
              <<"text">> => <<227,131,169>>,
              <<"vowel">> => <<"a">>}],
       <<"pause_mora">> =>
           #{<<"consonant">> => null,
             <<"text">> => <<227,128,129>>,
             <<"vowel">> => <<"pau">>}},
     #{<<"accent">> => 1,<<"is_interrogative">> => false,
       <<"moras">> =>
           [#{<<"consonant">> => <<"y">>, 
              <<"text">> => <<227,131,168>>,
              <<"vowel">> => <<"o">>}],
       <<"pause_mora">> =>
           #{<<"consonant">> => null,
             <<"text">> => <<227,128,129>>,
             <<"vowel">> => <<"pau">>}},
     #{<<"accent">> => 1,<<"is_interrogative">> => false,
       <<"moras">> =>
           [#{<<"consonant">> => <<"y">>, 
              <<"text">> => <<227,131,168>>,
              <<"vowel">> => <<"o">>}],
       <<"pause_mora">> =>
           #{<<"consonant">> => null,
             <<"text">> => <<227,128,129>>,
             <<"vowel">> => <<"pau">>}},
     #{<<"accent">> => 1,<<"is_interrogative">> => false,
       <<"moras">> =>
           [#{<<"consonant">> => <<"y">>, 
              <<"text">> => <<227,131,166>>,
              <<"vowel">> => <<"u">>}],
       <<"pause_mora">> =>
           #{<<"consonant">> => null,
             <<"text">> => <<227,128,129>>,
             <<"vowel">> => <<"pau">>}},
     #{<<"accent">> => 1,<<"is_interrogative">> => false,
       <<"moras">> =>
           [#{<<"consonant">> => <<"y">>, 
              <<"text">> => <<227,131,166>>,
              <<"vowel">> => <<"u">>}],
       <<"pause_mora">> =>
           #{<<"consonant">> => null,
             <<"text">> => <<227,128,129>>,
             <<"vowel">> => <<"pau">>}},
     #{<<"accent">> => 1,<<"is_interrogative">> => false,
       <<"moras">> =>
           [#{<<"consonant">> => <<"y">>, 
              <<"text">> => <<227,131,164>>,
              <<"vowel">> => <<"a">>}],
       <<"pause_mora">> =>
           #{<<"consonant">> => null,
             <<"text">> => <<227,128,129>>,
             <<"vowel">> => <<"pau">>}},
     #{<<"accent">> => 1,<<"is_interrogative">> => false,
       <<"moras">> =>
           [#{<<"consonant">> => <<"y">>, 
              <<"text">> => <<227,131,164>>,
              <<"vowel">> => <<"a">>}],
       <<"pause_mora">> =>
           #{<<"consonant">> => null,
             <<"text">> => <<227,128,129>>,
             <<"vowel">> => <<"pau">>}},
     #{<<"accent">> => 1,<<"is_interrogative">> => false,
       <<"moras">> =>
           [#{<<"consonant">> => <<"m">>, 
              <<"text">> => <<227,131,162>>,
              <<"vowel">> => <<"o">>}],
       <<"pause_mora">> =>
           #{<<"consonant">> => null,
             <<"text">> => <<227,128,129>>,
             <<"vowel">> => <<"pau">>}},
     #{<<"accent">> => 1,<<"is_interrogative">> => false,
       <<"moras">> =>
           [#{<<"consonant">> => <<"m">>, 
              <<"text">> => <<227,131,161>>,
              <<"vowel">> => <<"e">>}],
       <<"pause_mora">> =>
           #{<<"consonant">> => null,
             <<"text">> => <<227,128,129>>,
             <<"vowel">> => <<"pau">>}},
     #{<<"accent">> => 1,<<"is_interrogative">> => false,
       <<"moras">> =>
           [#{<<"consonant">> => <<"m">>, 
              <<"text">> => <<227,131,160>>,
              <<"vowel">> => <<"u">>}],
       <<"pause_mora">> =>
           #{<<"consonant">> => null,
             <<"text">> => <<227,128,129>>,
             <<"vowel">> => <<"pau">>}},
     #{<<"accent">> => 1,<<"is_interrogative">> => false,
       <<"moras">> =>
           [#{<<"consonant">> => <<"my">>,
              <<"text">> => <<227,131,159,227,131,167>>,
              <<"vowel">> => <<"o">>}],
       <<"pause_mora">> =>
           #{<<"consonant">> => null,
             <<"text">> => <<227,128,129>>,
             <<"vowel">> => <<"pau">>}},
     #{<<"accent">> => 1,<<"is_interrogative">> => false,
       <<"moras">> =>
           [#{<<"consonant">> => <<"my">>,
              <<"text">> => <<227,131,159,227,131,165>>,
              <<"vowel">> => <<"u">>}],
       <<"pause_mora">> =>
           #{<<"consonant">> => null, 
             <<"text">> => <<227,128,129>>,
             <<"vowel">> => <<"pau">>}},
     #{<<"accent">> => 1,<<"is_interrogative">> => false,
       <<"moras">> =>
           [#{<<"consonant">> => <<"my">>,
              <<"text">> => <<227,131,159,227,131,163>>,
              <<"vowel">> => <<"a">>}],
       <<"pause_mora">> =>
           #{<<"consonant">> => null,
             <<"text">> => <<227,128,129>>,
             <<"vowel">> => <<"pau">>}},
     #{<<"accent">> => 1,<<"is_interrogative">> => false,
       <<"moras">> =>
           [#{<<"consonant">> => <<"my">>,
              <<"text">> => <<227,131,159,227,130,167>>,
              <<"vowel">> => <<"e">>}],
       <<"pause_mora">> =>
           #{<<"consonant">> => null,
             <<"text">> => <<227,128,129>>,
             <<"vowel">> => <<"pau">>}},
     #{<<"accent">> => 1,<<"is_interrogative">> => false,
       <<"moras">> =>
           [#{<<"consonant">> => <<"m">>,
              <<"text">> => <<227,131,159>>,
              <<"vowel">> => <<"i">>}],
       <<"pause_mora">> =>
           #{<<"consonant">> => null,
             <<"text">> => <<227,128,129>>,
             <<"vowel">> => <<"pau">>}},
     #{<<"accent">> => 1,<<"is_interrogative">> => false,
       <<"moras">> =>
           [#{<<"consonant">> => <<"m">>,
              <<"text">> => <<227,131,158>>,
              <<"vowel">> => <<"a">>}],
       <<"pause_mora">> =>
           #{<<"consonant">> => null,
             <<"text">> => <<227,128,129>>,
             <<"vowel">> => <<"pau">>}},
     #{<<"accent">> => 1,<<"is_interrogative">> => false,
       <<"moras">> =>
           [#{<<"consonant">> => <<"p">>,
              <<"text">> => <<227,131,157>>,
              <<"vowel">> => <<"o">>}],
       <<"pause_mora">> =>
           #{<<"consonant">> => null,
             <<"text">> => <<227,128,129>>,
             <<"vowel">> => <<"pau">>}},
     #{<<"accent">> => 1,<<"is_interrogative">> => false,
       <<"moras">> =>
           [#{<<"consonant">> => <<"b">>,
              <<"text">> => <<227,131,156>>,
              <<"vowel">> => <<"o">>}],
       <<"pause_mora">> =>
           #{<<"consonant">> => null,
             <<"text">> => <<227,128,129>>,
             <<"vowel">> => <<"pau">>}},
     #{<<"accent">> => 1,<<"is_interrogative">> => false,
       <<"moras">> =>
           [#{<<"consonant">> => <<"h">>,
              <<"text">> => <<227,131,155>>,
              <<"vowel">> => <<"o">>}],
       <<"pause_mora">> =>
           #{<<"consonant">> => null,
             <<"text">> => <<227,128,129>>,
             <<"vowel">> => <<"pau">>}},
     #{<<"accent">> => 1,<<"is_interrogative">> => false,
       <<"moras">> =>
           [#{<<"consonant">> => <<"p">>,
              <<"text">> => <<227,131,154>>,
              <<"vowel">> => <<"e">>}],
       <<"pause_mora">> =>
           #{<<"consonant">> => null,
             <<"text">> => <<227,128,129>>,
             <<"vowel">> => <<"pau">>}},
     #{<<"accent">> => 1,<<"is_interrogative">> => false,
       <<"moras">> =>
           [#{<<"consonant">> => <<"b">>,
              <<"text">> => <<227,131,153>>,
              <<"vowel">> => <<"e">>}],
       <<"pause_mora">> =>
           #{<<"consonant">> => null,
             <<"text">> => <<227,128,129>>,
             <<"vowel">> => <<"pau">>}},
     #{<<"accent">> => 1,<<"is_interrogative">> => false,
       <<"moras">> =>
           [#{<<"consonant">> => null,
              <<"text">> => <<227,130,168>>,
              <<"vowel">> => <<"e">>}],
       <<"pause_mora">> =>
           #{<<"consonant">> => null,
             <<"text">> => <<227,128,129>>,
             <<"vowel">> => <<"pau">>}},
     #{<<"accent">> => 1,<<"is_interrogative">> => false,
       <<"moras">> =>
           [#{<<"consonant">> => <<"p">>,
              <<"text">> => <<227,131,151>>,
              <<"vowel">> => <<"u">>}],
       <<"pause_mora">> =>
           #{<<"consonant">> => null,
             <<"text">> => <<227,128,129>>,
             <<"vowel">> => <<"pau">>}},
     #{<<"accent">> => 1,<<"is_interrogative">> => false,
       <<"moras">> =>
           [#{<<"consonant">> => <<"b">>,
              <<"text">> => <<227,131,150>>,
              <<"vowel">> => <<"u">>}],
       <<"pause_mora">> =>
           #{<<"consonant">> => null,
             <<"text">> => <<227,128,129>>,
             <<"vowel">> => <<"pau">>}},
     #{<<"accent">> => 1,<<"is_interrogative">> => false,
       <<"moras">> =>
           [#{<<"consonant">> => <<"f">>,
              <<"text">> => <<227,131,149,227,130,169>>,
              <<"vowel">> => <<"o">>}],
       <<"pause_mora">> =>
           #{<<"consonant">> => null,
             <<"text">> => <<227,128,129>>,
             <<"vowel">> => <<"pau">>}},
     #{<<"accent">> => 1,<<"is_interrogative">> => false,
       <<"moras">> =>
           [#{<<"consonant">> => <<"f">>,
              <<"text">> => <<227,131,149,227,130,167>>,
              <<"vowel">> => <<"e">>}],
       <<"pause_mora">> =>
           #{<<"consonant">> => null,
             <<"text">> => <<227,128,129>>,
             <<"vowel">> => <<"pau">>}},
     #{<<"accent">> => 1,<<"is_interrogative">> => false,
       <<"moras">> =>
           [#{<<"consonant">> => <<"f">>,
              <<"text">> => <<227,131,149,227,130,163>>,
              <<"vowel">> => <<"i">>}],
       <<"pause_mora">> =>
           #{<<"consonant">> => null,
             <<"text">> => <<227,128,129>>,
             <<"vowel">> => <<"pau">>}},
     #{<<"accent">> => 1,<<"is_interrogative">> => false,
       <<"moras">> =>
           [#{<<"consonant">> => <<"f">>,
              <<"text">> => <<227,131,149,227,130,161>>,
              <<"vowel">> => <<"a">>}],
       <<"pause_mora">> =>
           #{<<"consonant">> => null,
             <<"text">> => <<227,128,129>>,
             <<"vowel">> => <<"pau">>}},
     #{<<"accent">> => 1,<<"is_interrogative">> => false,
       <<"moras">> =>
           [#{<<"consonant">> => <<"f">>,
              <<"text">> => <<227,131,149>>,
              <<"vowel">> => <<"u">>}],
       <<"pause_mora">> =>
           #{<<"consonant">> => null,
             <<"text">> => <<227,128,129>>,
             <<"vowel">> => <<"pau">>}},
     #{<<"accent">> => 1,<<"is_interrogative">> => false,
       <<"moras">> =>
           [#{<<"consonant">> => <<"py">>,
              <<"text">> => <<227,131,148,227,131,167>>,
              <<"vowel">> => <<"o">>}],
       <<"pause_mora">> =>
           #{<<"consonant">> => null,
             <<"text">> => <<227,128,129>>,
             <<"vowel">> => <<"pau">>}},
     #{<<"accent">> => 1,<<"is_interrogative">> => false,
       <<"moras">> =>
           [#{<<"consonant">> => <<"py">>,
              <<"text">> => <<227,131,148,227,131,165>>,
              <<"vowel">> => <<"u">>}],
       <<"pause_mora">> =>
           #{<<"consonant">> => null,
             <<"text">> => <<227,128,129>>,
             <<"vowel">> => <<"pau">>}},
     #{<<"accent">> => 1,<<"is_interrogative">> => false,
       <<"moras">> =>
           [#{<<"consonant">> => <<"py">>,
              <<"text">> => <<227,131,148,227,131,163>>,
              <<"vowel">> => <<"a">>}],
       <<"pause_mora">> =>
           #{<<"consonant">> => null,
             <<"text">> => <<227,128,129>>,
             <<"vowel">> => <<"pau">>}},
     #{<<"accent">> => 1,<<"is_interrogative">> => false,
       <<"moras">> =>
           [#{<<"consonant">> => <<"py">>,
              <<"text">> => <<227,131,148,227,130,167>>,
              <<"vowel">> => <<"e">>}],
       <<"pause_mora">> =>
           #{<<"consonant">> => null,
             <<"text">> => <<227,128,129>>,
             <<"vowel">> => <<"pau">>}},
     #{<<"accent">> => 1,<<"is_interrogative">> => false,
       <<"moras">> =>
           [#{<<"consonant">> => <<"p">>,
              <<"text">> => <<227,131,148>>,
              <<"vowel">> => <<"i">>}],
       <<"pause_mora">> =>
           #{<<"consonant">> => null,
             <<"text">> => <<227,128,129>>,
             <<"vowel">> => <<"pau">>}},
     #{<<"accent">> => 1,<<"is_interrogative">> => false,
       <<"moras">> =>
           [#{<<"consonant">> => <<"by">>,
              <<"text">> => <<227,131,147,227,131,167>>,
              <<"vowel">> => <<"o">>}],
       <<"pause_mora">> =>
           #{<<"consonant">> => null,
             <<"text">> => <<227,128,129>>,
             <<"vowel">> => <<"pau">>}},
     #{<<"accent">> => 1,<<"is_interrogative">> => false,
       <<"moras">> =>
           [#{<<"consonant">> => <<"by">>,
              <<"text">> => <<227,131,147,227,131,165>>,
              <<"vowel">> => <<"u">>}],
       <<"pause_mora">> =>
           #{<<"consonant">> => null,
             <<"text">> => <<227,128,129>>,
             <<"vowel">> => <<"pau">>}},
     #{<<"accent">> => 1,<<"is_interrogative">> => false,
       <<"moras">> =>
           [#{<<"consonant">> => <<"by">>,
              <<"text">> => <<227,131,147,227,131,163>>,
              <<"vowel">> => <<"a">>}],
       <<"pause_mora">> =>
           #{<<"consonant">> => null,
             <<"text">> => <<227,128,129>>,
             <<"vowel">> => <<"pau">>}},
     #{<<"accent">> => 1,<<"is_interrogative">> => false,
       <<"moras">> =>
           [#{<<"consonant">> => <<"by">>,
              <<"text">> => <<227,131,147,227,130,167>>,
              <<"vowel">> => <<"e">>}],
       <<"pause_mora">> =>
           #{<<"consonant">> => null,
             <<"text">> => <<227,128,129>>,
             <<"vowel">> => <<"pau">>}},
     #{<<"accent">> => 1,<<"is_interrogative">> => false,
       <<"moras">> =>
           [#{<<"consonant">> => <<"b">>,
              <<"text">> => <<227,131,147>>,
              <<"vowel">> => <<"i">>}],
       <<"pause_mora">> =>
           #{<<"consonant">> => null,
             <<"text">> => <<227,128,129>>,
             <<"vowel">> => <<"pau">>}},
     #{<<"accent">> => 1,<<"is_interrogative">> => false,
       <<"moras">> =>
           [#{<<"consonant">> => <<"hy">>,
              <<"text">> => <<227,131,146,227,131,167>>,
              <<"vowel">> => <<"o">>}],
       <<"pause_mora">> =>
           #{<<"consonant">> => null,
             <<"text">> => <<227,128,129>>,
             <<"vowel">> => <<"pau">>}},
     #{<<"accent">> => 1,<<"is_interrogative">> => false,
       <<"moras">> =>
           [#{<<"consonant">> => <<"hy">>,
              <<"text">> => <<227,131,146,227,131,165>>,
              <<"vowel">> => <<"u">>}],
       <<"pause_mora">> =>
           #{<<"consonant">> => null,
             <<"text">> => <<227,128,129>>,
             <<"vowel">> => <<"pau">>}},
     #{<<"accent">> => 1,<<"is_interrogative">> => false,
       <<"moras">> =>
           [#{<<"consonant">> => <<"hy">>,
              <<"text">> => <<227,131,146,227,131,163>>,
              <<"vowel">> => <<"a">>}],
       <<"pause_mora">> =>
           #{<<"consonant">> => null,
             <<"text">> => <<227,128,129>>,
             <<"vowel">> => <<"pau">>}},
     #{<<"accent">> => 1,<<"is_interrogative">> => false,
       <<"moras">> =>
           [#{<<"consonant">> => <<"hy">>,
              <<"text">> => <<227,131,146,227,130,167>>,
              <<"vowel">> => <<"e">>}],
       <<"pause_mora">> =>
           #{<<"consonant">> => null,
             <<"text">> => <<227,128,129>>,
             <<"vowel">> => <<"pau">>}},
     #{<<"accent">> => 1,<<"is_interrogative">> => false,
       <<"moras">> =>
           [#{<<"consonant">> => <<"h">>,
              <<"text">> => <<227,131,146>>,
              <<"vowel">> => <<"i">>}],
       <<"pause_mora">> =>
           #{<<"consonant">> => null,
             <<"text">> => <<227,128,129>>,
             <<"vowel">> => <<"pau">>}},
     #{<<"accent">> => 1,<<"is_interrogative">> => false,
       <<"moras">> =>
           [#{<<"consonant">> => <<"p">>,
              <<"text">> => <<227,131,145>>,
              <<"vowel">> => <<"a">>}],
       <<"pause_mora">> =>
           #{<<"consonant">> => null,
             <<"text">> => <<227,128,129>>,
             <<"vowel">> => <<"pau">>}},
     #{<<"accent">> => 1,<<"is_interrogative">> => false,
       <<"moras">> =>
           [#{<<"consonant">> => <<"b">>,
              <<"text">> => <<227,131,144>>,
              <<"vowel">> => <<"a">>}],
       <<"pause_mora">> =>
           #{<<"consonant">> => null,
             <<"text">> => <<227,128,129>>,
             <<"vowel">> => <<"pau">>}},
     #{<<"accent">> => 1,<<"is_interrogative">> => false,
       <<"moras">> =>
           [#{<<"consonant">> => <<"h">>,
              <<"text">> => <<227,131,143>>,
              <<"vowel">> => <<"a">>}],
       <<"pause_mora">> =>
           #{<<"consonant">> => null,
             <<"text">> => <<227,128,129>>,
             <<"vowel">> => <<"pau">>}},
     #{<<"accent">> => 1,<<"is_interrogative">> => false,
       <<"moras">> =>
           [#{<<"consonant">> => <<"n">>,
              <<"text">> => <<227,131,142>>,
              <<"vowel">> => <<"o">>}],
       <<"pause_mora">> =>
           #{<<"consonant">> => null,
             <<"text">> => <<227,128,129>>,
             <<"vowel">> => <<"pau">>}},
     #{<<"accent">> => 1,<<"is_interrogative">> => false,
       <<"moras">> =>
           [#{<<"consonant">> => <<"n">>,
              <<"text">> => <<227,131,141>>,
              <<"vowel">> => <<"e">>}],
       <<"pause_mora">> =>
           #{<<"consonant">> => null,
             <<"text">> => <<227,128,129>>,
             <<"vowel">> => <<"pau">>}},
     #{<<"accent">> => 1,<<"is_interrogative">> => false,
       <<"moras">> =>
           [#{<<"consonant">> => <<"n">>,
              <<"text">> => <<227,131,140>>,
              <<"vowel">> => <<"u">>}],
       <<"pause_mora">> =>
           #{<<"consonant">> => null,
             <<"text">> => <<227,128,129>>,
             <<"vowel">> => <<"pau">>}},
     #{<<"accent">> => 1,<<"is_interrogative">> => false,
       <<"moras">> =>
           [#{<<"consonant">> => <<"ny">>,
              <<"text">> => <<227,131,139,227,131,167>>,
              <<"vowel">> => <<"o">>}],
       <<"pause_mora">> =>
           #{<<"consonant">> => null,
             <<"text">> => <<227,128,129>>,
             <<"vowel">> => <<"pau">>}},
     #{<<"accent">> => 1,<<"is_interrogative">> => false,
       <<"moras">> =>
           [#{<<"consonant">> => <<"ny">>,
              <<"text">> => <<227,131,139,227,131,165>>,
              <<"vowel">> => <<"u">>}],
       <<"pause_mora">> =>
           #{<<"consonant">> => null,
             <<"text">> => <<227,128,129>>,
             <<"vowel">> => <<"pau">>}},
     #{<<"accent">> => 1,<<"is_interrogative">> => false,
       <<"moras">> =>
           [#{<<"consonant">> => <<"ny">>,
              <<"text">> => <<227,131,139,227,131,163>>,
              <<"vowel">> => <<"a">>}],
       <<"pause_mora">> =>
           #{<<"consonant">> => null,
             <<"text">> => <<227,128,129>>,
             <<"vowel">> => <<"pau">>}},
     #{<<"accent">> => 1,<<"is_interrogative">> => false,
       <<"moras">> =>
           [#{<<"consonant">> => <<"ny">>,
              <<"text">> => <<227,131,139,227,130,167>>,
              <<"vowel">> => <<"e">>}],
       <<"pause_mora">> =>
           #{<<"consonant">> => null,
             <<"text">> => <<227,128,129>>,
             <<"vowel">> => <<"pau">>}},
     #{<<"accent">> => 1,<<"is_interrogative">> => false,
       <<"moras">> =>
           [#{<<"consonant">> => <<"n">>,
              <<"text">> => <<227,131,139>>,
              <<"vowel">> => <<"i">>}],
       <<"pause_mora">> =>
           #{<<"consonant">> => null,
             <<"text">> => <<227,128,129>>,
             <<"vowel">> => <<"pau">>}},
     #{<<"accent">> => 1,<<"is_interrogative">> => false,
       <<"moras">> =>
           [#{<<"consonant">> => <<"n">>,
              <<"text">> => <<227,131,138>>,
              <<"vowel">> => <<"a">>}],
       <<"pause_mora">> =>
           #{<<"consonant">> => null,
             <<"text">> => <<227,128,129>>,
             <<"vowel">> => <<"pau">>}},
     #{<<"accent">> => 1,<<"is_interrogative">> => false,
       <<"moras">> =>
           [#{<<"consonant">> => <<"d">>,
              <<"text">> => <<227,131,137,227,130,165>>,
              <<"vowel">> => <<"u">>}],
       <<"pause_mora">> =>
           #{<<"consonant">> => null,
             <<"text">> => <<227,128,129>>,
             <<"vowel">> => <<"pau">>}},
     #{<<"accent">> => 1,<<"is_interrogative">> => false,
       <<"moras">> =>
           [#{<<"consonant">> => <<"d">>,
              <<"text">> => <<227,131,137>>,
              <<"vowel">> => <<"o">>}],
       <<"pause_mora">> =>
           #{<<"consonant">> => null,
             <<"text">> => <<227,128,129>>,
             <<"vowel">> => <<"pau">>}},
     #{<<"accent">> => 1,<<"is_interrogative">> => false,
       <<"moras">> =>
           [#{<<"consonant">> => <<"t">>,
              <<"text">> => <<227,131,136>>,
              <<"vowel">> => <<"o">>},
            #{<<"consonant">> => null,
              <<"text">> => <<"ー"/utf8>>, % おそらく辞書に「トゥ」があるせい
              <<"vowel">> => <<"o">>}],
       <<"pause_mora">> =>
           #{<<"consonant">> => null,
             <<"text">> => <<227,128,129>>,
             <<"vowel">> => <<"pau">>}},
     #{<<"accent">> => 1,<<"is_interrogative">> => false,
       <<"moras">> =>
           [#{<<"consonant">> => <<"t">>,
              <<"text">> => <<227,131,136>>,
              <<"vowel">> => <<"o">>}],
       <<"pause_mora">> =>
           #{<<"consonant">> => null,
             <<"text">> => <<227,128,129>>,
             <<"vowel">> => <<"pau">>}},
     #{<<"accent">> => 1,<<"is_interrogative">> => false,
       <<"moras">> =>
           [#{<<"consonant">> => <<"dy">>,
              <<"text">> => <<227,131,135,227,131,167>>,
              <<"vowel">> => <<"o">>}],
       <<"pause_mora">> =>
           #{<<"consonant">> => null,
             <<"text">> => <<227,128,129>>,
             <<"vowel">> => <<"pau">>}},
     #{<<"accent">> => 1,<<"is_interrogative">> => false,
       <<"moras">> =>
           [#{<<"consonant">> => <<"dy">>,
              <<"text">> => <<227,131,135,227,131,165>>,
              <<"vowel">> => <<"u">>}],
       <<"pause_mora">> =>
           #{<<"consonant">> => null,
             <<"text">> => <<227,128,129>>,
             <<"vowel">> => <<"pau">>}},
     #{<<"accent">> => 1,<<"is_interrogative">> => false,
       <<"moras">> =>
           [#{<<"consonant">> => <<"dy">>,
              <<"text">> => <<227,131,135,227,131,163>>,
              <<"vowel">> => <<"a">>}],
       <<"pause_mora">> =>
           #{<<"consonant">> => null,
             <<"text">> => <<227,128,129>>,
             <<"vowel">> => <<"pau">>}},
     #{<<"accent">> => 1,<<"is_interrogative">> => false,
       <<"moras">> =>
           [#{<<"consonant">> => <<"d">>,
              <<"text">> => <<227,131,135,227,130,163>>,
              <<"vowel">> => <<"i">>}],
       <<"pause_mora">> =>
           #{<<"consonant">> => null,
             <<"text">> => <<227,128,129>>,
             <<"vowel">> => <<"pau">>}},
     #{<<"accent">> => 1,<<"is_interrogative">> => false,
       <<"moras">> =>
           [#{<<"consonant">> => <<"d">>,
              <<"text">> => <<227,131,135>>,
              <<"vowel">> => <<"e">>}],
       <<"pause_mora">> =>
           #{<<"consonant">> => null,
             <<"text">> => <<227,128,129>>,
             <<"vowel">> => <<"pau">>}},
     #{<<"accent">> => 1,<<"is_interrogative">> => false,
       <<"moras">> =>
           [#{<<"consonant">> => <<"ty">>,
              <<"text">> => <<227,131,134,227,131,167>>,
              <<"vowel">> => <<"o">>}],
       <<"pause_mora">> =>
           #{<<"consonant">> => null,
             <<"text">> => <<227,128,129>>,
             <<"vowel">> => <<"pau">>}},
     #{<<"accent">> => 1,<<"is_interrogative">> => false,
       <<"moras">> =>
           [#{<<"consonant">> => <<"ty">>,
              <<"text">> => <<227,131,134,227,131,165>>,
              <<"vowel">> => <<"u">>}],
       <<"pause_mora">> =>
           #{<<"consonant">> => null,
             <<"text">> => <<227,128,129>>,
             <<"vowel">> => <<"pau">>}},
     #{<<"accent">> => 1,<<"is_interrogative">> => false,
       <<"moras">> =>
           [#{<<"consonant">> => <<"ty">>,
              <<"text">> => <<227,131,134,227,131,163>>,
              <<"vowel">> => <<"a">>}],
       <<"pause_mora">> =>
           #{<<"consonant">> => null,
             <<"text">> => <<227,128,129>>,
             <<"vowel">> => <<"pau">>}},
     #{<<"accent">> => 1,<<"is_interrogative">> => false,
       <<"moras">> =>
           [#{<<"consonant">> => <<"t">>,
              <<"text">> => <<227,131,134,227,130,163>>,
              <<"vowel">> => <<"i">>}],
       <<"pause_mora">> => 
           #{<<"consonant">> => null,
             <<"text">> => <<227,128,129>>,
             <<"vowel">> => <<"pau">>}},
     #{<<"accent">> => 1,<<"is_interrogative">> => false,
       <<"moras">> =>
           [#{<<"consonant">> => <<"t">>,
              <<"text">> => <<227,131,134>>,
              <<"vowel">> => <<"e">>}],
       <<"pause_mora">> => 
           #{<<"consonant">> => null,
             <<"text">> => <<227,128,129>>,
             <<"vowel">> => <<"pau">>}},
     #{<<"accent">> => 1,<<"is_interrogative">> => false,
       <<"moras">> =>
           [#{<<"consonant">> => <<"z">>,
              <<"text">> => <<227,130,186>>,
              <<"vowel">> => <<"u">>}],
       <<"pause_mora">> => 
           #{<<"consonant">> => null,
             <<"text">> => <<227,128,129>>,
             <<"vowel">> => <<"pau">>}},
     #{<<"accent">> => 1,<<"is_interrogative">> => false,
       <<"moras">> =>
           [#{<<"consonant">> => <<"ts">>,
              <<"text">> => <<227,131,132,227,130,169>>,
              <<"vowel">> => <<"o">>}],
       <<"pause_mora">> =>
           #{<<"consonant">> => null,
             <<"text">> => <<227,128,129>>,
             <<"vowel">> => <<"pau">>}},
     #{<<"accent">> => 1,<<"is_interrogative">> => false,
       <<"moras">> =>
           [#{<<"consonant">> => <<"ts">>,
              <<"text">> => <<227,131,132,227,130,167>>,
              <<"vowel">> => <<"e">>}],
       <<"pause_mora">> =>
           #{<<"consonant">> => null,
             <<"text">> => <<227,128,129>>,
             <<"vowel">> => <<"pau">>}},
     #{<<"accent">> => 1,<<"is_interrogative">> => false,
       <<"moras">> =>
           [#{<<"consonant">> => <<"ts">>,
              <<"text">> => <<227,131,132,227,130,163>>,
              <<"vowel">> => <<"i">>}],
       <<"pause_mora">> =>
           #{<<"consonant">> => null,
             <<"text">> => <<227,128,129>>,
             <<"vowel">> => <<"pau">>}},
     #{<<"accent">> => 1,<<"is_interrogative">> => false,
       <<"moras">> =>
           [#{<<"consonant">> => <<"ts">>,
              <<"text">> => <<227,131,132>>,
              <<"vowel">> => <<"u">>}],
       <<"pause_mora">> => null},
     #{<<"accent">> => 1,<<"is_interrogative">> => false,
       <<"moras">> =>
           [#{<<"consonant">> => null,
              <<"text">> => <<227,130,162>>,
              <<"vowel">> => <<"a">>}],
       <<"pause_mora">> =>
           #{<<"consonant">> => null,
             <<"text">> => <<227,128,129>>,
             <<"vowel">> => <<"pau">>}},
     #{<<"accent">> => 1,<<"is_interrogative">> => false,
       <<"moras">> =>
           [#{<<"consonant">> => <<"ts">>,
              <<"text">> => <<227,131,132>>,
              <<"vowel">> => <<"u">>}],
       <<"pause_mora">> =>
           #{<<"consonant">> => null,
             <<"text">> => <<227,128,129>>,
             <<"vowel">> => <<"pau">>}},
     #{<<"accent">> => 1,<<"is_interrogative">> => false,
       <<"moras">> =>
           [#{<<"consonant">> => null,
              <<"text">> => <<227,131,131>>,
              <<"vowel">> => <<"cl">>}],
       <<"pause_mora">> =>
           #{<<"consonant">> => null,
             <<"text">> => <<227,128,129>>,
             <<"vowel">> => <<"pau">>}},
     #{<<"accent">> => 1,<<"is_interrogative">> => false,
       <<"moras">> =>
           [#{<<"consonant">> => <<"j">>,
              <<"text">> => <<227,130,184>>,
              <<"vowel">> => <<"i">>}],
       <<"pause_mora">> =>
           #{<<"consonant">> => null,
             <<"text">> => <<227,128,129>>,
             <<"vowel">> => <<"pau">>}},
     #{<<"accent">> => 1,<<"is_interrogative">> => false,
       <<"moras">> =>
           [#{<<"consonant">> => <<"ch">>,
              <<"text">> => <<227,131,129,227,131,167>>,
              <<"vowel">> => <<"o">>}],
       <<"pause_mora">> =>
           #{<<"consonant">> => null,
             <<"text">> => <<227,128,129>>,
             <<"vowel">> => <<"pau">>}},
     #{<<"accent">> => 1,<<"is_interrogative">> => false,
       <<"moras">> =>
           [#{<<"consonant">> => <<"ch">>,
              <<"text">> => <<227,131,129,227,131,165>>,
              <<"vowel">> => <<"u">>}],
       <<"pause_mora">> =>
           #{<<"consonant">> => null,
             <<"text">> => <<227,128,129>>,
             <<"vowel">> => <<"pau">>}},
     #{<<"accent">> => 1,<<"is_interrogative">> => false,
       <<"moras">> =>
           [#{<<"consonant">> => <<"ch">>,
              <<"text">> => <<227,131,129,227,131,163>>,
              <<"vowel">> => <<"a">>}],
       <<"pause_mora">> =>
           #{<<"consonant">> => null,
             <<"text">> => <<227,128,129>>,
             <<"vowel">> => <<"pau">>}},
     #{<<"accent">> => 1,<<"is_interrogative">> => false,
       <<"moras">> =>
           [#{<<"consonant">> => <<"ch">>,
              <<"text">> => <<227,131,129,227,130,167>>,
              <<"vowel">> => <<"e">>}],
       <<"pause_mora">> =>
           #{<<"consonant">> => null,
             <<"text">> => <<227,128,129>>,
             <<"vowel">> => <<"pau">>}},
     #{<<"accent">> => 1,<<"is_interrogative">> => false,
       <<"moras">> =>
           [#{<<"consonant">> => <<"ch">>,
              <<"text">> => <<227,131,129>>,
              <<"vowel">> => <<"i">>}],
       <<"pause_mora">> =>
           #{<<"consonant">> => null,
             <<"text">> => <<227,128,129>>,
             <<"vowel">> => <<"pau">>}},
     #{<<"accent">> => 1,<<"is_interrogative">> => false,
       <<"moras">> =>
           [#{<<"consonant">> => <<"d">>,
              <<"text">> => <<227,131,128>>,
              <<"vowel">> => <<"a">>}],
       <<"pause_mora">> =>
           #{<<"consonant">> => null,
             <<"text">> => <<227,128,129>>,
             <<"vowel">> => <<"pau">>}},
     #{<<"accent">> => 1,<<"is_interrogative">> => false,
       <<"moras">> =>
           [#{<<"consonant">> => <<"t">>,
              <<"text">> => <<227,130,191>>,
              <<"vowel">> => <<"a">>}],
       <<"pause_mora">> =>
           #{<<"consonant">> => null,
             <<"text">> => <<227,128,129>>,
             <<"vowel">> => <<"pau">>}},
     #{<<"accent">> => 1,<<"is_interrogative">> => false,
       <<"moras">> =>
           [#{<<"consonant">> => <<"z">>,
              <<"text">> => <<227,130,190>>,
              <<"vowel">> => <<"o">>}],
       <<"pause_mora">> =>
           #{<<"consonant">> => null,
             <<"text">> => <<227,128,129>>,
             <<"vowel">> => <<"pau">>}},
     #{<<"accent">> => 1,<<"is_interrogative">> => false,
       <<"moras">> =>
           [#{<<"consonant">> => <<"s">>,
              <<"text">> => <<227,130,189>>,
              <<"vowel">> => <<"o">>}],
       <<"pause_mora">> =>
           #{<<"consonant">> => null,
             <<"text">> => <<227,128,129>>,
             <<"vowel">> => <<"pau">>}},
     #{<<"accent">> => 1,<<"is_interrogative">> => false,
       <<"moras">> =>
           [#{<<"consonant">> => <<"z">>,
              <<"text">> => <<227,130,188>>,
              <<"vowel">> => <<"e">>}],
       <<"pause_mora">> =>
           #{<<"consonant">> => null,
             <<"text">> => <<227,128,129>>,
             <<"vowel">> => <<"pau">>}},
     #{<<"accent">> => 1,<<"is_interrogative">> => false,
       <<"moras">> =>
           [#{<<"consonant">> => <<"s">>,
              <<"text">> => <<227,130,187>>,
              <<"vowel">> => <<"e">>}],
       <<"pause_mora">> =>
           #{<<"consonant">> => null,
             <<"text">> => <<227,128,129>>,
             <<"vowel">> => <<"pau">>}},
     #{<<"accent">> => 1,<<"is_interrogative">> => false,
       <<"moras">> =>
           [#{<<"consonant">> => <<"z">>,
              <<"text">> => <<227,130,186,227,130,163>>,
              <<"vowel">> => <<"i">>}],
       <<"pause_mora">> => 
           #{<<"consonant">> => null,
             <<"text">> => <<227,128,129>>,
             <<"vowel">> => <<"pau">>}},
     #{<<"accent">> => 1,<<"is_interrogative">> => false,
       <<"moras">> =>
           [#{<<"consonant">> => <<"z">>,
              <<"text">> => <<227,130,186>>,
              <<"vowel">> => <<"u">>}],
       <<"pause_mora">> => 
           #{<<"consonant">> => null,
             <<"text">> => <<227,128,129>>,
             <<"vowel">> => <<"pau">>}},
     #{<<"accent">> => 1,<<"is_interrogative">> => false,
       <<"moras">> =>
           [#{<<"consonant">> => <<"s">>,
              <<"text">> => <<227,130,185,227,130,163>>,
              <<"vowel">> => <<"i">>}],
       <<"pause_mora">> =>
           #{<<"consonant">> => null,
             <<"text">> => <<227,128,129>>,
             <<"vowel">> => <<"pau">>}},
     #{<<"accent">> => 1,<<"is_interrogative">> => false,
       <<"moras">> =>
           [#{<<"consonant">> => <<"s">>,
              <<"text">> => <<227,130,185>>,
              <<"vowel">> => <<"u">>}],
       <<"pause_mora">> =>
           #{<<"consonant">> => null,
             <<"text">> => <<227,128,129>>,
             <<"vowel">> => <<"pau">>}},
     #{<<"accent">> => 1,<<"is_interrogative">> => false,
       <<"moras">> =>
           [#{<<"consonant">> => <<"j">>,
              <<"text">> => <<227,130,184,227,131,167>>,
              <<"vowel">> => <<"o">>}],
       <<"pause_mora">> =>
           #{<<"consonant">> => null,
             <<"text">> => <<227,128,129>>,
             <<"vowel">> => <<"pau">>}},
     #{<<"accent">> => 1,<<"is_interrogative">> => false,
       <<"moras">> =>
           [#{<<"consonant">> => <<"j">>,
              <<"text">> => <<227,130,184,227,131,165>>,
              <<"vowel">> => <<"u">>}],
       <<"pause_mora">> =>
           #{<<"consonant">> => null,
             <<"text">> => <<227,128,129>>,
             <<"vowel">> => <<"pau">>}},
     #{<<"accent">> => 1,<<"is_interrogative">> => false,
       <<"moras">> =>
           [#{<<"consonant">> => <<"j">>,
              <<"text">> => <<227,130,184,227,131,163>>,
              <<"vowel">> => <<"a">>}],
       <<"pause_mora">> =>
           #{<<"consonant">> => null,
             <<"text">> => <<227,128,129>>,
             <<"vowel">> => <<"pau">>}}, 
     #{<<"accent">> => 1,<<"is_interrogative">> => false,
       <<"moras">> =>
           [#{<<"consonant">> => <<"j">>,
              <<"text">> => <<227,130,184,227,130,167>>,
              <<"vowel">> => <<"e">>}],
       <<"pause_mora">> =>
           #{<<"consonant">> => null,
             <<"text">> => <<227,128,129>>,
             <<"vowel">> => <<"pau">>}},
     #{<<"accent">> => 1,<<"is_interrogative">> => false,
       <<"moras">> =>
           [#{<<"consonant">> => <<"j">>,
              <<"text">> => <<227,130,184>>,
              <<"vowel">> => <<"i">>}],
       <<"pause_mora">> =>
           #{<<"consonant">> => null,
             <<"text">> => <<227,128,129>>,
             <<"vowel">> => <<"pau">>}},
     #{<<"accent">> => 1,<<"is_interrogative">> => false,
       <<"moras">> =>
           [#{<<"consonant">> => <<"sh">>,
              <<"text">> => <<227,130,183,227,131,167>>,
              <<"vowel">> => <<"o">>}],
       <<"pause_mora">> =>
           #{<<"consonant">> => null,
             <<"text">> => <<227,128,129>>,
             <<"vowel">> => <<"pau">>}},
     #{<<"accent">> => 1,<<"is_interrogative">> => false, 
       <<"moras">> =>
           [#{<<"consonant">> => <<"sh">>,
              <<"text">> => <<227,130,183,227,131,165>>,
              <<"vowel">> => <<"u">>}],
       <<"pause_mora">> =>
           #{<<"consonant">> => null,
             <<"text">> => <<227,128,129>>,
             <<"vowel">> => <<"pau">>}},
     #{<<"accent">> => 1,<<"is_interrogative">> => false,
       <<"moras">> =>
           [#{<<"consonant">> => <<"sh">>,
              <<"text">> => <<227,130,183,227,131,163>>,
              <<"vowel">> => <<"a">>}],
       <<"pause_mora">> =>
           #{<<"consonant">> => null,
             <<"text">> => <<227,128,129>>,
             <<"vowel">> => <<"pau">>}},
     #{<<"accent">> => 1,<<"is_interrogative">> => false,
       <<"moras">> =>
           [#{<<"consonant">> => <<"sh">>,
              <<"text">> => <<227,130,183,227,130,167>>,
              <<"vowel">> => <<"e">>}],
       <<"pause_mora">> =>
           #{<<"consonant">> => null,
             <<"text">> => <<227,128,129>>,
             <<"vowel">> => <<"pau">>}},
     #{<<"accent">> => 1,<<"is_interrogative">> => false,
       <<"moras">> =>
           [#{<<"consonant">> => <<"sh">>,
              <<"text">> => <<227,130,183>>,
              <<"vowel">> => <<"i">>}],
       <<"pause_mora">> =>
           #{<<"consonant">> => null,
             <<"text">> => <<227,128,129>>,
             <<"vowel">> => <<"pau">>}},
     #{<<"accent">> => 1,<<"is_interrogative">> => false,
       <<"moras">> =>
           [#{<<"consonant">> => <<"z">>,
              <<"text">> => <<227,130,182>>,
              <<"vowel">> => <<"a">>}],
       <<"pause_mora">> =>
           #{<<"consonant">> => null,
             <<"text">> => <<227,128,129>>,
             <<"vowel">> => <<"pau">>}},
     #{<<"accent">> => 1,<<"is_interrogative">> => false,
       <<"moras">> =>
           [#{<<"consonant">> => <<"s">>,
              <<"text">> => <<227,130,181>>,
              <<"vowel">> => <<"a">>}],
       <<"pause_mora">> =>
           #{<<"consonant">> => null,
             <<"text">> => <<227,128,129>>,
             <<"vowel">> => <<"pau">>}},
     #{<<"accent">> => 1,<<"is_interrogative">> => false,
       <<"moras">> =>
           [#{<<"consonant">> => <<"g">>,
              <<"text">> => <<227,130,180>>,
              <<"vowel">> => <<"o">>}],
       <<"pause_mora">> =>
           #{<<"consonant">> => null,
             <<"text">> => <<227,128,129>>,
             <<"vowel">> => <<"pau">>}},
     #{<<"accent">> => 1,<<"is_interrogative">> => false,
       <<"moras">> =>
           [#{<<"consonant">> => <<"k">>,
              <<"text">> => <<227,130,179>>,
              <<"vowel">> => <<"o">>}],
       <<"pause_mora">> =>
           #{<<"consonant">> => null,
             <<"text">> => <<227,128,129>>,
             <<"vowel">> => <<"pau">>}},
     #{<<"accent">> => 1,<<"is_interrogative">> => false,
       <<"moras">> =>
           [#{<<"consonant">> => <<"g">>,
              <<"text">> => <<227,130,178>>,
              <<"vowel">> => <<"e">>}],
       <<"pause_mora">> =>
           #{<<"consonant">> => null,
             <<"text">> => <<227,128,129>>,
             <<"vowel">> => <<"pau">>}},
     #{<<"accent">> => 1,<<"is_interrogative">> => false,
       <<"moras">> =>
           [#{<<"consonant">> => <<"k">>,
              <<"text">> => <<227,130,177>>,
              <<"vowel">> => <<"e">>}],
       <<"pause_mora">> =>
           #{<<"consonant">> => null,
             <<"text">> => <<227,128,129>>,
             <<"vowel">> => <<"pau">>}},
     #{<<"accent">> => 1,<<"is_interrogative">> => false,
       <<"moras">> =>
           [#{<<"consonant">> => <<"g">>,
              <<"text">> => <<227,130,176>>,
              <<"vowel">> => <<"u">>}],
       <<"pause_mora">> =>
           #{<<"consonant">> => null,
             <<"text">> => <<227,128,129>>,
             <<"vowel">> => <<"pau">>}},
     #{<<"accent">> => 1,<<"is_interrogative">> => false,
       <<"moras">> =>
           [#{<<"consonant">> => <<"k">>,
              <<"text">> => <<227,130,175>>,
              <<"vowel">> => <<"u">>}],
       <<"pause_mora">> =>
           #{<<"consonant">> => null,
             <<"text">> => <<227,128,129>>,
             <<"vowel">> => <<"pau">>}},
     #{<<"accent">> => 1,<<"is_interrogative">> => false,
       <<"moras">> =>
           [#{<<"consonant">> => <<"gy">>,
              <<"text">> => <<227,130,174,227,131,167>>,
              <<"vowel">> => <<"o">>}],
       <<"pause_mora">> =>
           #{<<"consonant">> => null,
             <<"text">> => <<227,128,129>>,
             <<"vowel">> => <<"pau">>}}, 
     #{<<"accent">> => 1,<<"is_interrogative">> => false,
       <<"moras">> =>
           [#{<<"consonant">> => <<"gy">>,
              <<"text">> => <<227,130,174,227,131,165>>,
              <<"vowel">> => <<"u">>}],
       <<"pause_mora">> =>
           #{<<"consonant">> => null,
             <<"text">> => <<227,128,129>>,
             <<"vowel">> => <<"pau">>}},
     #{<<"accent">> => 1,<<"is_interrogative">> => false,
       <<"moras">> =>
           [#{<<"consonant">> => <<"gy">>,
              <<"text">> => <<227,130,174,227,131,163>>,
              <<"vowel">> => <<"a">>}],
       <<"pause_mora">> =>
           #{<<"consonant">> => null,
             <<"text">> => <<227,128,129>>,
             <<"vowel">> => <<"pau">>}},
     #{<<"accent">> => 1,<<"is_interrogative">> => false,
       <<"moras">> =>
           [#{<<"consonant">> => <<"gy">>,
              <<"text">> => <<227,130,174,227,130,167>>,
              <<"vowel">> => <<"e">>}],
       <<"pause_mora">> =>
           #{<<"consonant">> => null,
             <<"text">> => <<227,128,129>>,
             <<"vowel">> => <<"pau">>}},
     #{<<"accent">> => 1,<<"is_interrogative">> => false,
       <<"moras">> =>
           [#{<<"consonant">> => <<"g">>,
              <<"text">> => <<227,130,174>>,
              <<"vowel">> => <<"i">>}],
       <<"pause_mora">> =>
           #{<<"consonant">> => null,
             <<"text">> => <<227,128,129>>,
             <<"vowel">> => <<"pau">>}},
     #{<<"accent">> => 1,<<"is_interrogative">> => false,
       <<"moras">> =>
           [#{<<"consonant">> => <<"ky">>,
              <<"text">> => <<227,130,173,227,131,167>>,
              <<"vowel">> => <<"o">>}], 
       <<"pause_mora">> =>
           #{<<"consonant">> => null,
             <<"text">> => <<227,128,129>>,
             <<"vowel">> => <<"pau">>}},
     #{<<"accent">> => 1,<<"is_interrogative">> => false,
       <<"moras">> =>
           [#{<<"consonant">> => <<"ky">>,
              <<"text">> => <<227,130,173,227,131,165>>,
              <<"vowel">> => <<"u">>}],
       <<"pause_mora">> =>
           #{<<"consonant">> => null,
             <<"text">> => <<227,128,129>>,
             <<"vowel">> => <<"pau">>}},
     #{<<"accent">> => 1,<<"is_interrogative">> => false,
       <<"moras">> =>
           [#{<<"consonant">> => <<"ky">>,
              <<"text">> => <<227,130,173,227,131,163>>,
              <<"vowel">> => <<"a">>}],
       <<"pause_mora">> =>
           #{<<"consonant">> => null,
             <<"text">> => <<227,128,129>>,
             <<"vowel">> => <<"pau">>}}, 
     #{<<"accent">> => 1,<<"is_interrogative">> => false,
       <<"moras">> =>
           [#{<<"consonant">> => <<"ky">>,
              <<"text">> => <<227,130,173,227,130,167>>,
              <<"vowel">> => <<"e">>}],
       <<"pause_mora">> =>
           #{<<"consonant">> => null,
             <<"text">> => <<227,128,129>>,
             <<"vowel">> => <<"pau">>}},
     #{<<"accent">> => 1,<<"is_interrogative">> => false,
       <<"moras">> =>
           [#{<<"consonant">> => <<"k">>,
              <<"text">> => <<227,130,173>>,
              <<"vowel">> => <<"i">>}],
       <<"pause_mora">> =>
           #{<<"consonant">> => null,
             <<"text">> => <<227,128,129>>,
             <<"vowel">> => <<"pau">>}},
     #{<<"accent">> => 1,<<"is_interrogative">> => false,
       <<"moras">> =>
           [#{<<"consonant">> => <<"g">>,
              <<"text">> => <<227,130,172>>,
              <<"vowel">> => <<"a">>}],
       <<"pause_mora">> =>
           #{<<"consonant">> => null,
             <<"text">> => <<227,128,129>>,
             <<"vowel">> => <<"pau">>}},
     #{<<"accent">> => 1,<<"is_interrogative">> => false,
       <<"moras">> =>
           [#{<<"consonant">> => <<"k">>,
              <<"text">> => <<227,130,171>>,
              <<"vowel">> => <<"a">>}],
       <<"pause_mora">> =>
           #{<<"consonant">> => null,
             <<"text">> => <<227,128,129>>,
             <<"vowel">> => <<"pau">>}},
     #{<<"accent">> => 1,<<"is_interrogative">> => false,
       <<"moras">> =>
           [#{<<"consonant">> => null,
              <<"text">> => <<227,130,170>>,
              <<"vowel">> => <<"o">>}],
       <<"pause_mora">> =>
           #{<<"consonant">> => null,
             <<"text">> => <<227,128,129>>,
             <<"vowel">> => <<"pau">>}},
     #{<<"accent">> => 1,<<"is_interrogative">> => false,
       <<"moras">> =>
           [#{<<"consonant">> => null,
              <<"text">> => <<227,130,170>>,
              <<"vowel">> => <<"o">>}],
       <<"pause_mora">> =>
           #{<<"consonant">> => null,
             <<"text">> => <<227,128,129>>,
             <<"vowel">> => <<"pau">>}},
     #{<<"accent">> => 1,<<"is_interrogative">> => false,
       <<"moras">> =>
           [#{<<"consonant">> => null,
              <<"text">> => <<227,130,168>>,
              <<"vowel">> => <<"e">>}],
       <<"pause_mora">> =>
           #{<<"consonant">> => null,
             <<"text">> => <<227,128,129>>,
             <<"vowel">> => <<"pau">>}},
     #{<<"accent">> => 1,<<"is_interrogative">> => false,
       <<"moras">> =>
           [#{<<"consonant">> => null,
              <<"text">> => <<227,130,168>>,
              <<"vowel">> => <<"e">>}],
       <<"pause_mora">> =>
           #{<<"consonant">> => null,
             <<"text">> => <<227,128,129>>,
             <<"vowel">> => <<"pau">>}},
     #{<<"accent">> => 1,<<"is_interrogative">> => false,
       <<"moras">> =>
           [#{<<"consonant">> => <<"w">>,
              <<"text">> => <<227,130,166,227,130,169>>,
              <<"vowel">> => <<"o">>}],
       <<"pause_mora">> =>
           #{<<"consonant">> => null,
             <<"text">> => <<227,128,129>>,
             <<"vowel">> => <<"pau">>}},
     #{<<"accent">> => 1,<<"is_interrogative">> => false,
       <<"moras">> =>
           [#{<<"consonant">> => <<"w">>,
              <<"text">> => <<227,130,166,227,130,167>>,
              <<"vowel">> => <<"e">>}],
       <<"pause_mora">> =>
           #{<<"consonant">> => null,
             <<"text">> => <<227,128,129>>,
             <<"vowel">> => <<"pau">>}},
     #{<<"accent">> => 1,<<"is_interrogative">> => false, 
       <<"moras">> =>
           [#{<<"consonant">> => <<"w">>,
              <<"text">> => <<227,130,166,227,130,163>>,
              <<"vowel">> => <<"i">>}],
       <<"pause_mora">> =>
           #{<<"consonant">> => null,
             <<"text">> => <<227,128,129>>,
             <<"vowel">> => <<"pau">>}},
     #{<<"accent">> => 1,<<"is_interrogative">> => false,
       <<"moras">> =>
           [#{<<"consonant">> => null,
              <<"text">> => <<227,130,166>>,
              <<"vowel">> => <<"u">>}],
       <<"pause_mora">> =>
           #{<<"consonant">> => null,
             <<"text">> => <<227,128,129>>,
             <<"vowel">> => <<"pau">>}},
     #{<<"accent">> => 1,<<"is_interrogative">> => false,
       <<"moras">> =>
           [#{<<"consonant">> => null,
              <<"text">> => <<227,130,166>>,
              <<"vowel">> => <<"u">>}],
       <<"pause_mora">> =>
           #{<<"consonant">> => null,
             <<"text">> => <<227,128,129>>,
             <<"vowel">> => <<"pau">>}},
     #{<<"accent">> => 1,<<"is_interrogative">> => false,
       <<"moras">> =>
           [#{<<"consonant">> => <<"y">>,
              <<"text">> => <<227,130,164,227,130,167>>,
              <<"vowel">> => <<"e">>}],
       <<"pause_mora">> =>
           #{<<"consonant">> => null,
             <<"text">> => <<227,128,129>>,
             <<"vowel">> => <<"pau">>}},
     #{<<"accent">> => 1,<<"is_interrogative">> => false,
       <<"moras">> =>
           [#{<<"consonant">> => null,
              <<"text">> => <<227,130,164>>,
              <<"vowel">> => <<"i">>}],
       <<"pause_mora">> =>
           #{<<"consonant">> => null,
             <<"text">> => <<227,128,129>>,
             <<"vowel">> => <<"pau">>}},
     #{<<"accent">> => 1,<<"is_interrogative">> => false,
       <<"moras">> =>
           [#{<<"consonant">> => null,
              <<"text">> => <<227,130,164>>,
              <<"vowel">> => <<"i">>}],
       <<"pause_mora">> =>
           #{<<"consonant">> => null,
             <<"text">> => <<227,128,129>>,
             <<"vowel">> => <<"pau">>}},
     #{<<"accent">> => 1,<<"is_interrogative">> => false,
       <<"moras">> =>
           [#{<<"consonant">> => null,
              <<"text">> => <<227,130,162>>,
              <<"vowel">> => <<"a">>}],
       <<"pause_mora">> =>
           #{<<"consonant">> => null,
             <<"text">> => <<227,128,129>>,
             <<"vowel">> => <<"pau">>}},
     #{<<"accent">> => 1,<<"is_interrogative">> => false,
       <<"moras">> =>
           [#{<<"consonant">> => null,
              <<"text">> => <<227,130,162>>,
              <<"vowel">> => <<"a">>}],
       <<"pause_mora">> =>
           #{<<"consonant">> => null,
             <<"text">> => <<227,128,129>>,
             <<"vowel">> => <<"pau">>}},
     #{<<"accent">> => 1,<<"is_interrogative">> => false,
       <<"moras">> =>
           [#{<<"consonant">> => <<"g">>,
              <<"text">> => <<227,130,176>>,
              <<"vowel">> => <<"u">>},
            #{<<"consonant">> => <<"w">>,
              <<"text">> => <<227,131,174>>,
              <<"vowel">> => <<"a">>}],
       <<"pause_mora">> =>
           #{<<"consonant">> => null,
             <<"text">> => <<227,128,129>>,
             <<"vowel">> => <<"pau">>}},

     #{<<"accent">> => 2,<<"is_interrogative">> => false,
       <<"moras">> =>
           [#{<<"consonant">> => <<"k">>,
              <<"text">> => <<227,130,175>>,
              <<"vowel">> => <<"u">>},
            #{<<"consonant">> => <<"w">>,
              <<"text">> => <<227,131,174>>,
              <<"vowel">> => <<"a">>}],
       <<"pause_mora">> =>
           #{<<"consonant">> => null,
             <<"text">> => <<227,128,129>>,
             <<"vowel">> => <<"pau">>}},
     #{<<"accent">> => 1,<<"is_interrogative">> => false,
       <<"moras">> => [#{<<"consonant">> => <<"w">>,
                          <<"text">> => <<227,131,174>>,
                          <<"vowel">> => <<"a">>}],
                   <<"pause_mora">> =>
                       #{<<"consonant">> => null,
                         <<"text">> => <<227,128,129>>,
                         <<"vowel">> => <<"pau">>}},
     #{<<"accent">> => 1,<<"is_interrogative">> => false,
       <<"moras">> => [#{<<"consonant">> => <<"k">>,
                          <<"text">> => <<227,131,182>>,
                          <<"vowel">> => <<"a">>}],
                   <<"pause_mora">> => null}].

