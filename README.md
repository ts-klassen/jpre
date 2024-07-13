jpre
=====

An OTP library

Build
-----

    $ rebar3 compile

Usage
-----

```
1> jpre:accent_phrases(<<"Hello">>).
[#{accent => 1,is_interrogative => false,
   moras =>
       [#{consonant => <<"h">>,consonant_length => 0.0,pitch => 0.0,
          text => <<227,131,143>>,
          vowel => <<"a">>,vowel_length => 0.0},
        #{consonant => <<"r">>,consonant_length => 0.0,pitch => 0.0,
          text => <<227,131,173>>,
          vowel => <<"o">>,vowel_length => 0.0},
        #{consonant => null,consonant_length => null,pitch => 0.0,
          text => <<227,131,188>>,
          vowel => <<"o">>,vowel_length => 0.0}],
   pause_mora => null}]
2>
```
