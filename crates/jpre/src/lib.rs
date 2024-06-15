extern crate rustler;
use rustler::{NifResult, NifMap, NifUntaggedEnum, Atom};
use jpreprocess_core::pronunciation;

rustler::init!("jpre", [
        accent_phrases,
    ]);

mod atoms {
    rustler::atoms! {
        null,
    }
}

#[derive(Debug, NifMap, Clone)]
struct Opt {
    dict: String
}

#[derive(Debug, NifMap, Clone)]
struct AccentPhrase {
    moras: Vec<Mora>,
    accent: usize,
}

impl AccentPhrase {
    fn from_njd(njd: &jpreprocess::NJD) -> Vec<Self> {
        let mut aps: Vec<Self> = Vec::new();
        for node in &njd.nodes {
            let p_mora = match aps.last() {
                Some(ap) => {
                    ap.moras.last()
                },
                None => {
                    None
                },
            };
            aps.push(Self::from_node(&node, p_mora));
        }
        aps
    }
    fn from_node(
        node: &jpreprocess_njd::NJDNode,
        p_mora: Option<&Mora>,
    ) -> Self {
        let pron = node.get_pron();
        Self {
            moras: Mora::from_pron(pron, p_mora),
            accent: pron.accent,
        }
    }
}

#[derive(Debug, NifMap, Clone)]
struct Mora {
    text: String,
    consonant: Nullable<String>,
    consonant_length: Nullable<f32>,
    vowel: String,
    vowel_length: f32,
    pitch: f32,
}

impl Mora {
    fn from_pron(
        pron: &pronunciation::Pronunciation,
        p_moras: Option<&Self>,
    ) -> Vec<Self> {
        let mut moras: Vec<Self> = Vec::new();
        for mora in pron.moras.iter() {
            let next_mora = match Self::from_mora(mora) {
                next_mora if next_mora.vowel == "Long".to_string() => {
                    match (moras.last(), p_moras) {
                        (Some(m), _) => {
                            let mut m = m.clone();
                            m.consonant = Nullable::null();
                            m.consonant_length = Nullable::null();
                            m
                        },
                        (_, Some(m)) => {
                            let mut m = m.clone();
                            m.consonant = Nullable::null();
                            m.consonant_length = Nullable::null();
                            m
                        },
                        (None, None) => {
                            // accent_phrase starting with mora "ー"
                            // should not exist.
                            // But, jpreprocess sometimes does that.
                            next_mora
                        },
                    }
                },
                other => {
                    other
                },
            };
            moras.push(next_mora);
        }
        moras
    }
    fn from_mora(mora: &pronunciation::mora::Mora) -> Self {
    use pronunciation::MoraEnum::*;
        let (text, vowel, cons) = match mora.mora_enum {
            Vyo => {("ヴョ", "o", Some("vy"))},
            Vyu => {("ヴュ", "u", Some("vy"))},
            Vya => {("ヴャ", "a", Some("vy"))},
            Vo => {("ヴォ", "o", Some("v"))},
            Ve => {("ヴェ", "e", Some("v"))},
            Vi => {("ヴィ", "i", Some("v"))},
            Va => {("ヴァ", "a", Some("v"))},
            Vu => {("ヴ", "u", Some("v"))},
            N => {("ン", "N", None)},
            Wo => {("ヲ", "o", Some("w"))},
            We => {("ヱ", "e", Some("w"))},
            Wi => {("ヰ", "i", Some("w"))},
            Wa => {("ワ", "a", Some("w"))},
            Ro => {("ロ", "o", Some("r"))},
            Re => {("レ", "e", Some("r"))},
            Ru => {("ル", "u", Some("r"))},
            Ryo => {("リョ", "o", Some("ry"))},
            Ryu => {("リュ", "u", Some("ry"))},
            Rya => {("リャ", "a", Some("ry"))},
            Rye => {("リェ", "e", Some("ry"))},
            Ri => {("リ", "i", Some("r"))},
            Ra => {("ラ", "a", Some("r"))},
            Yo => {("ヨ", "o", Some("y"))},
            Xyo => {("ョ", "o", Some("xy"))},
            Yu => {("ユ", "u", Some("y"))},
            Xyu => {("ュ", "u", Some("xy"))},
            Ya => {("ヤ", "a", Some("y"))},
            Xya => {("ャ", "a", Some("xy"))},
            Mo => {("モ", "o", Some("m"))},
            Me => {("メ", "e", Some("m"))},
            Mu => {("ム", "u", Some("m"))},
            Myo => {("ミョ", "o", Some("my"))},
            Myu => {("ミュ", "u", Some("my"))},
            Mya => {("ミャ", "a", Some("my"))},
            Mye => {("ミェ", "e", Some("my"))},
            Mi => {("ミ", "i", Some("m"))},
            Ma => {("マ", "a", Some("m"))},
            Po => {("ポ", "o", Some("p"))},
            Bo => {("ボ", "o", Some("b"))},
            Ho => {("ホ", "o", Some("h"))},
            Pe => {("ペ", "e", Some("p"))},
            Be => {("ベ", "e", Some("b"))},
            He => {("ヘ", "e", Some("h"))},
            Pu => {("プ", "u", Some("p"))},
            Bu => {("ブ", "u", Some("b"))},
            Fo => {("フォ", "o", Some("f"))},
            Fe => {("フェ", "e", Some("f"))},
            Fi => {("フィ", "i", Some("f"))},
            Fa => {("ファ", "a", Some("f"))},
            Fu => {("フ", "u", Some("f"))},
            Pyo => {("ピョ", "o", Some("py"))},
            Pyu => {("ピュ", "u", Some("py"))},
            Pya => {("ピャ", "a", Some("py"))},
            Pye => {("ピェ", "e", Some("py"))},
            Pi => {("ピ", "i", Some("p"))},
            Byo => {("ビョ", "o", Some("by"))},
            Byu => {("ビュ", "u", Some("by"))},
            Bya => {("ビャ", "a", Some("by"))},
            Bye => {("ビェ", "e", Some("by"))},
            Bi => {("ビ", "i", Some("b"))},
            Hyo => {("ヒョ", "o", Some("hy"))},
            Hyu => {("ヒュ", "u", Some("hy"))},
            Hya => {("ヒャ", "a", Some("hy"))},
            Hye => {("ヒェ", "e", Some("hy"))},
            Hi => {("ヒ", "i", Some("h"))},
            Pa => {("パ", "a", Some("p"))},
            Ba => {("バ", "a", Some("b"))},
            Ha => {("ハ", "a", Some("h"))},
            No => {("ノ", "o", Some("n"))},
            Ne => {("ネ", "e", Some("n"))},
            Nu => {("ヌ", "u", Some("n"))},
            Nyo => {("ニョ", "o", Some("ny"))},
            Nyu => {("ニュ", "u", Some("ny"))},
            Nya => {("ニャ", "a", Some("ny"))},
            Nye => {("ニェ", "e", Some("ny"))},
            Ni => {("ニ", "i", Some("n"))},
            Na => {("ナ", "a", Some("n"))},
            Dwu => {("ドゥ", "u", Some("dw"))},
            Do => {("ド", "o", Some("d"))},
            Twu => {("トゥ", "u", Some("tw"))},
            To => {("ト", "", Some("t"))},
            Dho => {("デョ", "o", Some("dh"))},
            Dhu => {("デュ", "u", Some("dh"))},
            Dha => {("デャ", "a", Some("dh"))},
            Dhi => {("ディ", "i", Some("dh"))},
            De => {("デ", "e", Some("d"))},
            Tho => {("テョ", "o", Some("th"))},
            Thu => {("テュ", "u", Some("th"))},
            Tha => {("テャ", "a", Some("th"))},
            Thi => {("ティ", "i", Some("th"))},
            Te => {("テ", "e", Some("t"))},
            Du => {("ヅ", "u", Some("d"))},
            Tso => {("ツォ", "o", Some("ts"))},
            Tse => {("ツェ", "e", Some("ts"))},
            Tsi => {("ツィ", "i", Some("ts"))},
            Tsa => {("ツァ", "a", Some("ts"))},
            Tsu => {("ツ", "u", Some("ts"))},
            Xtsu => {("ッ", "u", Some("xts"))},
            Di => {("ヂ", "i", Some("d"))},
            Cho => {("チョ", "o", Some("ch"))},
            Chu => {("チュ", "u", Some("ch"))},
            Cha => {("チャ", "a", Some("ch"))},
            Che => {("チェ", "e", Some("ch"))},
            Chi => {("チ", "i", Some("ch"))},
            Da => {("ダ", "a", Some("d"))},
            Ta => {("タ", "a", Some("t"))},
            Zo => {("ゾ", "o", Some("z"))},
            So => {("ソ", "o", Some("s"))},
            Ze => {("ゼ", "e", Some("z"))},
            Se => {("セ", "e", Some("s"))},
            Zwi => {("ズィ", "i", Some("zw"))},
            Zu => {("ズ", "u", Some("z"))},
            Swi => {("スィ", "i", Some("sw"))},
            Su => {("ス", "u", Some("s"))},
            Jo => {("ジョ", "o", Some("j"))},
            Ju => {("ジュ", "u", Some("j"))},
            Ja => {("ジャ", "a", Some("j"))},
            Je => {("ジェ", "e", Some("j"))},
            Ji => {("ジ", "i", Some("j"))},
            Sho => {("ショ", "o", Some("sh"))},
            Shu => {("シュ", "u", Some("sh"))},
            Sha => {("シャ", "a", Some("sh"))},
            She => {("シェ", "e", Some("sh"))},
            Shi => {("シ", "i", Some("sh"))},
            Za => {("ザ", "a", Some("z"))},
            Sa => {("サ", "a", Some("s"))},
            Go => {("ゴ", "o", Some("g"))},
            Ko => {("コ", "o", Some("k"))},
            Ge => {("ゲ", "e", Some("g"))},
            Ke => {("ケ", "e", Some("k"))},
            Gu => {("グ", "u", Some("g"))},
            Ku => {("ク", "u", Some("k"))},
            Gyo => {("ギョ", "o", Some("gy"))},
            Gyu => {("ギュ", "u", Some("gy"))},
            Gya => {("ギャ", "a", Some("gy"))},
            Gye => {("ギェ", "e", Some("gy"))},
            Gi => {("ギ", "i", Some("g"))},
            Kyo => {("キョ", "o", Some("ky"))},
            Kyu => {("キュ", "u", Some("ky"))},
            Kya => {("キャ", "a", Some("ky"))},
            Kye => {("キェ", "e", Some("ky"))},
            Ki => {("キ", "i", Some("k"))},
            Ga => {("ガ", "a", Some("g"))},
            Ka => {("カ", "a", Some("k"))},
            O => {("オ", "o", None)},
            Xo => {("ォ", "o", Some("x"))},
            E => {("エ", "e", None)},
            Xe => {("ェ", "e", Some("x"))},
            Who => {("ウォ", "o", Some("wh"))},
            Whe => {("ウェ", "e", Some("wh"))},
            Whi => {("ウィ", "i", Some("wh"))},
            U => {("ウ", "u", None)},
            Xu => {("ゥ", "u", Some("x"))},
            Ye => {("イェ", "e", Some("y"))},
            I => {("イ", "i", None)},
            Xi => {("ィ", "i", Some("x"))},
            A => {("ア", "a", None)},
            Xa => {("ァ", "a", Some("x"))},
            Long => {("ー", "Long", None)},
            Gwa => {("グヮ", "a", Some("gw"))},
            Kwa => {("クヮ", "a", Some("kw"))},
            Xwa => {("ヮ", "a", Some("xw"))},
            Xke => {("ヶ", "e", Some("xk"))},
            Touten => unreachable!(),
            Question => unreachable!(),
        };
        let (consonant, consonant_length) = match cons {
            None => {
                (Nullable::null(), Nullable::null())
            },
            Some(costr) => {
                (Nullable::value(costr.to_string()), Nullable::value(0.0))
            },
        };
        Self {
            text: text.to_string(),
            consonant: consonant,
            consonant_length: consonant_length,
            vowel: vowel.to_string(),
            vowel_length: 0.0,
            pitch: 0.0,
        }
    }
}

#[derive(Debug, NifUntaggedEnum, Clone)]
enum Nullable<T> {
   Null(Atom),
   Value(T),
}
impl<T> Nullable<T> {
    fn null() -> Self {
        Self::Null(atoms::null())
    }
    fn value(val: T) -> Self {
        Self::Value(val)
    }
}


#[rustler::nif]
fn accent_phrases(text: String, opt: Opt) -> NifResult<Vec<AccentPhrase>> {
    use jpreprocess::*;
    let config = JPreprocessConfig {
        dictionary: SystemDictionaryConfig::File(opt.dict.into()),
        user_dictionary: None,
    };
    let jpreprocess = JPreprocess::from_config(config).unwrap();

    let mut njd = jpreprocess.text_to_njd(&text).unwrap();
    njd.preprocess(); // not sure if I should do this or not...

    Ok(AccentPhrase::from_njd(&njd))
}

//# MEMO: using user_dictionary
//# let user = UserDictionaryConfig {
//#     path: "ipadic.csv".into(),
//#     kind: Some(DictionaryKind::IPADIC),
//# };
//# let config = JPreprocessConfig {
//#     dictionary: SystemDictionaryConfig::File(opt.dict.into()),
//#     user_dictionary: Some(user),
//# };

//# FILE SAMPLE: ipdadic.csv
//# テスト,1343,1343,3195,名詞,サ変接続,*,*,*,*,テスト,テスト,テスト,1/3,C1,-1
//# テストケース,1345,1345,3452,名詞,一般,*,*,*,*,テストケース,テストケース,テストケース,4/6,C1,-1

