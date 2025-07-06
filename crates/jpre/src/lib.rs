extern crate rustler;
use rustler::{NifResult, NifMap, NifUntaggedEnum, Atom};
use jpreprocess_core::pronunciation;

rustler::init!("jpre", [
        normal_detail,
        dirty_cpu_detail,
        dirty_io_detail,
        normalize,
        validate_user_dict,
    ]);

mod atoms {
    rustler::atoms! {
        null,
    }
}

#[derive(Debug, NifMap, Clone)]
struct Opt {
    dict: String,
    find_missing_words: bool,
    user_dict: Vec<Vec<String>>,
}

#[derive(Debug, NifMap, Clone)]
struct AccentPhrase {
    moras: Vec<Mora>,
    accent: usize,
    pause_mora: Nullable<Mora>,
    is_interrogative: bool,
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
            let chain_flag = node.get_chain_flag();
            let accent_phrase = Self::from_node(&node, p_mora);
            match chain_flag {
                Some(true) => {
                    let last = aps.last_mut().unwrap();
                    match last.pause_mora {
                        Nullable::Null(_) => {
                            last.append(&accent_phrase);
                        },
                        _ => {
                            aps.push(accent_phrase);
                        }
                    };
                },
                _ => {
                    aps.push(accent_phrase);
                }
            };
        }
        for i in (1..aps.len()).rev() {
            match aps[i].moras.len() {
                0 => {
                    aps[i-1].pause_mora = aps[i].pause_mora.clone();
                    aps[i-1].is_interrogative = aps[i].is_interrogative.clone();
                },
                _ => {
                    ()
                }
            }
        }
        aps = aps.into_iter().filter(|ap| ap.moras.len() > 0).collect();
        match aps.last_mut() {
            Some(ap) => {
                ap.pause_mora = Nullable::null();
            },
            None => {
                ()
            },
        };
        aps.into_iter().map(|ap| {
            let mut ap = ap;
            if ap.accent <= 0 || ap.moras.len() < ap.accent {
                ap.accent = ap.moras.len();
            }
            ap
        }).collect()
    }
    fn from_node(
        node: &jpreprocess_njd::NJDNode,
        p_mora: Option<&Mora>,
    ) -> Self {
        let pron = node.get_pron();
        let mut pau_count: usize = 0;
        let mut is_interrogative = false;
        let moras = Mora::from_pron(pron, p_mora).into_iter().filter(|mora|
            match mora {
                mora if mora.vowel == "pau".to_string() => {
                    pau_count += 1;
                    false
                },
                mora if mora.vowel == "Question".to_string() => {
                    pau_count += 1;
                    is_interrogative = true;
                    false
                },
                _ => {
                    true
                },
            }
        ).collect();
        let pause_mora = match pau_count {
            0 => {
                Nullable::null()
            },
            _ => {
                Nullable::value(Mora{
                    text: "、".to_string(),
                    consonant: Nullable::null(),
                    consonant_length: Nullable::null(),
                    vowel: "pau".to_string(),
                    vowel_length: pau_count as f32,
                    pitch: 0.0,
                })
            },
        };
        Self {
            moras: moras,
            accent: pron.accent,
            pause_mora: pause_mora,
            is_interrogative: is_interrogative,
        }
    }
    fn append(&mut self, sub: &Self) {
        let mut sub = sub.clone();
        self.moras.append(&mut sub.moras);
        self.pause_mora = sub.pause_mora;
        self.is_interrogative = sub.is_interrogative;
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
                            m.text = "ー".to_string();
                            m
                        },
                        (_, Some(m)) => {
                            let mut m = m.clone();
                            m.consonant = Nullable::null();
                            m.consonant_length = Nullable::null();
                            m.text = "ー".to_string();
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
            Vyo => {("ビョ", "o", Some("by"))},
            Vyu => {("ビュ", "u", Some("by"))},
            Vya => {("ビャ", "a", Some("by"))},
            Vo => {("ヴォ", "o", Some("v"))},
            Ve => {("ヴェ", "e", Some("v"))},
            Vi => {("ヴィ", "i", Some("v"))},
            Va => {("ヴァ", "a", Some("v"))},
            Vu => {("ヴ", "u", Some("v"))},
            N => {("ン", "N", None)},
            Wo => {("ヲ", "o", Some("w"))},
            We => {("エ", "e", None)},
            Wi => {("イ", "i", None)},
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
            Xyo => {("ヨ", "o", Some("y"))},
            Yu => {("ユ", "u", Some("y"))},
            Xyu => {("ユ", "u", Some("y"))},
            Ya => {("ヤ", "a", Some("y"))},
            Xya => {("ヤ", "a", Some("y"))},
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
            Dwu => {("ドゥ", "u", Some("d"))},
            Do => {("ド", "o", Some("d"))},
            Twu => {("ト", "u", Some("t"))},
            To => {("ト", "o", Some("t"))},
            Dho => {("デョ", "o", Some("dy"))},
            Dhu => {("デュ", "u", Some("dy"))},
            Dha => {("デャ", "a", Some("dy"))},
            Dhi => {("ディ", "i", Some("d"))},
            De => {("デ", "e", Some("d"))},
            Tho => {("テョ", "o", Some("ty"))},
            Thu => {("テュ", "u", Some("ty"))},
            Tha => {("テャ", "a", Some("ty"))},
            Thi => {("ティ", "i", Some("t"))},
            Te => {("テ", "e", Some("t"))},
            Du => {("ズ", "u", Some("z"))},
            Tso => {("ツォ", "o", Some("ts"))},
            Tse => {("ツェ", "e", Some("ts"))},
            Tsi => {("ツィ", "i", Some("ts"))},
            Tsa => {("ツァ", "a", Some("ts"))},
            Tsu => {("ツ", "u", Some("ts"))},
            Xtsu => {("ッ", "cl", None)},
            Di => {("ジ", "i", Some("j"))},
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
            Zwi => {("ズィ", "i", Some("z"))},
            Zu => {("ズ", "u", Some("z"))},
            Swi => {("スィ", "i", Some("s"))},
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
            Xo => {("オ", "o", None)},
            E => {("エ", "e", None)},
            Xe => {("エ", "e", None)},
            Who => {("ウォ", "o", Some("w"))},
            Whe => {("ウェ", "e", Some("w"))},
            Whi => {("ウィ", "i", Some("w"))},
            U => {("ウ", "u", None)},
            Xu => {("ウ", "u", None)},
            Ye => {("イェ", "e", Some("y"))},
            I => {("イ", "i", None)},
            Xi => {("イ", "i", None)},
            A => {("ア", "a", None)},
            Xa => {("ア", "a", None)},
            Long => {("ー", "Long", None)},
            Gwa => {("ガ", "a", Some("g"))},
            Kwa => {("カ", "a", Some("k"))},
            Xwa => {("ヮ", "a", Some("w"))},
            Xke => {("ヶ", "a", Some("k"))},
            Touten => {("、", "pau", None)},
            Question => {("？", "Question", None)},
        };
        let (consonant, consonant_length) = match cons {
            None => {
                (Nullable::null(), Nullable::null())
            },
            Some(costr) => {
                (Nullable::value(costr.to_string()), Nullable::value(0.0))
            },
        };
        let vowel = match mora.is_voiced {
            true => {
                vowel.to_string()
            },
            false => {
                vowel.to_uppercase()
            },
        };
        Self {
            text: text.to_string(),
            consonant: consonant,
            consonant_length: consonant_length,
            vowel: vowel,
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

#[derive(Debug, NifMap, Clone)]
struct Detail {
    accent_phrases: Vec<AccentPhrase>,
    missing_words: Vec<String>,
}


#[rustler::nif]
fn normalize(text: String) -> String {
    jpreprocess::normalize_text_for_naist_jdic(text.as_str())
}


// Validate rows for user dictionary. Returns `true` when all rows are valid,
// `false` otherwise. No message is printed even if some rows are invalid.
#[rustler::nif]
fn validate_user_dict(rows: Vec<Vec<String>>) -> bool {
    use jpreprocess_core::word_entry::WordEntry;

    // Normalize surface strings (same as in get_user_dictionary)
    let mut rows = rows;

    for row in &mut rows {
        if let Some(surface) = row.first_mut() {
            *surface = jpreprocess::normalize_text_for_naist_jdic(surface.as_str());
        }
    }

    rows.into_iter().all(|row| {
        match row.len() {
            3 => {
                // Simple user-dict row: [surface, POS, reading]
                let mut details: Vec<&str> = Vec::with_capacity(13);
                details.push(row[1].as_str()); // POS
                for _ in 0..5 {
                    details.push("*");
                }
                details.push(row[0].as_str()); // Base form (orig)
                details.push(row[2].as_str()); // Reading
                details.push("*");             // Pronunciation
                details.resize(13, "");
                WordEntry::load(&details).is_ok()
            }
            l if l >= 13 => {
                // Detailed row: columns 4.. contain the details section.
                let mut details: Vec<&str> = row.iter().skip(4).map(|s| s.as_str()).collect();
                details.resize(13, "");
                WordEntry::load(&details).is_ok()
            }
            _ => false, // any other length is invalid
        }
    })
}


#[rustler::nif]
fn normal_detail(text: String, opt: Opt) -> NifResult<Detail> {
    detail(text, opt)
}


#[rustler::nif(schedule = "DirtyCpu")]
fn dirty_cpu_detail(text: String, opt: Opt) -> NifResult<Detail> {
    detail(text, opt)
}


#[rustler::nif(schedule = "DirtyIo")]
fn dirty_io_detail(text: String, opt: Opt) -> NifResult<Detail> {
    detail(text, opt)
}


fn detail(text: String, opt: Opt) -> NifResult<Detail> {
    use jpreprocess::*;
    let user_dictionary = get_user_dictionary(opt.clone());
    let dictionary = SystemDictionaryConfig::File(opt.dict.into())
        .load().unwrap();
    let jpreprocess = JPreprocess::with_dictionaries(
        dictionary, user_dictionary);

    let mut njd = jpreprocess.text_to_njd(&text).unwrap();
    let missing_words = if opt.find_missing_words {
        let mut words = vec![];
        for node in &njd.nodes {
            match node.get_read() {
                None => {
                    words.push(node.get_string().to_string());
                },
                _ => {
                    ()
                }
            }
        }
        words
    }
    else {
        vec![]
    };
    njd.preprocess();
    jpreprocess_njd::accent_phrase::njd_set_accent_phrase(&mut njd);

    Ok(Detail{
        accent_phrases: AccentPhrase::from_njd(&njd),
        missing_words: missing_words,
    })
}

fn get_user_dictionary(opt: Opt) ->
        Option<jpreprocess::UserDictionary>
    {
    use jpreprocess_dictionary::serializer::jpreprocess::JPreprocessSerializer;
    use jpreprocess_dictionary_builder::ipadic_builder::IpadicBuilder;
    if opt.user_dict.len() == 0 {
        None
    }
    else {

        let mut rows = opt.user_dict;
        for row in &mut rows {
            row[0] = jpreprocess::normalize_text_for_naist_jdic(row[0].as_str());
        }
        rows.sort_by_key(|row| row[0].clone());
        let rows = rows.iter().map(|row| row.iter().map(|s| s as &str).collect()).collect();

        let dict = IpadicBuilder::new(Box::new(JPreprocessSerializer))
            .build_user_dict_from_data(&rows);
        match dict {
            Ok(user_dict) => {
                Some(user_dict)
            },
            _ => {
                None
            }
        }
    }
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

