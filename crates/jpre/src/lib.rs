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

#[derive(Debug, NifMap)]
struct Opt {
    dict: String
}

#[derive(Debug, NifMap)]
struct AccentPhrase {
    moras: Vec<Mora>,
    accent: usize,
}

impl AccentPhrase {
    fn from_njd(njd: &jpreprocess::NJD) -> Vec<Self> {
        let mut aps: Vec<Self> = Vec::new();
        for node in &njd.nodes {
            aps.push(Self::from_node(&node));
        }
        aps
    }
    fn from_node(node: &jpreprocess_njd::NJDNode) -> Self {
        let pron = node.get_pron();
        Self {
            moras: Mora::from_pron(pron),
            accent: pron.accent,
        }
    }
}

#[derive(Debug, NifMap)]
struct Mora {
    text: String,
    consonant: Nullable<String>,
    consonant_length: Nullable<f32>,
    vowel: String,
    vowel_length: f32,
    pitch: f32,
}

impl Mora {
    fn from_pron(pron: &pronunciation::Pronunciation) -> Vec<Self> {
        let mut moras: Vec<Self> = Vec::new();
        for mora in pron.moras.iter() {
            moras.push(Self::from_mora(mora));
        }
        moras
        // vec![Self::from_mora(&pron.moras[0])]
    }
    fn from_mora(mora: &pronunciation::mora::Mora) -> Self {
    use pronunciation::MoraEnum::*;
        let (text, vowel, cons) = match mora.mora_enum {
            A => {
                ("ア", "a", None)
            },
            Ta => {
                ("タ", "a", Some("t"))
            },
            Ma => {
                ("マ", "a", Some("m"))
            },
            _ => {
                todo!()
            }
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

#[derive(Debug, NifUntaggedEnum)]
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

    let njd = jpreprocess.text_to_njd(&text).unwrap();
    //njd.preprocess(); // not sure if I should do this or not...

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

