extern crate rustler;
use rustler::{NifResult, NifMap, NifUntaggedEnum, Atom};

rustler::init!("jpre", [
        moras,
    ]);

mod atoms {
    rustler::atoms! {
        null,
    }
}

#[derive(NifMap)]
struct Opt {
    dict: String
}

#[derive(NifMap)]
struct Mora {
    text: String,
    consonant: Nullable<String>,
    consonant_length: Nullable<f32>,
    vowel: String,
    vowel_length: f32,
    pitch: f32,
}

#[derive(NifUntaggedEnum)]
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
fn moras(text: String, opt: Opt) -> NifResult<Vec<Mora>> {
    use jpreprocess::*;
    let config = JPreprocessConfig {
        dictionary: SystemDictionaryConfig::File(opt.dict.into()),
        user_dictionary: None,
    };
    let jpreprocess = match JPreprocess::from_config(config) {
        Ok(jpreprocess) => {
            jpreprocess
        },
        Err(_) => {
            return Err(rustler::error::Error::BadArg);
        },
    };
    let mut njd = jpreprocess.text_to_njd(&text).unwrap();
    njd.preprocess();
    println!("{:?}", njd.nodes[0].get_pron().moras[0].mora_enum);
    println!("{:?}", njd.nodes[0].get_pron().accent);
    Ok(vec![
        Mora{
            text: "あ".to_string(),
            consonant: Nullable::null(),
            consonant_length: Nullable::null(),
            vowel: "a".to_string(),
            vowel_length: 0.0,
            pitch: 0.0,
        },
        Mora{
            text: "か".to_string(),
            consonant: Nullable::value("k".to_string()),
            consonant_length: Nullable::value(0.0),
            vowel: "a".to_string(),
            vowel_length: 0.0,
            pitch: 0.0,
        },
    ])
}

