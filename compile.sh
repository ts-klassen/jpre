#!/bin/bash
set -e

JPRE_COMPILE_PWD=`pwd`
cd `dirname $0`

if [ ! -d ./priv/naist-jdic ]; then
    mkdir ./priv;
    cd priv;
    wget https://github.com/jpreprocess/jpreprocess/releases/download/v0.9.1/naist-jdic-jpreprocess.tar.gz;
    tar -xf naist-jdic-jpreprocess.tar.gz;
    rm naist-jdic-jpreprocess.tar.gz;
    cd ..;
fi

rm -f priv/libjpre.so;
cargo build --manifest-path=crates/jpre/Cargo.toml --release;
mv crates/jpre/target/release/libjpre.so priv/.;


cd $JPRE_COMPILE_PWD
