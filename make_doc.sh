#!/bin/bash
# docs for doc
# https://doc.rust-lang.org/cargo/commands/cargo-doc.html

# make doc and allow private functions
cargo doc --open --document-private-items
