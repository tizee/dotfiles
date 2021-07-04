#!/usr/bin/env zsh
alias pgpshort="gpg -k --keyid-format 0xshort"
alias pgplong="gpg -k --keyid-format 0xlong"
alias pgpfp="gpg --fingerprint" # <key_id>, <user_id>
alias pgpexport="gpg --armor --export" # fingerprint of public key or private key, or use <user_id>
alias pgpexportmin="gpg --armor --export-options export-minimal --export" # fingerprint of public key or private key
alias pgprestart="gpgconf --kill all" # restart all daemons
