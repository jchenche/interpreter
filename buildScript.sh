#!/bin/bash

stack build
cp $(stack exec -- which interpreter-exe) jcc
alias jcc=./jcc
