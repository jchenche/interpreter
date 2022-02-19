#!/bin/bash

stack build
cp $(stack exec -- which interpreter-exe) gavagai
alias gavagai=./gavagai
