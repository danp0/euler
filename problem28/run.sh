#!/usr/bin/env bash

if [ $# == 1 ]
then
  erl -pa ebin -s spiral $1 -s init stop -noshell
else
  echo "usage: $(basename $0) <function>"
fi
