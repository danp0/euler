#!/usr/bin/env bash

if [ $# == 1 ]
then
  erl -pa ebin -s fibonacci $1 -s init stop -noshell
else
  echo "$(basename $0) <function>"
fi


