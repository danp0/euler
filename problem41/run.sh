#!/usr/bin/env bash

if [ $# == 1 ]
then
  erl -pa ebin -s pandigital $1 -s init stop -noshell
else
  echo "usage: $(basename $0) <function>"
fi
