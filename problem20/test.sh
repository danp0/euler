#!/usr/bin/env bash

erl -pa ebin -run factorial test -run init stop -noshell
