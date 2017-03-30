#!/usr/bin/env bash

erl -pa ebin -run summer main -run init stop -noshell -smp auto
