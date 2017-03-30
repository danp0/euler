#!/usr/bin/env bash

erl -pa ./ebin -run primes test -run init stop -noshell
