#!/usr/bin/env bash

erl -pa ./ebin -run primes main -run prime_sum main -run init stop -noshell
