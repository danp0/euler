# Project Euler Solutions

My [https://projecteuler.net](Project Euler) Solutions using erlang.

# Building
You must install erlang to build the solutions. Some solutions use 
[https://www.erlang.org](erlang) 
makefiles (Emakefile). The remaining use 
[https://github.com/rebar/rebar](rebar).

## Emakefile
To build with an Emakefile, enter the command `erl -make` and then `./run.sh`.
You can run the unit tests with `./test.sh`.

## rebar
To build with rebar, enter `make` and then `make run`.
You can run unit tests with `make test`.
