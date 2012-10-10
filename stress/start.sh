#!/bin/sh
erl -sname stress_test -pa ebin -pa ebin deps/*/ebin -eval "stress_test:start()."
