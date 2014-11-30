#!/bin/sh
exec erl \
    -pa ebin ../ebin ../deps/*/ebin \
    -boot start_sasl \
    -sname demo_dev \
    -s demo
