#!/bin/bash

case "$1" in
    debug)
        exec cerl -rgdb \
            -pa ebin edit deps/*/ebin \
            -sname gusion_dev \
            -config sys.config
        ;;
    *)
        exec erl +K true -smp +P 102400 \
            -pa ebin edit deps/*/ebin \
            -boot start_sasl \
            -env ERL_MAX_ETS_TABLES 10000 -env ERL_MAX_PORTS 100000 \
            -sname gusion_dev \
            -config sys.config
esac
