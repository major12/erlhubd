#!/bin/sh
cd `dirname $0`
exec erl -pa $PWD/ebin -boot start_sasl -s erlhubd "$@"
