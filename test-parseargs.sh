#!/bin/sh
# Copyright © 2016 Bart Massey
# Run simple parseargs tests
PA=parseargs-example
TMP=/tmp/test-parseargs-$$
trap "rm -f $TMP" 0 1 2 3 15
for f in tests/*.in
do
    T="`basename $f .in`"
    $PA `cat $f` >$TMP
    if ! cmp $TMP tests/$T.out
    then
        echo "test $T failed"
        echo "expected:"
        cat tests/$T.out
        echo "got:"
        cat $TMP
        exit 1
    fi
done
exit 0
