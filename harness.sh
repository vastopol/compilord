#!/bin/bash

# USAGE: ./harness.sh name
# phase1 == lexer
# phase2 == parser
# phase3 == compiler

if   [ $1 == "lexer" ] ; then
    cd phase1
elif [ $1 == "parser" ] ; then
    cd phase2
elif [ $1 == "compiler" ] ; then
    cd phase3
else
    echo "Error: project name to test"
    exit 1
fi

# rebuild
make clean
make

FILES="../tests"

echo "Testing files:"

# run tests
for i in $(ls $FILES | grep \.min)
do
    echo $i
    ./$1 $FILES/$i > $i."txt"
done

cd ..
exit 0