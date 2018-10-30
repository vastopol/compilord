#!/bin/bash

# USAGE: ./test "folder" "exe"
# phase1 lexer
# phase2 parser
# phase3 compiler

# rebuild
cd $1
make clean
make
cd ..

# run tests
for i in $(ls tests | grep \.min)
do
    echo $i
    ./$1/$2 tests/$i > $i".txt"
done

# clean up
cd $1
make clean
cd ..
exit 0