#!/bin/bash

# USAGE: ./harness.sh
# phase3 == compiler

echo
echo "Making compiler:"
echo

cd phase3
make
cd ..

#----------------------------------------

echo
echo "Output files:"
echo

FILES="tests"
COMP="phase3/compiler"
for i in $(ls $FILES | grep \.min)
do
    echo $i".txt"
    $COMP $FILES/$i > $i."txt"
done

#----------------------------------------

echo
echo "Test files:"
echo

MILR="tests/mil_run"
for i in $(ls | grep \.min.txt)
do
    echo $i
    $MILR $i <<< 10 # heredoc should work
    echo
done

#----------------------------------------

echo "Cleanup:"
echo

mv fibonacci.min.txt tmpfile # save output

rm *.min.txt

cd phase3
make clean
cd ..

echo

