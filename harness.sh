#!/bin/bash

# USAGE: ./harness.sh
# build and test script for phase3 mini-l compiler

#----------------------------------------

# this is the function called at bottom of script
# comment out subfunctions in main to change script
function main()
{
    build_cc
    cc_files
    test_files
    clean_up
}

#----------------------------------------

function build_cc()
{
    echo
    echo "Making compiler:"
    echo

    cd phase3
    make
    cd ..
}
#----------------------------------------

function cc_files()
{
    echo
    echo "Output files:"
    echo

    FILES="tests"
    COMP="phase3/compiler"
    for i in $(ls $FILES | grep \.min)
    do
        echo $i".txt"
        $COMP $FILES/$i > $i".txt"
    done
}
#----------------------------------------

# run generated code against mil interpreter
function test_files()
{
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
}
#----------------------------------------

function clean_up()
{
    echo "Clean up:"
    echo

    rm *.min.txt

    cd phase3
    make clean
    cd ..

    echo
}
#----------------------------------------

# MAIN
main
