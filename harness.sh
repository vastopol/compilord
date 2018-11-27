#!/bin/bash

# USAGE: ./harness.sh
# build and test script for phase3 mini-l compiler

#----------------------------------------

# this is the function called at bottom of script
# comment out subfunctions in main to change script actions
function main()
{
    build_cc
    test_cc
    cc_files
    # test_files
    # clean_up
}

#----------------------------------------

# make the phase3 compiler
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

# compile and run custom tests
function test_cc()
{
    echo
    echo "Testing compiler:"
    echo

    TESTS="tests/custom"
    COMP="phase3/compiler"
    MILR="tests/mil_run"

    $COMP $TESTS/test_comp.min > test_comp.min.mil

    cat test_comp.min.mil
    echo

    $MILR test_comp.min.mil
}
#----------------------------------------

# compile the standard test files: fibonacci, mytest, primes
function cc_files()
{
    echo
    echo "Compiling files:"
    echo

    FILES="tests"
    COMP="phase3/compiler"
    for i in $(ls $FILES | grep \.min)
    do
        echo $i".mil"
        $COMP $FILES/$i > $i".mil"
    done
}
#----------------------------------------

# run generated code from the standard test files against mil interpreter
function test_files()
{
    echo
    echo "Testing files:"
    echo

    MILR="tests/mil_run"
    for i in $(ls | grep \.min.mil)
    do
        echo $i
        $MILR $i <<< 10 # heredoc should work
        echo
    done
}
#----------------------------------------

# remove generated files from build
function clean_up()
{
    echo "Clean up:"
    echo

    rm *.min.mil

    cd phase3
    make clean
    cd ..

    echo
}
#----------------------------------------

# MAIN
main
