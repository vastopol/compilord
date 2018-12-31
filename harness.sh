#!/bin/bash

# USAGE: ./harness.sh
# build and test script for phase3 mini-l compiler

#----------------------------------------

# shell variables used by the various functions below.

FOLDER="phase4"
FILES="tests"
TESTS="tests/custom"
COMP=$FOLDER"/compiler"
MILR="tests/mil_run"

#----------------------------------------

# this is the function called at bottom of script
# comment out subfunctions in main to change script actions
function main()
{
    build_cc
    test_cc
    test_error
    cc_files
    test_files
    clean_up
}

#----------------------------------------

# make the phase3 compiler
function build_cc()
{
    echo
    echo "Making compiler:"
    echo

    cd $FOLDER
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

    $COMP $TESTS/test_comp.min > test_comp.min.mil

    cat test_comp.min.mil;
    echo

    $MILR test_comp.min.mil <<< '1 2 3 4 5 6 7' # there are 7 reads in the test file
}
#----------------------------------------

# compile and run custom tests
function test_error()
{
    echo
    echo "Testing Errors:"
    echo

    $COMP $TESTS/err_comp1.min > err_comp1.min.mil
    $COMP $TESTS/err_comp2.min > err_comp2.min.mil
    $COMP $TESTS/err_comp3.min > err_comp3.min.mil

    cat err_comp1.min.mil;
    echo

    cat err_comp2.min.mil;
    echo

    cat err_comp3.min.mil;
    echo
}
#----------------------------------------

# compile the standard test files: fibonacci, mytest, primes
function cc_files()
{
    echo
    echo "Compiling files:"
    echo

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

    for i in $(ls | grep \.min.mil)
    do
        echo $i
        $MILR $i <<< '10 5' # mytest.min has 2 reads
        echo
    done
}
#----------------------------------------

# remove generated files from build
function clean_up()
{
    echo "Clean up:"
    echo

    rm *.mil

    cd $FOLDER
    make clean
    cd ..

    echo
}
#----------------------------------------

# MAIN
main
