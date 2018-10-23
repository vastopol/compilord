#!/bin/bash

make clean
make
./parser "$1" > test2/tmp.parse
make clean
