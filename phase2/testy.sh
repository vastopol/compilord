#!/bin/bash

make clean
make
./parser "$1" > p2tests/tmp.parse
make clean
