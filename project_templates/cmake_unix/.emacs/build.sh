#!/bin/bash

config=$1
cmake --build build/$config -- -j6
