#!/bin/bash

for filename in ../tests/*.kt; do
    echo 'Testing: ' "$filename"
    ./Main "$filename"
    sleep 1
done
