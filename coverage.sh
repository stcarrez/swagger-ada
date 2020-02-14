#!/bin/sh
NAME=openapi.cov
lcov --quiet --base-directory . --directory . -c --include "*/openapi-ada/src/*" -o $NAME
rm -rf cover
genhtml --quiet --ignore-errors source -o ./cover -t "test coverage" --num-spaces 4 $NAME
 
