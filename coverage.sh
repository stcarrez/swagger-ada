#!/bin/sh
NAME=openapi.cov
lcov --base-directory . --directory . -c --include "*/openapi-ada/src/*" -o $NAME
rm -rf cover
genhtml --ignore-errors source -o ./cover -t "test coverage" --num-spaces 4 $NAME
 
