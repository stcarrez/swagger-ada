#!/bin/sh
NAME=openapi.cov
lcov --quiet --base-directory . --directory . -c -o $NAME
lcov --quiet --remove $NAME "/usr*" -o $NAME
lcov --quiet --remove $NAME "/build*" -o $NAME
lcov --quiet --remove $NAME "/opt*" -o $NAME
lcov --quiet --remove $NAME "*/adainclude*" -o $NAME
lcov --quiet --remove $NAME "*/regtests*" -o $NAME
lcov --quiet --remove $NAME "*/b__*" -o $NAME
rm -rf cover
genhtml --quiet --ignore-errors source -o ./cover -t "test coverage" --num-spaces 4 $NAME
 
