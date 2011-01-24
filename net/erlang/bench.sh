#!/usr/bin/env zsh

TIMEFMT=$'%E'

tests=(10 100 200 300 400 500 750 1000 2000 3000 4000 5000 6000 7000 8000 9000 10000)
#tests=(10 20)
for i in "${tests[@]}"
do
    echo -n "$i "
    time erl -noshell -s stress_test main $i
done
