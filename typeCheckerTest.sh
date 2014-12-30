#!/bin/bash

# Typechecker test

echo "Typechecker tests"
echo "####################################################"


testdir="testlatte"
good="$testdir/good"
bad="$testdir/bad"
arrays="$testdir/extensions/arrays1"
struct="$testdir/extensions/struct"
objects1="$testdir/extensions/objects1"
objects2="$testdir/extensions/objects2"
all_succeded="All tests succeded!"

function test {
	echo "Tests from $1 directory, with using Typechecker from $2 directory" 

	succeded="All test from $1 succeded!"

	for test in $(ls $1 | grep lat)
	do
		output=$($2/TypeChecker $1/$test | grep "Correct")
		if [ -z "$output" ]; then
			echo "Test $test failed"
			succeded=""
			all_succeded=""
		else
			echo "Test $test succeded"
		fi
	done
	echo $succeded
	echo "####################################################"
}  

test $good $2
test $arrays $2
test $bad $2
# test $struct
# test $objects1
# test $objects2
# echo $all_succeded