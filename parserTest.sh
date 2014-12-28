#!/bin/bash

# Parser test

echo "Parser tests"
echo "####################################################"


testdir="testlatte"
good="$testdir/good"
arrays="$testdir/extensions/arrays1"
struct="$testdir/extensions/struct"
objects1="$testdir/extensions/objects1"
objects2="$testdir/extensions/objects2"
all_succeded="All tests succeded!"

function test {
	echo "Tests from $1 directory"

	succeded="All test from $1 succeded!"

	for test in $(ls $1 | grep lat)
	do
		output=$(latte/TestLatte $1/$test | grep "Parse Successful\!")
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

test $good
test $arrays
test $struct
test $objects1
test $objects2
echo $all_succeded