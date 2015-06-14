#! /usr/bin/env bash
# 
# Running tests
#   ./test.sh # this script
#
# Adding a test
#   Add two files to this directory
#       <name>.lit and <name>
#   Where <name> is the correct code output of <name.lit

# Globals for directories/lit exe
test_dir=$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )
tests=$(ls $test_dir/*.lit)
lit_home="$test_dir/../"
lit_exec=$lit_home/dist/build/lit/lit

# Store the output from running lit in test/results
[ -d $test_dir/results ] || mkdir $test_dir/results &>/dev/null
 
num_tests=$(wc -w <<< $tests)
num_pass=0
for test in $tests; do
    prefix=$(basename ${test%.*})
    printf "Testing $prefix: "
    $lit_exec -c "$test" --code-dir $test_dir/results &>/dev/null
    diff $test_dir/$prefix $test_dir/results/$prefix > $test_dir/results/$prefix.diff
    if [ -s $test_dir/results/$prefix.diff ]; then
        echo "failed, see $test_dir/results/$prefix.diff"
    else
        echo "passed"
        num_pass=$(( $num_pass + 1 ))
        rm $test_dir/results/$prefix.diff
    fi
done

# Print summary
echo ""
echo "Passed: $num_pass/$num_tests"

if [ $num_pass -lt $num_tests ]; then
    exit 1;
fi
