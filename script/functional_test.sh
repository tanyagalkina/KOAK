#!/bin/bash

# COLOR

LIGHTRED='\033[1;31m'
LIGHTGREEN='\033[1;32m'
WHITE='\033[1;37m'
NOCOLOR='\033[0m'

# BINARY

KOAK=./koak

# VARIABLE

nb_test=0
nb_failure=0
success_percentage=0.00

# TEST FUNCTION

function test() {
    $KOAK $1 > unit_test_result.txt
    echo -e "${WHITE}Try [./koak ${1}]"
    if [[ $(< unit_test_result.txt) == $2 ]]
        then
            echo -e "${LIGHTGREEN}SUCCESS !"
        else
            echo -e "${LIGHTRED}FAILURE !"
            echo -e "${LIGHTGREEN}    Expected : ${2}"
            echo -ne "${LIGHTRED}    Got : "
            cat unit_test_result.txt
            if [[ $(< unit_test_result.txt) == "" ]]
                then
                    echo ""
            fi
            ((nb_failure=nb_failure+1))
    fi
    echo ""
    ((nb_test=nb_test+1))
}

# EXAMPLES

test "data/examples/numberExamples/numberExample1.koak" "1"
test "data/examples/numberExamples/numberExample2.koak" "2"
test "data/examples/numberExamples/numberExample3.koak" "270198"
test "data/examples/numberExamples/numberExample3.koak" "3.14"

test "data/examples/binopExample.koak" "TO DO"
test "data/examples/unaryExample.koak" "TO DO"
test "data/examples/primaryExample.koak" "TO DO"
test "data/examples/postfixExample.koak" "TO DO"
test "data/examples/exprExample.koak" "TO DO"
test "data/examples/whileExprExample.koak" "TO DO"
test "data/examples/ifExprExample.koak" "TO DO"
test "data/examples/forExprExample.koak" "TO DO"
test "data/examples/defsExample.koak" "TO DO"
test "data/examples/subjectExample.koak" "TO DO"

# FUNCTIONS

test "data/functions/fibonnacci.koak" "TO DO"
test "data/functions/getNumber.koak" "TO DO"

# DELETE TEMPORARY FILE

rm unit_test_result.txt

# PRINT SUMMARY

((success_percentage=100-(nb_failure*100/nb_test)))

if [[ $nb_failure != 0 ]]
    then
        echo -e "${LIGHTRED}${nb_test} test(s) | ${nb_failure} failure(s) | ${success_percentage}% of completion"
        echo -ne "${NOCOLOR}"
        exit 1
    else
        echo -e "${LIGHTGREEN}${nb_test} test(s) | ${nb_failure} failure(s) | ${success_percentage}% of completion"
        echo -ne "${NOCOLOR}"
        exit 0
fi