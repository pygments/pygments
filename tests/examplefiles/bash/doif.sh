#!/bin/bash

make -f Makefile

# For testing purposes, not intented to be correct
do-release-upgrade
do--release-upgrade
do_something
doanotherthing
do@arobase
do?question
do!exclamation
do@!?test
lets-do-it
if-else-script

do
    true;
done
until [ false ]

num=15
if [ $num -gt 10 ]; then
  echo "Number is greater than 10"
else
  echo "Number is 10 or less"
fi
