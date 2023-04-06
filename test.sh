#!/bin/bash

export DBPATH=/tmp/frame
export PROG=./debug/bin/x86_64-linux-gnu/frame.elf

if [ ! -z "$VG" ]; then
   export VG="valgrind --leak-check=full --show-leak-kinds=all "
fi

die () {
   echo testcommand failure: $@
   exit -1
}

rm -rf $DBPATH
$VG $PROG --dbpath=$DBPATH init || die failed to init
$VG $PROG --dbpath=$DBPATH status || die failed status
$VG $PROG --dbpath=$DBPATH replace --message="Root: replacement" || die failed replace
$VG $PROG --dbpath=$DBPATH status || die failed status
$VG $PROG --dbpath=$DBPATH push one --message="One: message for one" || die failed push
$VG $PROG --dbpath=$DBPATH status || die failed status
$VG $PROG --dbpath=$DBPATH append --message="\nNew Message: One\n" || die failed append
$VG $PROG --dbpath=$DBPATH status || die failed status

