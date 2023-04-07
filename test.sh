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
$VG $PROG --dbpath=$DBPATH history || die failed history
echo '--------------------------------'

$VG $PROG --dbpath=$DBPATH replace --message="Root: replacement" || die failed replace
$VG $PROG --dbpath=$DBPATH status || die failed status
$VG $PROG --dbpath=$DBPATH history || die failed history
echo '--------------------------------'

$VG $PROG --dbpath=$DBPATH push one --message="One: message one" || die failed push
$VG $PROG --dbpath=$DBPATH status || die failed status
$VG $PROG --dbpath=$DBPATH history || die failed history
echo '--------------------------------'

$VG $PROG --dbpath=$DBPATH append --message="\nMessage: 1\n" || die failed append
$VG $PROG --dbpath=$DBPATH status || die failed status
$VG $PROG --dbpath=$DBPATH history || die failed history
echo '--------------------------------'

$VG $PROG --dbpath=$DBPATH up || die failed uptree
$VG $PROG --dbpath=$DBPATH status || die failed status
$VG $PROG --dbpath=$DBPATH history || die failed history
echo '--------------------------------'

$VG $PROG --dbpath=$DBPATH push two --message="Two: message two" || die failed push
$VG $PROG --dbpath=$DBPATH status || die failed status
$VG $PROG --dbpath=$DBPATH history || die failed history
echo '--------------------------------'

$VG $PROG --dbpath=$DBPATH switch root/one || die failed switch
$VG $PROG --dbpath=$DBPATH status || die failed status
$VG $PROG --dbpath=$DBPATH history || die failed history
echo '--------------------------------'

$VG $PROG --dbpath=$DBPATH push three --message="three: three" || die failed push
$VG $PROG --dbpath=$DBPATH status || die failed status
$VG $PROG --dbpath=$DBPATH history || die failed history
echo '--------------------------------'

$VG $PROG --dbpath=$DBPATH pop  || die failed pop
$VG $PROG --dbpath=$DBPATH status || die failed status
$VG $PROG --dbpath=$DBPATH history || die failed history
echo '--------------------------------'


