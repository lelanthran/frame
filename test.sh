#!/bin/bash

export DBPATH=/tmp/frame/
export PROG=./debug/bin/x86_64-linux-gnu/frame.elf

if [ ! -z "$VG" ]; then
   export VG="valgrind --leak-check=full --show-leak-kinds=all "
fi

die () {
   echo testcommand failure: $@
   exit -1
}

export NC="\e[0m"
export INV="\e[7m"
export RED="\e[31m"
export GREEN="\e[32m"
export BLUE="\e[34m"
export CYAN="\e[36m"
export YELLOW="\e[33m"

export HI_ON="${RED}${INV}"
export STMT_NUM=0

if [ -z "$DEBUG" ]; then
   export DEBUG=-1
fi

execute () {
   echo -ne "${RED}Executing${NC} ${GREEN}$STMT_NUM:${NC} "
   echo -e "${BLUE}$@${NC}"
   if [ ! -z "$VG" ]; then
      valgrind --leak-check=full --show-leak-kinds=all $@
      export RET=$?
      export STMT_NUM=$(($STMT_NUM + 1))
      return $RET
      return $?
   fi

   if [ "$DEBUG" -eq "$STMT_NUM" ]; then
      gdb --args $@
      export RET=$?
      export STMT_NUM=$(($STMT_NUM + 1))
      return $RET
   fi

   export STMT_NUM=$(($STMT_NUM + 1))
   $@
}

rm -rf $DBPATH
execute $PROG --dbpath=$DBPATH init || die failed to init
execute $PROG --dbpath=$DBPATH status || die failed status
execute $PROG --dbpath=$DBPATH history || die failed history

execute $PROG --dbpath=$DBPATH replace --message="Root: replacement" || die failed replace
execute $PROG --dbpath=$DBPATH status || die failed status
execute $PROG --dbpath=$DBPATH history || die failed history

execute $PROG --dbpath=$DBPATH push one --message="One: message one" || die failed push
execute $PROG --dbpath=$DBPATH status || die failed status
execute $PROG --dbpath=$DBPATH history || die failed history

execute $PROG --dbpath=$DBPATH append --message="\nMessage: 1\n" || die failed append
execute $PROG --dbpath=$DBPATH status || die failed status
execute $PROG --dbpath=$DBPATH history || die failed history

execute $PROG --dbpath=$DBPATH up || die failed uptree
execute $PROG --dbpath=$DBPATH status || die failed status
execute $PROG --dbpath=$DBPATH history || die failed history

execute $PROG --dbpath=$DBPATH push two --message="Two: message two" || die failed push
execute $PROG --dbpath=$DBPATH status || die failed status
execute $PROG --dbpath=$DBPATH history || die failed history

execute $PROG --dbpath=$DBPATH switch root/one || die failed switch
execute $PROG --dbpath=$DBPATH status || die failed status
execute $PROG --dbpath=$DBPATH history || die failed history

execute $PROG --dbpath=$DBPATH push three --message="three: three" || die failed push
execute $PROG --dbpath=$DBPATH status || die failed status
execute $PROG --dbpath=$DBPATH history || die failed history

execute $PROG --dbpath=$DBPATH pop  || die failed pop
execute $PROG --dbpath=$DBPATH status || die failed status
execute $PROG --dbpath=$DBPATH history || die failed history


