#!/bin/bash

export DBPATH=/tmp/frame/
export PROG=./debug/bin/x86_64-linux-gnu/frame.elf

if [ ! -z "$VG" ]; then
   export VG="valgrind --leak-check=full --show-leak-kinds=all --error-exitcode=1"
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
      $VG $@ --dbpath=$DBPATH
      export RET=$?
      export STMT_NUM=$(($STMT_NUM + 1))
      return $RET
   fi

   if [ "$DEBUG" -eq "$STMT_NUM" ]; then
      gdb --args $@ --dbpath=$DBPATH
      export RET=$?
      export STMT_NUM=$(($STMT_NUM + 1))
      return $RET
   fi

   export STMT_NUM=$(($STMT_NUM + 1))
   $@ --dbpath=$DBPATH
}

rm -rf $DBPATH
execute $PROG create || die failed to create
execute $PROG status || die failed status
execute $PROG history || die failed history

execute $PROG replace --message="Root: replacement" || die failed replace
execute $PROG status || die failed status
execute $PROG history || die failed history

execute $PROG push one --message="One: message one" || die failed push
execute $PROG status || die failed status
execute $PROG history || die failed history

execute $PROG append --message="\nMessage: 1\n" || die failed append
execute $PROG status || die failed status
execute $PROG history || die failed history

execute $PROG up || die failed uptree
execute $PROG status || die failed status
execute $PROG history || die failed history

execute $PROG push two --message="Two: message two" || die failed push
execute $PROG status || die failed status
execute $PROG history || die failed history

execute $PROG switch root/one || die failed switch
execute $PROG status || die failed status
execute $PROG history || die failed history

execute $PROG push three --message="three: three" || die failed push
execute $PROG status || die failed status
execute $PROG history || die failed history

execute $PROG pop  || die failed pop
execute $PROG status || die failed status
execute $PROG history || die failed history

execute $PROG push four --message="four: four" || die failed push
execute $PROG status || die failed status
execute $PROG history || die failed history

execute $PROG delete root/two  || die failed delete
execute $PROG status || die failed status
execute $PROG history || die failed history

execute $PROG switch root || die failed switch
execute $PROG status || die failed status
execute $PROG history || die failed history

execute $PROG push five --message="five: five" || die failed push
execute $PROG up || die failed up
execute $PROG push six --message="six: six" || die failed push
execute $PROG up || die failed up
execute $PROG push seven --message="seven: seven" || die failed push
execute $PROG up || die failed up
execute $PROG push eight --message="eight: eight" || die failed push
execute $PROG up || die failed up
execute $PROG push eighteen --message="eight: eight" || die failed push
execute $PROG up || die failed up
execute $PROG push eighty --message="eight: eight" || die failed push
execute $PROG up || die failed up
execute $PROG down one || die failed down
execute $PROG push one --message="new one" || die failed push
execute $PROG up || die failed up
execute $PROG push two --message="new one" || die failed push
execute $PROG up || die failed up
execute $PROG push three --message="new one" || die failed push
execute $PROG up || die failed up
# execute $PROG push four --message="new one" || die failed push
# execute $PROG up || die failed up
execute $PROG push five --message="new one" || die failed push
execute $PROG up || die failed up
execute $PROG push six --message="new one" || die failed push
execute $PROG up || die failed up
execute $PROG push seven --message="new one" || die failed push
execute $PROG up || die failed up
execute $PROG push eight --message="new one" || die failed push
execute $PROG up || die failed up
execute $PROG push nine --message="new one" || die failed push
execute $PROG up || die failed up
execute $PROG push ten --message="new one" || die failed push
execute $PROG up || die failed up
execute $PROG status || die failed status
execute $PROG list || die failed list

# Current node is root/one
execute $PROG match --from-root "e" || die failed match
execute $PROG match --from-root "ei" || die failed match
execute $PROG match --from-root "eigh" || die failed match

execute $PROG match "e" || die failed match
execute $PROG match "ei" || die failed match
execute $PROG match "eigh" || die failed match
execute $PROG match "eigh" --inverse || die failed match
execute $PROG match "one/eigh" && die expected failed match

execute $PROG match --from-root "one/ei" || die failed match
