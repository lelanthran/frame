
# ########################################################################## #
# Frame  (©2023 Lelanthran Manickum)                                         #
#                                                                            #
# This program comes with ABSOLUTELY NO WARRANTY. This is free software      #
# and you are welcome to redistribute it under certain conditions;  see      #
# the LICENSE file for details.                                              #
# ########################################################################## #

$DBPATH="$Env:TEMP\frame"
$PROG="$PWD\recent\bin\x86_64-w64-mingw32\frame.exe"

$NC="[0m"
$INV="[7m"
$RED="[31m"
$GREEN="[32m"
$BLUE="[34m"
$CYAN="[36m"
$YELLOW="[33m"
$HI_ON="$RED$NV"

$STMT_NUM=0

$LIMIT="$1"

echo Removing $DBPATH
Remove-Item $DBPATH -Recurse -Force

function Execute-Frame {
   Param (
      [string]$p1,
      [string]$p2,
      [string]$p3,
      [string]$p4,
      [string]$p5
   )
   echo "$REDExecuting$NC $GREEN $global:STMT_NUM $NC"
   echo "$BLUE" $p1 $p2 $p3 $p4 $p5 " "  "$NC"
   $global:STMT_NUM++
   &  $p1 $p2 $p3 $p4 $p5 "--dbpath=$DBPATH"
   if ($LastExitCode -ne 0) {
      die "Command Failure"
   }

}


function die {
   Param (
      [string] $msg
   )
   echo $msg
   exit -1
}


Execute-Frame $PROG create
Execute-Frame $PROG status
Execute-Frame $PROG history

echo Created

Execute-Frame $PROG replace --message="Root: replacement"
Execute-Frame $PROG status
Execute-Frame $PROG history

Execute-Frame $PROG push one --message="One: message one"
Execute-Frame $PROG status
Execute-Frame $PROG history

Execute-Frame $PROG append --message="\nMessage: 1\n"
Execute-Frame $PROG status
Execute-Frame $PROG history

Execute-Frame $PROG up
Execute-Frame $PROG status
Execute-Frame $PROG history

Execute-Frame $PROG push two --message="Two: message two"
Execute-Frame $PROG status
Execute-Frame $PROG history

Execute-Frame $PROG top
Execute-Frame $PROG down one
Execute-Frame $PROG status
Execute-Frame $PROG history

Execute-Frame $PROG back 1
Execute-Frame $PROG status
Execute-Frame $PROG history

Execute-Frame $PROG back 1
Execute-Frame $PROG status
Execute-Frame $PROG history

Execute-Frame $PROG push three --message="three: three"
Execute-Frame $PROG status
Execute-Frame $PROG history

Execute-Frame $PROG pop 
Execute-Frame $PROG status
Execute-Frame $PROG history

Execute-Frame $PROG push four --message="four: four"
Execute-Frame $PROG status
Execute-Frame $PROG history

Execute-Frame $PROG delete root/two 
Execute-Frame $PROG status
Execute-Frame $PROG history

Execute-Frame $PROG top
Execute-Frame $PROG status
Execute-Frame $PROG history


Execute-Frame $PROG push five --message="five: five"
Execute-Frame $PROG up
Execute-Frame $PROG push six --message="six: six"
Execute-Frame $PROG up
Execute-Frame $PROG push seven --message="seven: seven"
Execute-Frame $PROG up
Execute-Frame $PROG push eight --message="eight: eight"
Execute-Frame $PROG up
Execute-Frame $PROG push eighteen --message="eight: eight"
Execute-Frame $PROG up
Execute-Frame $PROG push eighty --message="eight: eight"
Execute-Frame $PROG up
Execute-Frame $PROG down one
Execute-Frame $PROG push one --message="new one"
Execute-Frame $PROG up
Execute-Frame $PROG push two --message="new one"
Execute-Frame $PROG up
Execute-Frame $PROG push three --message="new one"
Execute-Frame $PROG up
# execute $PROG push four --message="new one"
# execute $PROG up
Execute-Frame $PROG push five --message="new one"
Execute-Frame $PROG up
Execute-Frame $PROG push six --message="new one"
Execute-Frame $PROG up
Execute-Frame $PROG push seven --message="new one"
Execute-Frame $PROG up
Execute-Frame $PROG push eight --message="new one"
Execute-Frame $PROG up
Execute-Frame $PROG push nine --message="new one"
Execute-Frame $PROG up
Execute-Frame $PROG push ten --message="new one"
Execute-Frame $PROG up
Execute-Frame $PROG status
Execute-Frame $PROG list

# Current node is root/one
Execute-Frame $PROG match --from-root "e"
Execute-Frame $PROG match --from-root "ei"
Execute-Frame $PROG match --from-root "eigh"

Execute-Frame $PROG match "e"
Execute-Frame $PROG match "ei"
Execute-Frame $PROG match "eigh"
Execute-Frame $PROG match "eigh" --invert
Execute-Frame $PROG status
Execute-Frame $PROG match "one/egh" > t
# if [ `wc -l t | cut -f 1 -d \   ` -ne 1 ]; then
#   die expected failed match
# fi

# Rename root/one/five
Execute-Frame $PROG switch root/one/five
Execute-Frame $PROG rename 'FIVE'
# Current node is root/one/FIVE
Execute-Frame $PROG status

Execute-Frame $PROG match --from-root "one/ei"

Execute-Frame $PROG tree


exit 0

