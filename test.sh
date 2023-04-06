#!/bin/bash

export DBPATH=/tmp/frame
export PROG=./debug/bin/x86_64-linux-gnu/frame.elf

rm -rf $DBPATH
$PROG init
$PROG status
$PROG push one --message="My Message"
$PROG status

