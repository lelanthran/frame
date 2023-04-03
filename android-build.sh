#!/bin/bash

# Build for android targets
#

# Use pretty colors for output
export FG_RESET="\033[39m"
export BG_RESET="\033[49m"
export RESET="${FG_RESET}${BG_RESET}"
export FG_BLACK="\033[30m"
export FG_RED="\033[31m"
export FG_GREEN="\033[32m"
export FG_YELLOW="\033[33m"
export FG_BLUE="\033[34m"
export FG_MAGENTA="\033[35m"
export FG_CYAN="\033[36m"
export FG_GREY90="\033[37m"
export BG_BLACK="\033[40m"
export BG_RED="\033[41m"
export BG_GREEN="\033[42m"
export BG_YELLOW="\033[43m"
export BG_BLUE="\033[44m"
export BG_MAGENTA="\033[45m"
export BG_CYAN="\033[46m"
export BG_GREY90="\033[47m"
export BG_GREY50="\033[100m"
export BG_LIGHTRED="\033[101m"
export BG_LIGHTGREEN="\033[102m"
export BG_LIGHTYELLOW="\033[103m"
export BG_LIGHTBLUE="\033[104m"
export BG_PURPLE="\033[105m"
export BG_TEAL="\033[106m"
export BG_WHITE="\033[107m"

# Quick exit with a message
function die () {
   printf "${FG_RED}$@${RESET}\n"
   printf "${BG_RED}${FG_BLACK}Aborting ...              ${RESET}\n"
   exit 127
}

# Print the variables we are using to locate the GCC we want.
function print_kv () {
   export size_name=`echo -ne $1 | wc -c`
   export nspaces=$((20 - $size_name))
   printf "${FG_BLUE}${1}${RESET}"
   printf "%${nspaces}s" " "
   printf "[${FG_GREEN}${2}${RESET}]\n"
}

# Read the user selections. Whatever is set on the command-line overrides the
# environment variables of the same name.
#

function highlight () {
   echo -ne "${BG_GREY90}${FG_GREEN}${1}${RESET}"
}

function print_help_message () {
   export HELP_MSG="
android-build.sh     [--android-ndk-path=<path>]
                     [--target-machine=<machine>]
                     [--android-level=<number>]
                     <make targets>

      Build for a specified android target architecture. At least one make
   target must be specified. Multiple make targets separated by spaces can
   be specified.

      Options are set from files, the environment and the command-line.  Any
   option set later overrides in options that were already set. Options are
   set in the following order:

         1. From the system android-build.conf file ("`highlight /etc/android-build.conf`"
            or "`highlight /c/Windows/android-build.conf`" on Windows).

         2. From the file "`highlight .android-build.conf`" file in the users home directory
            (even on Windows): "`highlight \\$HOME/.android-build.conf`"

         3. From the file '.android-build.conf' in the current directory:
            "`highlight \\$PWD/.android-build.conf`"

         4. From the environment variables (the description of each option below
            specifies which variable is used for each option).

         5. From the command-line, as described below.

      Command-line options override the environment variables, which override
   options set in the "`highlight \\$PWD/.android-build.conf`", which overrides
   "`highlight \\$HOME/.android-build.conf`" which overrides "`highlight \\$SYSROOT/android-build.conf`".

   --android-ndk-path=<path>
         Specify a path. A path must be specified. If no path is specified the
      environment variable "`highlight ANDROID_NDK_PATH`" is used.

   --target-machine=<machine>
         Specify the machine to build for. The name "`highlight all`" will build for all
      machines found in the Android NDK. At least one machine or the word 'all'
      must be specified. Multiple target machines may be specified using spaces
      to separate the machine names. If a target machine is not specified the
      environment variable "`highlight TARGET_MACHINE`" is used.

   --android-level=<number>
         Specify the android level to build for. Multiple android levels can be
      specified, seperated with a space. If a level is not specified the
      environment variable "`highlight ANDROID_LEVEL`" is used.
   "
   printf "$HELP_MSG\n"

   exit 0
}

export HOST_MACHINE="`uname -m`"
export HOST_OS="`uname -o`"
export TARGET_OS=linux
export SYSROOT_CONF=/etc/android-build.conf

# TODO: Double-check this on Windows.
if [ "$HOST_OS" == "msys" ]; then
   export SYSROOT_CONF=/c/Windows/android-build.conf
fi

function source_file () {
   if [ -f "$1" ]; then
      printf "Reading variables from ${FG_MAGENTA}$1${RESET}\n"
      . "$1"
   fi
}

source_file "$SYSROOT_CONF"
source_file "$HOME/.android-build.conf"
source_file "$PWD/.android-build.conf"

function get_cline_value () {
   echo $1 | cut -f 2 -d =
}

while [ ! -z "$1" ]; do
   case $1 in

      --help*)          print_help_message
         ;;

      --android-ndk-path=*)   export ANDROID_NDK_PATH=`get_cline_value $1`
         ;;

      --target-machine=*)     export TARGET_MACHINE=`get_cline_value "$1"`

         ;;

      --android-level=*)      export ANDROID_LEVEL=`get_cline_value "$1"`
         ;;

      *)                      export MAKE_TARGETS="$MAKE_TARGETS $1"
         ;;

   esac
   shift 1
done

[ -z "${MAKE_TARGETS}" ]     && die "No make targets specified."
[ -z "${ANDROID_NDK_PATH}" ] && die 'Value $ANDROID_NDK_PATH not set. Try --help.'
[ -z "${TARGET_MACHINE}" ]   && die 'Value $TARGET_MACHINE not set. Try --help.'
[ -z "${ANDROID_LEVEL}" ]    && die 'Value $ANDROID_LEVEL not set. Try --help.'

# TODO: Add in the one for Windows here
case "$HOST_OS" in

   GNU/Linux)     export HOST_OS=linux
      ;;


   *)             die "Unknown OS - $HOST_OS"
      ;;
esac

export HOST_BUILD_TOOLS="$ANDROID_NDK_PATH/toolchains/llvm/prebuilt/$HOST_OS-$HOST_MACHINE"

if [ "$TARGET_MACHINE" == "all" ]; then
   export INSTALLED_ARCHES="`ls -1 $HOST_BUILD_TOOLS/bin/*-android*-clang* | sed \"s:$HOST_BUILD_TOOLS/bin/::g\" | cut -f 1 -d - | sort -u`"
   echo "Found the following arches: $INSTALLED_ARCHS"
   export TARGET_MACHINE=$INSTALLED_ARCHES
fi

for X in $TARGET_MACHINE; do
   export TARGET_EABI=android
   if [ `echo $X | grep -c arm` -gt 0 ]; then
      export TARGET_EABI=androideabi
      echo "TARGET_EABI $TARGET_EABI"
   fi

   for Y in $ANDROID_LEVEL; do
      export ANDROID_GCC="$HOST_BUILD_TOOLS/bin/${X}-${TARGET_OS}-${TARGET_EABI}${Y}-clang"
      export ANDROID_GXX="$HOST_BUILD_TOOLS/bin/${X}-${TARGET_OS}-${TARGET_EABI}${Y}-clang++"

      print_kv MAKE_TARGETS         ${MAKE_TARGETS}
      print_kv ANDROID_NDK_PATH     ${ANDROID_NDK_PATH}
      print_kv ANDROID_GCC          ${ANDROID_GCC}
      print_kv HOST_MACHINE         ${HOST_MACHINE}
      print_kv HOST_OS              ${HOST_OS}
      print_kv TARGET_MACHINE       ${X}
      print_kv ANDROID_LEVEL        ${Y}

      $ANDROID_GCC --version &> /dev/null || die "Error executing android compiler [${BG_RED}${FG_BLACK}$ANDROID_GCC${RESET}]"

      export GCC=$ANDROID_GCC
      export GXX=$ANDROID_GXX
      export LD_PROG=$ANDROID_GCC
      export LD_LIB=$ANDROID_GCC

      for Z in $MAKE_TARGETS; do
         make $Z -j 4
      done
   done
done
