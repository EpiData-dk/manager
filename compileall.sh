#!/bin/bash
# Compile all versions of manager!

## Changeable options. 
SIMPLEOPTS="-MObjFPC -Sci -Xs -CX -Ci -WG -O3 -l"
#SIMPLEOPTS="$SIMPLEOPTS -gl -g -dEPI_DEBUG"
MANAGERFILENAME="epidatamanager"

# Paths - change to your location
CORE_LOCATION="/home/torsten/EpiData/Core/trunk"
FPSPREASHEET_LOCATION="/home/torsten/FreePascal/fpspreadsheet"
LAZARUS_LOCATION="/home/torsten/FreePascal/lazarus"
SVN2REV_CMD="$HOME/bin/svn2revisioninc"

###########################################
#  DO NOT EDIT				  #
###########################################
SVN_LOC=""
check_svn_status() {
  RES=`svnversion $SVN_LOC | grep [MSP:] | wc -l`
  if [ $RES != "0" ]
  then
    echo ""
    echo "Warning:"
    echo "$SVN_LOC is not commited to repository."
  fi
}


get_filename() {
  FILENAME="$MANAGERFILENAME.$MANAGER_VERSION.$MY_CPU_TARGET-$MY_OS_TARGET"
  if [ $MY_LCL_TARGET = "win32" ]
  then
    FILENAME="$FILENAME.exe"
  fi
}

clean_filename() {
  get_filename
  if [ -a "$MANAGER_LOCATION/$FILENAME" ] 
  then
    rm -f "$MANAGER_LOCATION/$FILENAME"
  fi
}

clean_lib() {
  if [ -d "$MANAGER_LOCATION/lib/$MY_CPU_TARGET-$MY_OS_TARGET" ]
  then
    rm -fr "$MANAGER_LOCATION/lib/$MY_CPU_TARGET-$MY_OS_TARGET"
  fi
}

clean() {
  clean_filename
  clean_lib
}

clean_core() {
  HERE=`pwd`
  cd "$CORE_LOCATION"
  make clean CPU_TARGET="$MY_CPU_TARGET" OS_TARGET="$MY_OS_TARGET" OPT="$SIMPLEOPTS" > /dev/null
  cd "$HERE"
}

compile_core() {
  clean_core

  HERE=`pwd`
  cd "$CORE_LOCATION"
  make all CPU_TARGET="$MY_CPU_TARGET" OS_TARGET="$MY_OS_TARGET" OPT="$SIMPLEOPTS" > /dev/null
  if [ $MY_OS_TARGET = "win32" ]
  then
    make all CPU_TARGET="$MY_CPU_TARGET" OS_TARGET="$MY_OS_TARGET" OPT="$SIMPLEOPTS" > /dev/null
  fi
  cd "$HERE"
}

clean_up() {
  clean_lib
  clean_core
}

compile() {
  echo "Compiling for: $MY_CPU_TARGET-$MY_OS_TARGET"
  echo "------------------------------"
  clean
  compile_core

  CPU="-P$MY_CPU_TARGET"
  TARGET="-T$MY_OS_TARGET"

  mkdir -p "$MANAGER_LOCATION/lib/$MY_CPU_TARGET-$MY_OS_TARGET"
  MANAGER_OUTPUT="-FU$MANAGER_LOCATION/lib/$MY_CPU_TARGET-$MY_OS_TARGET/"
  MANAGER_UNITS="-Fu$MANAGER_LOCATION/editor -Fu$MANAGER_LOCATION/designer -Fu$MANAGER_LOCATION -Fu."
  MANAGER_INCLUDES="-Fi$MANAGER_LOCATION/lib/$MY_CPU_TARGET-$MY_OS_TARGET"
  CORE_UNITS="-Fu$CORE_LOCATION/lib/$MY_CPU_TARGET-$MY_OS_TARGET/"
  FPSPREAD_UNITS="-Fu$FPSPREASHEET_LOCATION/lib/$MY_CPU_TARGET-$MY_OS_TARGET/"
  MANAGER_OPTS="$MANAGER_OUTPUT $MANAGER_UNITS $MANAGER_INCLUDES $CORE_UNITS $FPSPREAD_UNITS"

  LCL_UNITS="-Fu$LAZARUS_LOCATION/lcl/units/$MY_CPU_TARGET-$MY_OS_TARGET/ -Fu$LAZARUS_LOCATION/lcl/units/$MY_CPU_TARGET-$MY_OS_TARGET/$MY_LCL_TARGET/"
  FCL_UNITS="-Fu$LAZARUS_LOCATION/packager/units/$MY_CPU_TARGET-$MY_OS_TARGET/"
  SYNEDIT_UNITS="-Fu$LAZARUS_LOCATION/components/synedit/units/$MY_CPU_TARGET-$MY_OS_TARGET/"
  LAZARUS_UNITS="$LCL_UNITS $FCL_UNITS $SYNEDIT_UNITS"
  
  get_filename
  OUTPUTNAME="-o$FILENAME"
  DEFINEOPTS="-dLCL -d$MY_LCL_TARGET"

#  echo "fpc $CPU $TARGET $SIMPLEOPTS $MANAGER_OPTS $LAZARUS_UNITS $OUTPUTNAME $DEFINEOPTS epidatamanager.lpr"
  fpc $CPU $TARGET $SIMPLEOPTS $MANAGER_OPTS $LAZARUS_UNITS $OUTPUTNAME $DEFINEOPTS epidatamanager.lpr 

  clean_up
  echo ""
}

get_version_info() {
  V1=`cat epidatamanager.lpi | grep CurrentVersionNr | cut -c 32-32`
  if [ -z $V1 ]
  then
    V1="0"
  fi

  V2=`cat epidatamanager.lpi | grep CurrentMajorRevNr | cut -c 33-33`
  if [ -z $V2 ]
  then
    V2="0"
  fi

  V3=`cat epidatamanager.lpi | grep CurrentMinorRevNr | cut -c 33-33`
  if [ -z $V3 ]
  then
    V3="0"
  fi

  V4=`cat epidatamanager.lpi | grep CurrentBuildNr | cut -c 30-30`
  if [ -z $V4 ]
  then
    V4="0"
  fi

  MANAGER_VERSION="$V1.$V2.$V3.$V4" 
}

# Easy paths:
MANAGER_LOCATION=`pwd`

SVN_LOC=$CORE_LOCATION
check_svn_status
SVN_LOC=$MANAGER_LOCATION
check_svn_status

SVN2REV_CMD="$SVN2REV_CMD $MANAGER_LOCATION"
$SVN2REV_CMD

get_version_info

echo "**********************"
echo " Start compiling..."
echo " Manager version: $MANAGER_VERSION"
echo "**********************"

MY_LCL_TARGET="win32"
MY_OS_TARGET="win32"
MY_CPU_TARGET="i386"
compile

MY_LCL_TARGET="gtk2"
MY_OS_TARGET="linux"
MY_CPU_TARGET="i386"
compile

MY_LCL_TARGET="win32"
MY_OS_TARGET="win64"
MY_CPU_TARGET="x86_64"
compile


MY_LCL_TARGET="gtk2"
MY_OS_TARGET="linux"
MY_CPU_TARGET="x86_64"
compile

echo "**********************"
echo "        DONE!"
echo "**********************"



