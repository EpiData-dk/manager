#!/bin/bash
# Compile all versions of manager!

## Changeable options.
## Dont forget to update version no.!
MANAGER_VERSION="0.2.1.5"
SIMPLEOPTS="-MObjFPC -Sci -Xs -CX -Ci -WG -O3 -l"
MANAGERFILENAME="epidatamanager"

# Paths - change to your location
CORE_LOCATION="/home/torsten/EpiData/Core/trunk"
FPSPREASHEET_LOCATION="/home/torsten/FreePascal/fpspreadsheet"
LAZARUS_LOCATION="/home/torsten/FreePascal/lazarus"

get_filename(){
  FILENAME="$MANAGERFILENAME.$MANAGER_VERSION.$MY_CPU_TARGET-$MY_OS_TARGET"
  if [ $MY_LCL_TARGET = "win32" ]
  then
    FILENAME="$FILENAME.exe"
  fi
}

clean() {
  get_filename
  if [ -a "$MANAGER_LOCATION/$FILENAME" ] 
  then
    rm -f "$MANAGER_LOCATION/$FILENAME"
  fi

  if [ -d "$MANAGER_LOCATION/lib/$MY_CPU_TARGET-$MY_OS_TARGET" ]
  then
    rm -fr "$MANAGER_LOCATION/lib/$MY_CPU_TARGET-$MY_OS_TARGET"
  fi
}

compile() {
  echo "Compiling for: $MY_CPU_TARGET-$MY_OS_TARGET"

  clean

  CPU="-P$MY_CPU_TARGET"
  TARGET="-T$MY_OS_TARGET"

  mkdir -p "$MANAGER_LOCATION/lib/$MY_CPU_TARGET-$MY_OS_TARGET"
  MANAGER_UNITS="-FU$MANAGER_LOCATION/lib/$MY_CPU_TARGET-$MY_OS_TARGET/ -Fu$MANAGER_LOCATION -Fu."
  MANAGER_INCLUDES="-Fi$MANAGER_LOCATION/lib/$MY_CPU_TARGET-$MY_OS_TARGET"
  CORE_UNITS="-Fu$CORE_LOCATION/lib/$MY_CPU_TARGET-$MY_OS_TARGET/"
  FPSPREAD_UNITS="-Fu$FPSPREASHEET_LOCATION/lib/$MY_CPU_TARGET-$MY_OS_TARGET/"
  OWN_UNITS="$MANAGER_UNITS $MANAGER_INCLUDES $CORE_UNITS $FPSPREAD_UNITS"

  LCL_UNITS="-Fu$LAZARUS_LOCATION/lcl/units/$MY_CPU_TARGET-$MY_OS_TARGET/ -Fu$LAZARUS_LOCATION/lcl/units/$MY_CPU_TARGET-$MY_OS_TARGET/$MY_LCL_TARGET/"
  LAZPACK_UNITS="-Fu$LAZARUS_LOCATION/packager/units/$MY_CPU_TARGET-$MY_OS_TARGET/"
  LAZARUS_UNITS="$LCL_UNITS $LAZPACK_UNITS"
  
  get_filename
  OUTPUTNAME="-o$FILENAME"
  DEFINEOPTS="-dLCL -d$MY_LCL_TARGET"

#  echo "fpc $CPU $TARGET $SIMPLEOPTS $OWN_UNITS $LAZARUS_UNITS $OUTPUTNAME $DEFINEOPTS epidatamanager.lpr"
  fpc $CPU $TARGET $SIMPLEOPTS $OWN_UNITS $LAZARUS_UNITS $OUTPUTNAME $DEFINEOPTS epidatamanager.lpr > /dev/null
}

# Easy paths:
MANAGER_LOCATION=`pwd`

echo "**********************"
echo " Start compiling..."
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



