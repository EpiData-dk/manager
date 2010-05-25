#!/bin/bash
# Compile all versions of manager!

## Changeable options.
MANAGER_OPTIONS="-Schi -WG"
#SIMPLEOPTS="$SIMPLEOPTS -veiwd"
#SIMPLEOPTS="$SIMPLEOPTS -gl -g -dEPI_DEBUG"
MANAGERFILENAME="epidatamanager"

###########################################
#  DO NOT EDIT				  #
###########################################
SVN_LOC=""
LOCALIZATIONS_FILE="buildall.rc"
. "$LOCALIZATIONS_FILE"

MANAGER_OPTIONS="$MANAGER_OPTIONS $COMMON_OPTIONS"

build_package() {
  HERE=`pwd`
  cd $PACKAGE_LOCATION
  make all CPU_TARGET="$MY_CPU_TARGET" OS_TARGET="$MY_OS_TARGET" OPT="$PACKAGE_OPTIONS" > /dev/null
  cd "$HERE"
}

build_all_packages() {
  PACKAGE_LOCATION=$FPSPREADSHEET_LOCATION
  PACKAGE_OPTIONS=$FPSPREADSHEET_OPTIONS
  build_package
  PACKAGE_LOCATION=$DCPCRYPT_LOCATION
  PACKAGE_OPTIONS=$DCPCRYPT_OPTIONS
  build_package
  PACKAGE_LOCATION=$CORE_LOCATION
  PACKAGE_OPTIONS=$CORE_OPTIONS
  build_package
}

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
    ln -sf "$FILENAME" "$MANAGERFILENAME.$MY_OS_TARGET.exe"
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

compile() {
  echo "Compiling for: $MY_CPU_TARGET-$MY_OS_TARGET ($MY_LCL_TARGET)"
  echo "------------------------------"
  clean
  build_all_packages

  SIMPLEOPTS="$SIMPLEOPTS -dEPI_RELEASE"
  CPU="-P$MY_CPU_TARGET"
  TARGET="-T$MY_OS_TARGET"

  mkdir -p "$MANAGER_LOCATION/lib/$MY_CPU_TARGET-$MY_OS_TARGET"
  # Manager dirs
  MANAGER_OUTPUT="-FU$MANAGER_LOCATION/lib/$MY_CPU_TARGET-$MY_OS_TARGET/"
  MANAGER_UNITS="-Fu$MANAGER_LOCATION/editor -Fu$MANAGER_LOCATION/designer -Fu$MANAGER_LOCATION -Fu."
  MANAGER_INCLUDES="-Fi$MANAGER_LOCATION/lib/$MY_CPU_TARGET-$MY_OS_TARGET"
  MANAGER_DIRS="$MANAGER_OUTPUT $MANAGER_UNITS $MANAGER_INCLUDES"
  
  # Packages dirs
  CORE_UNITS="-Fu$CORE_LOCATION/lib/$MY_CPU_TARGET-$MY_OS_TARGET/"
  FPSPREAD_UNITS="-Fu$FPSPREADSHEET_LOCATION/lib/$MY_CPU_TARGET-$MY_OS_TARGET/"
  CRYPT_UNITS="-Fu$DCPCRYPT_LOCATION/lib/$MY_CPU_TARGET-$MY_OS_TARGET/"
  PACKAGES_DIRS="$CORE_UNITS $FPSPREAD_UNITS $CRYPT_UNITS"

  # Lazarus dirs
  LCL_UNITS="-Fu$LAZARUS_LOCATION/lcl/units/$MY_CPU_TARGET-$MY_OS_TARGET/ -Fu$LAZARUS_LOCATION/lcl/units/$MY_CPU_TARGET-$MY_OS_TARGET/$MY_LCL_TARGET/"
  FCL_UNITS="-Fu$LAZARUS_LOCATION/packager/units/$MY_CPU_TARGET-$MY_OS_TARGET/"
  SYNEDIT_UNITS="-Fu$LAZARUS_LOCATION/components/synedit/units/$MY_CPU_TARGET-$MY_OS_TARGET/"
  LAZARUS_DIRS="$LCL_UNITS $FCL_UNITS $SYNEDIT_UNITS"
  
  get_filename
  OUTPUTNAME="-o$FILENAME"
  DEFINEOPTS="-dLCL -d$MY_LCL_TARGET"

#  echo "fpc $CPU $TARGET $SIMPLEOPTS $MANAGER_OPTS $LAZARUS_UNITS $OUTPUTNAME $DEFINEOPTS epidatamanager.lpr"
  fpc $CPU $TARGET $MANAGER_OPTIONS $MANAGER_DIRS $PACKAGES_DIRS $LAZARUS_DIRS  $OUTPUTNAME $DEFINEOPTS epidatamanager.lpr 

  ZIP_NAME=$MANAGERFILENAME.$MANAGER_VERSION
  echo "$ZIP_NAME"
  case $MY_LCL_TARGET in
    win32)
      FINALZIP_NAME=$ZIP_NAME.$MY_OS_TARGET.zip
      zip $FINALZIP_NAME $FILENAME
    	 ;;
    gtk2)
      case $MY_CPU_TARGET in
        x86_64)
          POSTFIX="linux64"
          ;;
        i386)
          POSTFIX="linux32"
          ;;
      esac
      FINALZIP_NAME=$ZIP_NAME.$POSTFIX.tgz
      tar -zcf $FINALZIP_NAME $FILENAME
         ;;
    *)
     ;;
  esac
  mv $FINALZIP_NAME "/home/torsten/epiexec"		

  echo ""
}

get_version_info() {
  V1=`cat epidatamanager.lpi | grep MajorVersionNr | cut -c 30-30`
  if [ -z $V1 ]
  then
    V1="0"
  fi

  V2=`cat epidatamanager.lpi | grep MinorVersionNr | cut -c 30-30`
  if [ -z $V2 ]
  then
    V2="0"
  fi

  V3=`cat epidatamanager.lpi | grep RevisionNr | cut -c 26-26`
  if [ -z $V3 ]
  then
    V3="0"
  fi

  V4=`cat epidatamanager.lpi | grep BuildNr | cut -c 23-23`
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

SVN2REV_CMD="./update_revision.sh"
$SVN2REV_CMD

get_version_info

echo "**********************"
echo " Start compiling..."
echo " Manager version: $MANAGER_VERSION"
echo "**********************"

for TARGET in $ALL_TARGETS ; do {
  MY_CPU_TARGET=`echo "$TARGET" | cut -f 1-1 -d '-'`
  MY_OS_TARGET=`echo $TARGET | cut -f 2-2 -d '-'`
  MY_LCL_TARGET=`echo $TARGET | cut -f 3-3 -d '-'`
  compile
} ; done

echo "**********************"
echo "        DONE!"
echo "**********************"

