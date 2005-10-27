#!/bin/sh
#Build a fresh copy of the Starlink STARJAVA CVS
#

#HOSTNAME
hostname -f


NAME="Starlink CVS"
  
# Exit immediately if command fails
set -e
  
# Print command executed to stdout
set -v


CVS_BUILD_DIR=PREFIX
cp -f ${CVS_BUILD_DIR}/starlink/stardev.properties15 ~/.stardev.properties
STARJAVA_SOURCE=${CVS_BUILD_DIR}/build/build-home/starjava15
STARJAVA_DIR=${CVS_BUILD_DIR}/build/build-root/starjava15
SUN_JDK=JDK_HOME
STAR_JAVA=${SUN_JDK}/bin/java
DISPLAY=EXPORT_DISPLAY
STARLINK=${CVS_BUILD_DIR}/build/build-root
STAR_DIR=${STARLINK}

export STARLINK


ANT_OPTS="-Djava.awt.headless=true"
export ANT_OPTS

export STAR_JAVA
export DISPLAY
#Set path install to Classic Starlink stuff

export PATH=${STAR_DIR}/bin:${PATH}

export SYSTEM=ix86_Linux

#Now build the cvs tree

ANT_BIN_PATH=${STARJAVA_SOURCE}/source/ant/bin:${SUN_JDK}/bin:/usr/bin:${PATH}
export PATH=${ANT_BIN_PATH}
echo ${PATH}
cd ${STARJAVA_SOURCE}/source/ant

ant install 
ant clean 

cd ../../

export PATH=${STARJAVA_DIR}/bin:${PATH}
which ant
cd source
#Sign ANT!
ant sign_ant 

#compiles the system
ant build 

#build the native jniast shared libs
cd jniast

ant build-native

#build the native jnihds shared libs

cd ../jnihds

ant build-native

cd ../

#creates packages API documentation
ant javadocs 

#creates the complete API documentation
ant javadocs-full 

#installs the system and documentation
ant install 

#test the system
ant test 

#Keep a copy of the STARJAVA install for distribution.
#create webstart.

${CVS_BUILD_DIR}/starlink/starjava-dist15.sh

#create full export distributions of packages
#ant export 

#exports monolithic runtime distributions
#ant export-full-built 

#exports monolithic javadocs archive
#ant export-full-docs 

#exports all monolithic archives
#ant export-full 

#cleans up the build
ant clean 

#deinstalls the system
ant deinstall 

#exports full source archives
#ant export-full-source





