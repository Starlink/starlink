#!/bin/sh

#HOSTNAME
hostname -f


CVS_BUILD_DIR=PREFIX
STARJAVA_DIR=${CVS_BUILD_DIR}/build/build-root/starjava
STARJAVA_SOURCE=${CVS_BUILD_DIR}/build/build-home/starjava
STARLINK=${CVS_BUILD_DIR}/build/build-root
BUILD_URL="URL_TO_BUILD_INDEX"
SUN_JDK=JDK_HOME
PATH=${SUN_JDK}/bin:${PATH}
export PATH
BUILD_DATE=`date +%d%m%y`
export BUILD_DATE

echo "CVS BUILD ON ${BUILD_DATE} FOR SYSTEM_NAME WITH SUN JDK" > ${STARJAVA_DIR}/version

tar -cvf ${CVS_BUILD_DIR}/build/temp/starjava-cvs-${BUILD_DATE}.tar -C ${STARLINK} starjava > /dev/null 2>&1


gzip -9 ${CVS_BUILD_DIR}/build/temp/starjava-cvs-${BUILD_DATE}.tar > /dev/null 2>&1

mv ${CVS_BUILD_DIR}/build/temp/starjava-cvs-${BUILD_DATE}.tar.gz ${CVS_BUILD_DIR}/build/packages/tars/


cp -fr ${STARJAVA_DIR}/* ${CVS_BUILD_DIR}/build/public_html/webstart


cd ${CVS_BUILD_DIR}/build/public_html/webstart/lib

alljnlp=`ls -1 *.jnlp`
 
for name in ${alljnlp}; do
sed 's#codebase="http://www.starlink.ac.uk/starjava/lib#codebase="'${BUILD_URL}'/webstart/lib#g' ${name} > ${name}.tmp
sed 's#starlink_logo_med.gif#Starlink.gif#g' ${name}.tmp > ${name}.tmp2
mv -f ${name}.tmp2 ${name}
rm -f ${name}.tmp
done

cp -f ${CVS_BUILD_DIR}/starlink/Starlink.gif ${CVS_BUILD_DIR}/build/public_html/webstart/lib/

jarsigner -keystore ${STARJAVA_SOURCE}/source/keystore -storepass Majikthise -keypass Vroomfondel ant.jar  Starlink-UK




