#!/bin/sh

BUILD_TOP=`echo $1 | sed -e 's#--buildtop=##g'` 
PREFIX=`echo $2 | sed -e 's#--prefix=##g'`
BUILD_URL=`echo $3 | sed -e 's#--buildurl=##g'`

export BUILD_TOP
export PREFIX
export BUILD_URL

# Location of the full source checkout
SOURCE=${BUILD_TOP}/build/build-home
export SOURCE

# The componentset.xml file
COMPONENT_SET=${SOURCE}/MakeWorld/componentset.xml
export COMPONENT_SET

if test -f "${COMPONENT_SET}"; then
 # Get all the names of the packages that are built from cvs
 PACKAGES=`xsltproc ${BUILD_TOP}/starlink/packaging/allcomponents.xsl  ${COMPONENT_SET} | sed 's#^[ \t]*##' | grep '.'`
 export PACKAGES
else
  echo "No ${COMPONENT_SET} file, cannot continue without it!"
  exit 1
fi

#Now build the XML files that contain all the 
#information extracted from the build.

for PACKAGE in $PACKAGES
do

#Now build the XML files that contain all the 
#information extracted from the build.

 ${BUILD_TOP}/starlink/packaging/buildxml.sh --buildtop=${BUILD_TOP} --prefix=${PREFIX} \
 --buildurl=${BUILD_URL} --package=${PACKAGE} \
 > ${BUILD_TOP}/starlink/packaging/logfile

#Create the RPM spec file and EPM list file from the XML files.

 if test -f "${BUILD_TOP}/starlink/packaging/xmlfiles/${PACKAGE}.xml"; then

  xsltproc --novalid ${BUILD_TOP}/starlink/packaging/createrpmspecs.xsl ${BUILD_TOP}/starlink/packaging/xmlfiles/${PACKAGE}.xml \
  > ${BUILD_TOP}/starlink/packaging/rpmspecs/${PACKAGE}.spec

  xsltproc --novalid ${BUILD_TOP}/starlink/packaging/createepmlists.xsl ${BUILD_TOP}/starlink/packaging/xmlfiles/${PACKAGE}.xml \
  > ${BUILD_TOP}/starlink/packaging/epmlists/${PACKAGE}.list

 else

  echo "No ${BUILD_TOP}/starlink/packaging/xmlfiles/${PACKAGE}.xml file, 
        cannot create ${BUILD_TOP}/starlink/packaging/rpmspecs/${PACKAGE}.spec
        without it!"

 fi

done

# Make the tcltk file, we have to do tcl, tk and itcl as
# one spec file.

xsltproc --novalid --stringparam tcl ${BUILD_TOP}/starlink/packaging/xmlfiles/tcl.xml \
--stringparam tk ${BUILD_TOP}/starlink/packaging/xmlfiles/tk.xml \
--stringparam itcl ${BUILD_TOP}/starlink/packaging/xmlfiles/itcl.xml \
${BUILD_TOP}/starlink/packaging/creattclsys.xsl \
${BUILD_TOP}/starlink/packaging/xmlfiles/tcl.xml \
> ${BUILD_TOP}/starlink/packaging/rpmspecs/tclsys.spec

#Package the xml and spec files up.

cd ${BUILD_TOP}/starlink/packaging
tar -czvf ${BUILD_TOP}/build/packages/tars/xmlfiles.tar.gz xmlfiles
tar -czvf ${BUILD_TOP}/build/packages/tars/rpmspecs.tar.gz rpmspecs
