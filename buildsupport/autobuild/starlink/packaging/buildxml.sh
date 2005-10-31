#!/bin/sh
octal(){
	case $1 in
        "---")  echo 0;;
        "--x")  echo 1;;
        "-w-")  echo 2;;
        "-wx")  echo 3;;
        "r--")  echo 4;;
        "r-x")  echo 5;;
        "rw-")  echo 6;;
        "rwx")  echo 7;;
        esac
}

#The function above calculates the octal permission number from
#the rwx letters that are output from unix ls.

#Prefix of the built installation, default setting
#as it is overridden by the command line option
# --prefix.
PREFIX_=/stardev
#The top of the nightly build installation.
#Overridden by the command line option --buildtop.
BUILD_TOP=TOP_OF_BUILD
#A default setting for the package to create.
#Overridden by the command line option --package.
PACKAGE=ast
#The URL for the web output of the nightly 
# build.
BUILD_URL="http://dev.starlink.ac.uk/build/RHEL-WS3-3_i386"
#xsltproc command.
XSLT=/usr/bin/xsltproc
export XSLT
                                                                           
BUILD_TOP=`echo $1 | sed -e 's#--buildtop=##g'` 
PREFIX=`echo $2 | sed -e 's#--prefix=##g'`
BUILD_URL=`echo $3 | sed -e 's#--buildurl=##g'`
PACKAGE=`echo $4 | sed -e 's#--package=##g'`

export BUILD_TOP
export PREFIX
export BUILD_URL
export PACKAGE
export XMLDIR

#derived vars
REAL_PREFIX=${BUILD_TOP}/build/build-root
export REAL_PREFIX

MANIFEST_DIR=${REAL_PREFIX}/manifests
export MANIFEST_DIR

# Location of the full source checkout
SOURCE=${BUILD_TOP}/build/build-home
export SOURCE

# The componentset.xml file
COMPONENT_SET=${SOURCE}/MakeWorld/componentset.xml
export COMPONENT_SET

if test -f "${COMPONENT_SET}"; then

 # Get all the names of the packages that are build from cvs
 PACKAGES=`${XSLT} ${BUILD_TOP}/starlink/packaging/allcomponents.xsl  \
 ${COMPONENT_SET} | sed 's#^[ \t]*##' | grep '.'`
 export PACKAGES

 # The component.xml file for the package, get the package group
 # based on the source location in the CVS tree.
 TEMPVAR1=`${XSLT} --stringparam compname "${PACKAGE}" \
 ${BUILD_TOP}/starlink/packaging/component-location.xsl "${COMPONENT_SET}"`
 GROUP="starlink/"`echo $TEMPVAR1 | cut -d "/" -f1`
 export GROUP

 # The component.xml file for the package, 
 CVSPATH=`${XSLT} --stringparam compname "${PACKAGE}" \
 ${BUILD_TOP}/starlink/packaging/component-location.xsl "${COMPONENT_SET}"`
 export CVSPATH
 END_PATH=`echo ${CVSPATH} | cut -d "/" -f3`
 export END_PATH

 if test "${END_PATH}" = ""; then

 TEMPVAR1=`${XSLT} --stringparam compname "${PACKAGE}" \
 ${BUILD_TOP}/starlink/packaging/component-location.xsl "${COMPONENT_SET}"`
 LOCATION=`echo $TEMPVAR1 | cut -d "/" -f2`
 COMPONENT=${SOURCE}/${LOCATION}/component.xml
 export COMPONENT

 else
 
 TEMPVAR1=`${XSLT} --stringparam compname "${PACKAGE}" \
 ${BUILD_TOP}/starlink/packaging/component-location.xsl "${COMPONENT_SET}"`
 LOCATION=`echo $TEMPVAR1 | cut -d "/" -f3`
 COMPONENT=${SOURCE}/${LOCATION}/component.xml
 export COMPONENT

 fi
 
else
  echo "buildxml - No ${COMPONENT_SET} file, cannot continue without it!"
  exit 1
fi

if test -f "${COMPONENT}"; then
  echo "Found ${COMPONENT} file."
else
  echo "No ${COMPONENT} file, cannot continue without it!"
  exit 1
fi

# XML file with the flat deps
#FLATDEPS=${BUILD_TOP}/starlink/flatdeps.xml
#export FLATDEPS

#if test -f "${FLATDEPS}"; then
#  echo "Found ${FLATDEPS} file."
#else
#  echo "No ${FLATDEPS} file, cannot continue without it!"
#  exit 1
#fi

if test -f "${SOURCE}/MakeWorld/buildsupport/starconf/componentinfo.dtd";
 then
  cp -f ${SOURCE}/MakeWorld/buildsupport/starconf/componentinfo.dtd \
        ${MANIFEST_DIR}/
  cp -f ${SOURCE}/MakeWorld/buildsupport/starconf/componentinfo.dtd .
else
  echo "No ${SOURCE}/MakeWorld/buildsupport/starconf/componentinfo.dtd file!"
fi

# The output xml file
OUTPUT_FILE=${BUILD_TOP}/starlink/packaging/xmlfiles/${PACKAGE}.xml
export OUTPUT_FILE

# Location of the manifests for the package
MANIFEST=${MANIFEST_DIR}/${PACKAGE}
export MANIFEST

if test -f "${MANIFEST}"; then
  #The list of files from the manifests.xsl file
  files=`${XSLT} ${BUILD_TOP}/starlink/packaging/manifests.xsl \
         "${MANIFEST}"`
  
  # The package name from the manifest
  package=`grep "<manifest component=" "${MANIFEST}" | cut -d"'" -f2`
   
  # The package version from the manifest
  version1=`grep "<version>" "${MANIFEST}" | cut -d">" -f2 | cut -d"<" -f1`
  version=`echo ${version1} | sed -e 's#-#.#g'`
  
else
  echo "No ${MANIFEST} file, cannot continue without it!"
  exit 1
fi

# A file shows as - on UNIX ls 
dir_type="-"

# A link shows as l on UNIX ls
link_type="l"

# The date
DATE=`date '+%d%m%Y'`
export DATE

# Write header for the output XML file, the DTD will be componentpack.dtd
echo "<?xml version='1.0'?>" > ${OUTPUT_FILE}
echo "<!DOCTYPE package SYSTEM 'componentpack.dtd'>" >> ${OUTPUT_FILE}

# Write the package name to the XML file
echo "<package component='$package'>" >> ${OUTPUT_FILE}

# Write the version of the package to the XML file
echo "<version>$version</version>" >> ${OUTPUT_FILE}

# Write the group of the package to the XML file
echo "<group>$GROUP</group>" >> ${OUTPUT_FILE}

# Write out the source filename
echo "<source>$package-$version1.tar.gz</source>" >> ${OUTPUT_FILE}

# Write out the prefix 
echo "<prefix>${PREFIX}</prefix>" >> ${OUTPUT_FILE}

# Write out the source file url (package version
# name should only have . for RPM!)
echo "<sourceurl>${BUILD_URL}/dist/$package-$version1.tar.gz</sourceurl>" \
>> ${OUTPUT_FILE}

# Write the Release number to the file
echo "<release>CVS${DATE}</release>" >> ${OUTPUT_FILE}

# Write the License type to the file
#echo "<license>GPL</license>" >> ${OUTPUT_FILE}

# Write the Vendor info to the file
echo "<vendor>Starlink</vendor>" >> ${OUTPUT_FILE}

# Write the Copyright info to the file
echo "<copyright> GPL - Copyright (C) 1999 Council for the Central Laboratory of the Research Councils (CCLRC)</copyright>" \
>> ${OUTPUT_FILE}

# Write the Packager info to the file
echo "<packager>Starlink Software (stardev@jiscmail.ac.uk)</packager>" \
>> ${OUTPUT_FILE}

# Write the Starlink URL info to the file
echo "<url>http://www.starlink.ac.uk</url>" >> ${OUTPUT_FILE}

# Get description etc from the package component.xml file.
${XSLT} --stringparam cpt "${PACKAGE}" \
 --stringparam  componentset "${COMPONENT_SET}" \
 ${BUILD_TOP}/starlink/packaging/componentcopy.xsl ${COMPONENT} \
 | sed 's#^[ \t]*##' | grep '.' >> ${OUTPUT_FILE}

# Start the list of files
echo "<files>" >> ${OUTPUT_FILE}

# We need to get various bits of information about the file
# list, such as type, permissions and if it needs to be parsed
# To relocate we would need the parse info.

for d in $files
do

 #Set type to f if the file $d is a not a directory
 type=`ls -ld $d | cut -d " " -f1 | cut -b1`
 if test "$type" = "$dir_type"; then
  type="f"
 fi
 
 # The real file that the symbolic link links to,
 # with full path.
 if test "$type" = "$link_type"; then
  link_to=`/bin/ls -l $d | awk -F"> " '{ print $NF}'`
 fi
 
 # Get the permissions of the file in rwx form
 user_perm=`ls -ld $d | cut -d " " -f1 | cut -b2-4`
 group_perm=`ls -ld $d | cut -d " " -f1 | cut -b5-7`
 world_perm=`ls -ld $d | cut -d " " -f1 | cut -b8-10`
 
 # Get the mime-type of the file
 mime_type=`file -bi $d | cut -d ";" -f1`

 # Initially we assume that we should parse the file
 # untill we find out otherwise.
 parse=yes

 # Cases of mime-types that we should not parse
 case $mime_type in
  "application/x-sharedlib, not stripped") parse=no;;
  "application/x-executable, for GNU/Linux 2.2.5, dynamically linked (uses shared libs), not stripped") parse=no;;
  "application/x-perl") parse=no;;
  "application/x-archive") parse=no;;
  "application/x-not-regular-file") parse=no;;
  "image/gif") parse=no;;
  "text/html") parse=no;;
  "application/octet-stream") parse=no;;  
 esac

 # Cases of file extensions that we should not parse
 file_extension=`awk 'BEGIN { len=split("'$d'",names,"."); print names[len]; }'`
 case $file_extension in
  "tex") parse=no;;
  "la") parse=no;;
  "css") parse=no;;
 esac

 # Testing, output if the file is to be parsed.
 if test "${parse}" = 'yes'; then
  if `grep "${REAL_PREFIX}" $d > /dev/null 2>&1`; then
   echo "File has the prefix. $d"
  else
   echo "File has not got the prefix. $d"
  parse=no
  fi
 fi
  
 # Find the permissions of the file in octal
 octperm=`octal $user_perm;octal $group_perm;octal $world_perm`
 octperm=`echo $octperm | sed -e 's# ##g'`
 
 # Get file directory
FILE_DIR=`echo $d | sed -e 's#'$REAL_PREFIX/'##g' | awk -F"/" '{ print $1}'`
THE_FILE=`echo $d | sed -e 's#'$REAL_PREFIX/$FILE_DIR/'##g'`
 # Output the file info to the XML output file.
 if test "$type" = "$link_type"; then
 echo "<file mime-type=\""$mime_type"\" \
permissions-string=\""$user_perm$group_perm$world_perm"\" \
permissions-octal=\
\""0""$octperm"\" type=\""$type"\" link=\""$link_to"\" parse=\""$parse"\" dir=\""${FILE_DIR}"\">$THE_FILE</file>" \
>> ${OUTPUT_FILE}
 else
  echo "<file mime-type=\""$mime_type"\" \
permissions-string=\""$user_perm$group_perm$world_perm"\" \
permissions-octal=\
\""0""$octperm"\" type=\""$type"\" parse=\""$parse"\" dir=\""${FILE_DIR}"\">$THE_FILE</file>" \
>> ${OUTPUT_FILE}
 fi
 
done

 # Output the info on the manifest file to the XML output file.
 echo "<file mime-type=\""text/plain\; charset=us-ascii"\" \
permissions-string=\""-rw-r--r--"\" \
permissions-octal=\
\""0644"\" type=\""f"\" parse=\""no"\" dir=\""manifests"\">${PACKAGE}</file>" \
>> ${OUTPUT_FILE}

echo "</files>" >> ${OUTPUT_FILE}
echo "</package>" >> ${OUTPUT_FILE}

rm -f ${MANIFEST_DIR}/componentinfo.dtd
rm -f componentinfo.dtd
