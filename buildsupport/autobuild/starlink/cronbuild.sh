#!/bin/sh

#Get the Perl version and set PERL5LIB.

PERLVERSION=`perl -V:version | cut -d "'" -f2`
PERL5LIB=PREFIX/lib/perl5/site_perl/$PERLVERSION:$PERL5LIB
export PERL5LIB

#The CVS root dir, you should set MYCVSROOT to your CVSROOT dir,
#do not rely on the CVSROOT environment variable being picked up
#and set when this script is run.

#We need java to run Normans stuff.
JAVA=JDK_HOME/bin/java
export JAVA

MYCVSROOT=$CVSROOT

BUILD_MYTOP=PREFIX
export BUILD_MYTOP

#Checkout componentset.xml so that we can create autobuild.conf.

cd ${BUILD_MYTOP}/starlink
rm -fr componentset.xml CVS flatdeps.xml

cvs -d $MYCVSROOT co componentset.xml

#Use XSLT script to create autobuild.conf and remove
#the starting XML line.

#Use Normans java stuff to calculate the flatdeps.xml
cd ${BUILD_MYTOP}/starlink/javacalc/java
rm -f componentset.xml
cp ${BUILD_MYTOP}/starlink/componentset.xml .
${JAVA} GenerateDependencies --flatdeps=flatdeps.xml componentset.xml
cp -f flatdeps.xml ../../
cd ../../

#Now build the main part of the autobuild.conf file.
xsltproc autobuild.xslt componentset.xml > top.conf

#Use if you want to run the tests.
##xsltproc test.xsl componentset.xml > test.conf
##cat top.conf test.conf tail.txt > autobuild.conf

#Use if you do not want to run the tests
cat top.conf tail.txt > autobuild.conf

#Make a backup of autobuild.conf
cp autobuild.conf autobuild.conf-`date +%d%m%y`
chmod 644 autobuild.conf-`date +%d%m%y`


#Remove the files from the previous build so we start a fresh

rm -fr ${BUILD_MYTOP}/build/build-cache/*
rm -fr ${BUILD_MYTOP}/build/build-home/*
rm -fr ${BUILD_MYTOP}/build/build-root/*
rm -fr ${BUILD_MYTOP}/build/install/*
rm -fr ${BUILD_MYTOP}/build/install-comp/*
rm -fr ${BUILD_MYTOP}/build/temp/*
rm -fr ${BUILD_MYTOP}/build/packages/tars/*
rm -fr ${BUILD_MYTOP}/build/public_html/webstart/*
rm -fr ${BUILD_MYTOP}/build/public_html/webstart15/*
mkdir ${BUILD_MYTOP}/build/packages/tars/starjava15

#Run the build

${BUILD_MYTOP}/bin/auto-build.pl --conf ${BUILD_MYTOP}/starlink/autobuild.conf

