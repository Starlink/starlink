#!/bin/sh

#Get the Perl version and set PERL5LIB.

PERLVERSION=`perl -V:version | cut -d "'" -f2`
PERL5LIB=PREFIX/lib/perl5/site_perl/$PERLVERSION:$PERL5LIB
export PERL5LIB

#The CVS root dir, you should set MYCVSROOT to your CVSROOT dir,
#do not rely on the CVSROOT environment variable being picked up
#and set when this script is run.

MYCVSROOT=$CVSROOT

#Checkout componentset.xml so that we can create autobuild.conf.

cd PREFIX/starlink

cvs -d $MYCVSROOT co componentset.xml

#Use XSLT script to create autobuild.conf and remove
#the starting XML line.

xsltproc autobuild.xslt componentset.xml > autobuild.conf
sed '1,1d' autobuild.conf > autobuild.conf-tmp
rm -f autobuild.conf
mv autobuild.conf-tmp autobuild.conf

#Remove the files from the previous build so we start a fresh

rm -fr PREFIX/build/build-cache/*
rm -fr PREFIX/build/build-home/*
rm -fr PREFIX/build/build-root/*
rm -fr PREFIX/build/install/*
rm -fr PREFIX/build/temp/*
rm -fr PREFIX/build/packages/tars/*

#Run the build

PREFIX/bin/auto-build.pl --conf PREFIX/starlink/autobuild.conf
