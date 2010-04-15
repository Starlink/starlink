#! /bin/csh -f
#+
# catremote-setup.csh
#
# Example shell script to setup the environment variables used by
# catremote.  Note that this file should be sourced.
#
# Author:
#   A C Davenhall (Edinburgh)
# History:
#   23/5/01 (ACD): Original version.
#-

#
#  Define the utility program to be used to submit the query.  The options
#  are:
#    "/star/bin/cursa/geturl" -- geturl utility,
#    "lynx -source"           -- lynx command line browser,
#    "java  UrlReader"        -- Java utility.
#    "wget -q -O -"           -- wget utility (probably RedHat Linux only).

setenv  CATREM_URLREADER "/home/acd/starbase/cursa/catremote/geturl/geturl"

#
#  Specify the configuration file to be used.  The URL given here is the
#  the CURSA default.

setenv  CATREM_CONFIG "http://dev.starlink.ac.uk/~pwd/catremote/cursa.cfg"
#
#  Define the maximum number of objects which may be included in the
#  returned table.

setenv  CATREM_MAXOBJ  200

#
#  Specify whether the URL constituting the query is echoed to the
#  command line.  The options re:
#    no   -  do not echo the URL (default),
#    yes  -  echo the URL.

setenv  CATREM_ECHOURL no

#
#  Set the Java CLASSPATH environment variable to pick up the URLreader
#  (note that CLASSPATH has to be set rather than setting the corresponding
#  command line option because the latter does not work on alphas).

setenv CLASSPATH /home/acd/starbase/cursa/catremote:/usr/lib/netscape/java/classes
