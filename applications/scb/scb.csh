#+
#  Name:
#     scb.csh
#
#  Purpose:
#     Start the SCB system from the Unix shell.
#
#  Type of module:
#     C shell script
#
#  Invocation:
#     source scb.csh
#
#  Description:
#     This procedure starts the SCB system for use from the C shell by 
#     setting aliases for the commands it provides and setting up some 
#     environment variables.
#
#  Authors:
#     MBT: Mark Taylor (STARLINK)
#
#  History:
#     08-DEC-1999 (MBT):
#        Initial revision.
#-

#  The following definitions allow the file to run without having been
#  edited by the install process without causing an error.  This mode of
#  use should not however be necessary under normal circumstances.
      if ( ! $?STARLINK ) set STARLINK = "/star"
      if ( ! $?INSTALL ) set INSTALL = $STARLINK
      set SCB_INDEX = "$INSTALL/etc/scb"
      set SCB_SOURCES = "$STARLINK/sources"
      set SCB_BROWSER_TMP = "/usr/tmp/scb"
      set SCB_INDEXER_TMP = "/usr/tmp/scbindex"
      set HTX_SERVER = "http://star-www.rl.ac.uk/cgi-bin/htxserver/"
      set SCB_DIR = "$INSTALL/bin/scb"
      set PKG_VERS = '[version]'
 
#  The following environment variable assignments are edited by the install 
#  process.
      setenv STARLINK "$STARLINK"
      setenv SCB_INDEX "$SCB_INDEX"
      setenv SCB_SOURCES "$SCB_SOURCES"
      setenv SCB_BROWSER_TMP "$SCB_BROWSER_TMP"
      setenv SCB_INDEXER_TMP "$SCB_INDEXER_TMP"
      setenv HTX_SERVER "$HTX_SERVER"
      setenv SCB_DIR "$SCB_DIR"

#  The following version string setting is edited by the install process.
      set PKG_VERS = "$PKG_VERS"

#  We now unset the C-shell variables which would otherwise mask the
#  environment variables.
      unset STARLINK
      unset INSTALL
      unset SCB_INDEX
      unset SCB_SOURCES
      unset SCB_BROWSER_TMP
      unset SCB_INDEXER_TMP
      unset HTX_SERVER
      unset SCB_DIR

#  Now set the aliases for the commands.
      alias scb '$SCB_DIR/scb.pl'
      alias scbindex '$SCB_DIR/scbindex.pl'
      alias dbmcat '$SCB_DIR/dbmcat.pl'
      alias scbcp '$SCB_DIR/scbcp.pl'
      alias ctag '$SCB_DIR/ctag'
      alias ftag '$SCB_DIR/ftag'

#  Indicate that the SCB commands are available.
#  This startup message is a bit on the verbose side, but you might want
#  different settings of the environment variables for different 
#  occasions, so it's as well to be reminded of the values of the more
#  important ones.
      echo " "
      echo "   Source Code Browser (SCB) version $PKG_VERS.  See SUN/225 for help."
      echo " "
      echo "   Environment variable settings:"
      echo "      SCB_INDEX         $SCB_INDEX"
      echo "      SCB_SOURCES       $SCB_SOURCES"
      echo "      SCB_BROWSER_TMP   $SCB_BROWSER_TMP"
      echo "      SCB_INDEXER_TMP   $SCB_INDEXER_TMP"
      echo " "

# $Id$
