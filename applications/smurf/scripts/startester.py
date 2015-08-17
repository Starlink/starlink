#!/usr/bin/env python

'''
*+
*  Name:
*     startester

*  Purpose:
*     Perform regression tests to verify changes to starlink software.

*  Language:
*     python (2.7 or 3.*)

*  Description:
*     This script runs one or more tests using starlink software to
*     generate output NDFs from specified input NDFs. The output NDFs
*     are compared to previously generated and verified reference NDFs,
*     and any differences between them are reported. Any reported
*     differences should be investigated to determine if the new NDFs
*     represent an improvement over the reference NDFs (in which case the
*     reference NDFs should be updated - see parameter UPDATE), or if the
*     differences represent some error in the generating software (in
*     which case the error should be fixed and the tests re-run).
*
*     If parameter SUMMARY is set TRUE, then a summary of the successes
*     and failures of the previous run (in the same ROOTDIR) is
*     displayed, but no further tests are performed.

*  Test Definition Files:
*     The nature of the tests to be performed, the reference results and
*     all other files related to the tests should reside in a specified
*     directory tree (see parameter ROOTDIR). The details of each test to
*     be performed are specified by one or more "test definition files",
*     which should reside in the top level of the ROOTDIR directory tree.
*     These files should have suffix ".tdf". All TDF files present in
*     the specified ROOTDIR are processed sequentially when startester
*     is run.
*
*     Each TDF file has the format of a TOPCAT "ASCII" catalogue. This
*     format is documented in section "Supported Input Formats/ASCII"
*     of www.star.bris.ac.uk/~mbt/topcat/sun253/sun253.html. It
*     consists of an initial header consisting of lines that start
*     with a "#"character, followed by a table of values in which each
*     row defines a single test to be performed. The header must
*     contain a line of the form "# type = <test type>", which defines
*     the nature of the tests defined within the file (all tests in a
*     single TDF file must be of the same type). Here, "<test type>"
*     must be one of the recognised test types listed in section "Test
*     Types:" below (case insensitive). The "#" character starting the
*     final line in the header should be followed by a space separated
*     list of column names. The column names must match those required by
*     the specified test type, although they may appear in any order. All
*     other header lines are ignored (except for any extra header lines
*     required by specific test type) and may be used to document the TDF
*     file. Each row within the table following the header can include the
*     word "null" (unquoted) to indicate that no value is avalaible for
*     a particular column. If a column value need to include spaces, then
*     the whole column value should be quoted (single or double). Literal
*     quotes  may be included within quoted strings by escaping them
*     using a backslash character ("\").

*  Reference NDFs:
*     Each test should generate one or more output NDFs. These are
*     compared with existing reference NDFs, and any differences between
*     the new NDFs and the corresponding reference NDFs are reported.
*     The reference NDFs used by a particular TDF file are stored in
*     a subdirectory with path "<rootdir>/<tdf>/test_<id>/ref", where
*     "<rootdir>" is the directory specified by parameter ROOTDIR, "<tdf>"
*     is the basename of the TDF file and "<id>" is the value of the ID
*     column for the particular test in question. If no reference NDFs exist
*     for a specified test, then the newly generated NDFs are simply copied
*     into the correct subdirectory and no comparisons are made (the
*     subdirectory is created if it does not already exist).

*  Reporting the Results:
*     The results of the tests are reported in two ways - to the screen
*     and to textual report files. The content of the screen output is
*     controlled by parameter ILEVEL, and indicates which tests passed
*     and which failed. It does not however, include information about
*     the specific differences that caused any test to fail. Such
*     information is instead written to a "report file" - one for each
*     test. Each report file is called "report" and is placed in a
*     subdirectory with path "<rootdir>/<tdf>/test_<id>", where "<rootdir>"
*     is the directory specified by parameter ROOTDIR, "<tdf>" is the
*     basename of the TDF file and "<id>" is the value of the ID column
*     for the particular test in question. A report file is generated
*     only if the test fails.
*
*     Any text written to standard output whilst running a test is stored
*     in a file called "log", again stored in "<rootdir>/<tdf>/test_<id>".
*
*     If any test fails, the new NDFs that have changed are stored in a
*     directory called "<rootdir>/<tdf>/test_<id>/failures".

*     If any test - from any TDF file - fails, then the output parameter
*     SUCCESS is set FALSE. If all tests are passed, then SECCESS is set
*     to TRUE.

*  Accepting Changes:
*     If a comparison indicates that one or more NDFs generated by a test
*     differ from the reference NDF, then it may be appropriate to delete
*     the original reference NDFs and use the new ones in their place
*     (e.g. if the new NDFs are in some sense "better" than the old
*     reference NDFs). To simplify this procedure, the UPDATE parameter
*     may be set to a non-null value. Options are available to update
*     specified tests, or to update all tests that failed.

*  Test Types:
*     The startester script can support different types of tests. The
*     basic concept of a test of any type is that it takes zero or more
*     input files, which may reside anywhere, and generates one or more
*     output NDFs. Each individual test is defined by the values in a
*     single row of a TDF file. The columns that are required in a TDF
*     file depend on the specific sub-type of tests included in the file
*     (and specified by the "# type = <type>" line in the header of the
*     file). The list of currently supported test types is:
*
*     "type=shell": Shell tests generate the output NDFs by running an
*     arbitrary shell command under a standard POSIX shell. An extra
*     header line is required (in addition to the "# type = shell"
*     line) of the form "# compare = <value>", where value is either
*     "ndfcompare" or "sc2compare" (without the quotes). This indicates
*     the command that is to be used to compare each output NDF with the
*     corresponding reference NDF. The following columns are required in
*     the TDF file:
*
*        - "ID": A single-word identifier for the test. All the ID values
*        in a single TDF file should be unique. IDs are case insensitive.
*
*        - "OUT": A GRP group expression listing the output NDFs generated
*        by the test, relative to its current working directory ("./").
*        These are the NDFs that are compared against the reference NDFs.
*
*        - "CMD": The command to be executed.
*
*     "type=makemap": MAKEMAP tests generate a single output NDF ("called
*     "map.sdf") by running the SMURF:MAKEMAP command. Note, this type of
*     test cannot be used for generating tiled maps (see "type=makemap-jsa"
*     instead). The SMURF:SC2COMPARE command is used to compare the output
*     NDF to the corresponding reference NDF. The header should include an
*     extra line (in addition to the "# type = makemap" line) of the form
*     "# sc2data = <path>" where "<path>" is the absolute path to a directory
*     containing raw SCUBA-2 data. The specified directory should contain a
*     subdirectory for each SCUBA-2 array, named "s8a", "s8b", etc. Each of
*     these subdirectory should contain a subdirectory for each UT date, which
*     in turn should contain subdirectories for each observation. The following
*     columns are required in the TDF file:
*
*        - "ID": A single-word identifier for the test. All the ID values
*        in a single TDF file should be unique. IDs are case insensitive.
*
*        - "OBS": A comma-separated list of observations to be supplied as
*        input to MAKEMAP. Each obseravtion is specified in the form
*        "ut/obs", where "ut" is a UT date in "yyymmdd" format, and "obs" is
*        an observation number.
*
*        - "ARRAYS": The SCUBA-2 sub-arrays to use. This may be "s8" for all
*        850 um arrays, "s4" for all 450 um arrays, or a comma separated list
*        of individual array names (e.g. "s8a,s8c")
*
*        - "CONFIG": The name of a standard config file, or "null". If a
*        name is supplied, it must be the name (without any path or leading "^"
*        character) of one of the makemap config files supplied with smurf.
*        If "null" is supplied, the PARAMS column should include a setting
*        for the CONFIG parameter.
*
*        - "PARAMS": Any extra parameter settings that are to be used when
*        running MAKEMAP. This list should not include the IN, OUT, JSATILES
*        or TILEDIMS parameter. The CONFIG parameter may be included only
*        if the value in column CONFIG is "null".
*
*     "type=makemap_jsa": MAKEMAP_JSA tests are like MAKEMAP tests except that
*     they run MAKEMAP with the JSATILES=YES parameter set. Each one generates
*     a set of output NDFs of the form "tile_xxx.sdf" , each corresponding to
*     a single JSA tile. An extra column is required (over and above those
*     required by the basic makemap test described above):
*
*        - "TILES": A comma-separated list of integer JSA tile identifiers.
*        These are the tiles for which output NDFs are generated by the test.

*  Usage:
*     startester rootdir

*  Parameters:
*     GLEVEL = LITERAL (Read)
*        Controls the level of information to write to a text log file.
*        Allowed values are as for "ILEVEL". The log file to create is
*        specified via parameter "LOGFILE. ["NONE"]
*     ILEVEL = LITERAL (Read)
*        Controls the level of information displayed on the screen by the
*        script. It can take any of the following values (note, these values
*        are purposefully different to the SUN/104 values to avoid confusion
*        in their effects):
*
*        - "NONE": No screen output is created
*
*        - "CRITICAL": Only test failures and other critical messages are
*        displayed. Test successes are not reported. So if all tests are
*        passed, no screen output is generated.
*
*        - "PROGRESS": Extra messages indicating test success are also
*        displayed.
*
*        - "ATASK": Extra messages are also displayed describing each atask
*        invocation. Lines starting with ">>>" indicate the command name
*        and parameter values, and subsequent lines hold the screen output
*        generated by the command.
*
*        - "DEBUG": Extra messages are also displayed containing unspecified
*        debugging information. In addition scatter plots showing how each Q
*        and U image compares to the mean Q and U image are displayed at this
*        ILEVEL.
*
*        ["PROGRESS"]
*     LOGFILE = LITERAL (Read)
*        The name of the log file to create if GLEVEL is not NONE. The
*        default is "<command>.log", where <command> is the name of the
*        executing script (minus any trailing ".py" suffix), and will be
*        created in the current directory. Any file with the same name is
*        over-written. []
*     MSG_FILTER = LITERAL (Read)
*        Controls the default level of information reported by Starlink
*        atasks invoked within the executing script. This default can be
*        over-ridden by including a value for the msg_filter parameter
*        within the command string passed to the "invoke" function. The
*        accepted values are the list defined in SUN/104 ("None", "Quiet",
*        "Normal", "Verbose", etc). ["Normal"]
*     OK = _LOGICAL (Read)
*        Only used if parameter UPDATE is set to "Prompt". It is used to
*        determine if each nominated test should be updated or not.
*     ROOTDIR = LITERAL (Read)
*        The path to the root directory in which information about the
*        tests to be performed is stored. The directory should contain
*        one or more TDF files, as described in earlier sections of this
*        prologue.
*     SUCCESS = _LOGICAL (Write)
*        An output parameter that is set TRUE if all tests are passed,
*        and FALSE otherwise.
*     SUMMARY = _LOGICAL (Write)
*        If set TRUE, then a summary of the successes and failures of
*        the previous run (in the same ROOTDIR) is displayed, but no
*        further tests are performed. [FALSE]
*     UPDATE = LITERAL (Read)
*        Only used if SUMMARY is FALSE. If a non-null list of tests is
*        supplied for UPDATE, no tests are performed. Instead, the most
*        recently generated output NDFs for the specified tests are copied
*        so that they are used as the corresponding reference NDFs on the
*        next invocation of startester. This replaces any previously
*        existing reference NDFs. The supplied value for UPDATE can be a
*        comma separated list of test identifiers, the world "All" or the
*        word "Prompt". A test identifier consists of the basename of the
*        TDF file followed by "_<id>", where "<id>" is the test identifier
*        as specified in the "ID" column of the TDF file. Any tests that
*        did not fail on the previous invocation of startester are ignored.
*        Specifying "All" causes all failed tests to be updated. Specifying
*        "Prompt" causes the user to be asked whether or not to update each
*        failed test (see parameter OK). [!]

*  Copyright:
*     Copyright (C) 2015 East Asian Observatory.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either Version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
*     02110-1301, USA.

*  Authors:
*     DSB: David S. Berry (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     20-MAY-2015 (DSB):
*        Original version
*-
'''

import shutil
import os
import glob
import re
import abc
import starutil
from starutil import invoke
from starutil import NDG
from starutil import Parameter
from starutil import ParSys
from starutil import msg_out
from starutil import AtaskError

#  A function to clean up before exiting. Delete all temporary NDFs etc.
#  Also delete the script's temporary ADAM directory.
def cleanup():
   ParSys.cleanup()
   NDG.cleanup()

#  A function to split a line of text into words using the TOPCAT ASCII
#  format quoting scheme.
def splitter( line, iline, tdfpath ):
   result = []
   inWord = False
   quoted = None
   escaped = False
   ic = 0
   for c in line:
      if inWord:
         if not quoted:
            if c.isspace():
               inWord = False
               result.append( word )
            else:
               word += c
         else:
            if escaped:
               word += c
               escaped = False
            elif c == '\\':
               escaped = True
            elif c == quoted:
               inWord = False
               result.append( word )
            else:
               word += c

      elif not c.isspace():
         if c == "'" or c == '"':
            quoted = c
            word = ""
         else:
            quoted = None
            word = c
         inWord = True

      ic += 1

   if inWord:
      if quoted:
         raise TDFError("Bad TDF file '{0}': Closing quote missing at end "
                        "of line {1}:\n{2}".format(tdfpath,iline,line) )
      else:
         result.append( word )

   return result



#  An exception raise if an error is found within a TDF file.
#  ==========================================================
class TDFError(Exception):
   pass



#  The abstract base class from which all classes of test are derived.
#  ===================================================================
#  All classes of testset have and "ID" column and an "OUT" column. Some
#  classes of testset may generate the "OUT" column from other columns
#  before invoking the _verify() method, and so do not require the supplied
#  TDF file to contain an OUT column.
class TestSet(object):
   __metaclass__ = abc.ABCMeta

#  -----------
#  Initialiser
#  -----------
   @abc.abstractmethod
   def __init__( self, tdfpath, rootdir, table ):
      self._tdfpath = tdfpath
      self._rootdir = rootdir
      self._name = os.path.basename( os.path.splitext(tdfpath)[0] )
      self._table = table
      self._ntest = 0
      self._compare = "ndfcompare"

#  -------------------------------------------
#  Verify the table contents and header values.
#  -------------------------------------------
   def _verify( self, headerValues ):
      if "ID" not in self._table:
         raise TDFError( "Bad TDF file '{0}': Column 'ID' not found.".
                         format(self._tdfpath))
      elif "OUT" not in self._table:
         raise TDFError( "Bad TDF file '{0}': Column 'OUT' not found.".
                         format(self._tdfpath))
      else:
         self._ntest = len( self._table["ID"] )
         idset = set()
         for id in self._table["ID"]:
            lid = id.lower()
            if lid in idset:
               raise TDFError( "Bad TDF file '{0}': Identifier '{0}' is "
                               "used more than once.".format(id))
            else:
               idset.add(lid)

#  -------------------------------------------
#  Run all the tests described by the TestSet.
#  -------------------------------------------
   def run( self ):

#  Ensure the directory exists for the test set and move into it.
      if not os.path.exists(self._name):
         os.makedirs(self._name)
      os.chdir(self._name)

#  Assume all tests succeed.
      success = True

#  Do each test in the test set in turn.
      for itest in range(self._ntest):

#  Extract the values defining this test from the table.
         testVals = {}
         for colName in self._table:
            testVals[ colName ] = self._table[ colName ][ itest ]

#  Tell the user which test is being run.
         this_id = testVals["ID"]
         test_id = "{0}_{1}".format(self._name,this_id)
         msg_out("Running test {0} ...".format(test_id))
         addBlank = False

#  Ensure the directory exists for the current test ID and move into it.
         testdir = "test_{0}".format(this_id)
         if not os.path.exists(testdir):
            os.makedirs(testdir)
         os.chdir(testdir)

#  Delete any pre-extsing output text files.
         reportFile = "{0}/report".format(os.getcwd())
         if os.path.exists(reportFile):
            os.remove(reportFile)

         logFile = "{0}/log".format(os.getcwd())
         if os.path.exists(logFile):
            os.remove(logFile)

#  Ensure we have a "ref" directory in which to store any new reference
#  NDFs (and which should already contain any existing reference NDFs).
         if not os.path.exists("ref"):
            os.makedirs("ref")

#  Ensure we have an empty "failures" directory in which to perform the test,
#  and move into it. We use "failures" to make the paths included in the
#  report files look correct. The actual failure NDFs are stored
#  initiaslly in a directory called "temp" (created below), which is
#  renamed to "failures" after the "failures" directory created here has
#  been deleted.
         if os.path.exists("failures"):
            shutil.rmtree("failures")
         os.makedirs("failures")
         os.chdir("failures")

#  Generate new output NDFs, and write standard output to the test's log file.
         try:
            stdoutText = self._generateNDFs( testVals )
         except Exception as err:
            stdoutText = str(err)
            msg_out( stdoutText, starutil.CRITICAL )

         if stdoutText and not stdoutText.isspace():
            log = open( logFile, "w" )
            log.write( stdoutText )
            log.close()
         elif os.path.exists(logFile):
            os.remove(logFile)

#  Report any expected output NDFs that were not created.
         testOK = True
         realNDFs = []
         missingNDFs = []
         for newNDF in NDG( testVals["OUT"], False ):
            if not starutil.ndfExists( newNDF ):
               msg_out("   Expected output NDF '{0}' was not created !!!".
                       format(newNDF), starutil.CRITICAL)
               addBlank = True
               testOK = False
               missingNDFs.append(os.path.basename(newNDF)+".sdf")
            else:
               realNDFs.append( newNDF )

#  Ensure we have an empty "temp" directory to hold NDFs that fail their
#  comparison test.
         if os.path.exists("../temp"):
            shutil.rmtree("../temp")
         os.makedirs("../temp")

#  Include a list of any missing outputs in the temp directory so that
#  subsequent SUMMARY operations will mention them.
         if missingNDFs:
            mf = open("../temp/missing","w")
            for ndf in missingNDFs:
               mf.write("{0}\n".format(ndf))
            mf.close()

#  Check we have some output NDFs to check.
         if realNDFs:

#  Loop round each trio of new, reference and failure NDF.
            allNew = True
            newNDG = NDG( realNDFs )
            refNDG = NDG( newNDG, "../ref/*" )
            failNDG = NDG( newNDG, "../temp/*" )
            for (newNDF,refNDF,failNDF) in zip(newNDG,refNDG,failNDG):

#  If the reference NDF does not exist, just copy the new NDF to the
#  reference NDF.
               if not starutil.ndfExists( refNDF ):
                  msg_out("   Retaining new output NDF '{0}' as a reference NDF".
                          format(os.path.basename(newNDF)), starutil.CRITICAL)
                  addBlank = True
                  invoke("$KAPPA_DIR/ndfcopy in={0} out={1}".format(newNDF,refNDF) )

#  Otherwise, compare the new and reference NDFs, using the appropriate
#  tool.
               else:
                  allNew = False
                  if self._compare == "sc2compare":
                     invoke("$SMURF_DIR/sc2compare.py in1={0} in2={1} report=report".format(newNDF,refNDF) )
                     ok = starutil.get_task_par( "SIMILAR", "sc2compare" )

                  else:
                     invoke("$KAPPA_DIR/ndfcompare in1={0} in2={1} report=report".format(newNDF,refNDF) )
                     ok = starutil.get_task_par( "SIMILAR", "ndfcompare" )

#  If the test failed, copy the NDF into the failures directory, and
#  append the report file for this NDF to the total report file for the
#  current test (which includes reports on all changed NDFs created by
#  the test).
                  if not ok:
                     invoke("$KAPPA_DIR/ndfcopy in={0} out={1}".format(newNDF,failNDF) )
                     if testOK:
                        os.system( "echo ' ' > ../report" )
                     else:
                        os.system( "echo ' ' >> ../report" )
                     os.system( "echo '===========================================================' >> ../report" )
                     os.system( "echo 'Report on new NDF \"{0}\" generated by test \"{1}\"...' >> ../report".format(os.path.basename(newNDF),test_id) )
                     os.system( "echo '===========================================================' >> ../report" )
                     os.system( "echo ' ' >> ../report" )
                     os.system( "cat report >> ../report" )

                     testOK = False

#  Move up into the current test's main directory.
         os.chdir("..")

#  Remove the working directory.
         shutil.rmtree("failures")

#  Make a report to standard output. If the test passed, remove the
#  temp directory. If the test failed, rename it to "failures".
         if testOK:
            if not allNew:
               msg_out( "   test {0} passed".format(test_id))
            shutil.rmtree("temp")
         else:
            msg_out( "   test {0} failed !!!".format(test_id),
                     starutil.CRITICAL)
            reportFile = "{0}/report".format(os.getcwd())
            if os.path.exists(reportFile):
               msg_out( "   (see {0} for details)".format(reportFile),
                        starutil.CRITICAL)
            addBlank = True
            os.rename("temp","failures")
            success = False

#  Move up into the test set's main directory.
         os.chdir("..")

#  Display a nice blank line after a critical message.
         if addBlank:
            msg_out( " ", starutil.CRITICAL )

#  After completeing all tests in the test set, move back up to the main
#  root directory.
      os.chdir("..")

      return success

#  --------------------------------------------------------------
#  Update the reference NDFs for selected tests within a TestSet.
#  --------------------------------------------------------------
   def update( self, parsys, update ):

#  Check the directory exists for the test set and move into it.
      if os.path.exists(self._name):
         os.chdir(self._name)

#  Consider each test in the test set in turn.
         for this_id in self._table["ID"]:

#  Get the ID for the test.
            test_id = "{0}_{1}".format(self._name,this_id)

#  See if this test ID is in the list to be updated. Don't prompt now,
#  since we do not want to issue a prompt if this test does not need updating.
            if update == "all" or update == "prompt":
               doUpdate = True
            else:
               doUpdate = False
               i = 0
               for id in update:
                  if id.lower() == test_id.lower():
                     doUpdate = True
                     del update[i]
                  i += 1

#  Check the test's directory exists. If so, move into it.
            testdir = "test_{0}".format(this_id)
            if os.path.exists(testdir):
               os.chdir(testdir)

#  Check the test's failures directory exists, and move into it.
               if os.path.exists("failures"):
                  os.chdir("failures")

#  Get a list of the files in the failures directory. Only proceed if
#  some are found.
                  fails = glob.glob("*")
                  nfail = len( fails )
                  if nfail:

#  Decide if this test is to be updated. We can prompt the user now that
#  we know that this test is a candidate for being updated.
                     if doUpdate and update == "prompt":
                        parsys["OK"].cancel()
                        parsys["OK"].prompt = "Update reference NDFs for test {0}?".format(test_id)
                        doUpdate = parsys["OK"].value

#  Do the update if required.
                     if doUpdate:

#  Ensure the reference directory exists.
                        if not os.path.exists("../ref"):
                           os.makedirs("../ref")

#  Loop round each file in the failures directory. Move each one into the
#  ref directory.
                        for failpath in fails:
                           dst = "../ref/{0}".format(failpath)
                           if os.path.exists(dst):
                              os.remove(dst)
                           shutil.move( failpath, dst )

                        if nfail == 1:
                           msg_out("Updated 1 reference NDF for test {0}".format(test_id) )
                        else:
                           msg_out("Updated {0} reference NDF for test {1}".format(nfail,test_id) )

#  Move up into the main test directory, and delete the failures directory.
                        os.chdir("..")
                        os.removedirs("failures")

#  If not updating the test, just move up into the main test directory.
                     else:
                        os.chdir("..")

#  If there are no NDFs to update, just move up into the main test directory.
                  else:
                     os.chdir("..")

#  Move up into the main test set directory.
               os.chdir("..")

#  Return the modified list of test that remain to be updated.
      return update

#  ------------------------------------------------------------------------
#  Summarise the successes an failures of the previous run of this test set.
#  -------------------------------------------------------------------------
   def summary( self ):

#  Check the test set directory exists. If not report this, otherwise move
#  into it.
      if not os.path.exists(self._name):
         msg_out("No previous test results found for TDF file {0}.tdf".format(self._name) )
      else:
         os.chdir(self._name)

#  Consider each test in the test set in turn.
         for this_id in self._table["ID"]:

#  Get the ID for the test.
            test_id = "{0}_{1}".format(self._name,this_id)

#  Check the test's directory exists. If not, report it. Otherwise, move
#  into it.
            testdir = "test_{0}".format(this_id)
            if not os.path.exists(testdir):
               msg_out("No previous results found for test {0}".format(test_id))
            else:
               os.chdir(testdir)

#  Check the test's failures directory exists. If not, report it.
#  Otherwise, move into it.
               if not os.path.exists("failures"):
                  msg_out("Test {0} passed.".format(test_id))
               else:
                  os.chdir("failures")

#  Get a list of any output NDFs that were not created.
                  if os.path.exists("missing"):
                     with open("missing") as f:
                        missing = f.readlines()
                  else:
                     missing = []

#  Get a list of the files in the failures directory. If there are some,
#  report failures.
                  fails = glob.glob("*.sdf")
                  if fails or missing:
                     reportFile = "{0}/report".format(os.getcwd())
                     if os.path.exists(reportFile):
                        msg_out("Test {0} failed for the following output NDFs (see "
                                "{1} for details):".format(test_id,reportFile))
                     else:
                        msg_out("Test {0} failed for the following output NDFs:"
                                .format(test_id))

                     if fails:
                        for failpath in fails:
                           msg_out("   {0}".format(failpath))

                     if missing:
                        for failpath in missing:
                           msg_out("   {0}".format(failpath))

                     msg_out(" ")
                  else:
                     msg_out("Test {0} passed.".format(test_id))

#  Move up into the main test directory.
                  os.chdir("..")

#  Move up into the main test set directory.
               os.chdir("..")


#  -----------------------------
#  Generate the new output NDFs.
#  -----------------------------
   @abc.abstractmethod
   def _generateNDFs( self, testVals ):
      return None


#  ----------------------------------------------------------------------
#  A static factory method to create a TestSet of a suitable class from a
#  TDF file.
#  ----------------------------------------------------------------------
   @staticmethod
   def create( tdfpath, rootdir ):
      result = None
      table = {}
      headerValues = {}
      colNames = None
      headerValueRE = re.compile("^#\s*(\S+)\s*=\s*(\S+)" )

#  Open the TDF file and loop round every line, removing leading and
#  trailing white space (including newlines).
      inHeader = True
      tdf = open( tdfpath, "r" )
      iline = 0
      for line in tdf:
         line = line.strip()
         iline += 1

#  Header lines...
         if line[0:1] == '#':
            if not inHeader:
               raise TDFError("Bad TDF file '{0}': Unexpected header line "
                              "found at line {1}:\n{2}".format(tdfpath,iline,line) )
            else:
               match = headerValueRE.search( line )
               if match:
                  headerValues[ match.group(1).lower() ] = match.group(2).lower()
               colNames = line[1:].upper().split()

#  Table lines:
         elif line:
            if inHeader:
               inHeader = False
               if colNames:
                  ncol = len( colNames )
                  for colName in colNames:
                     table[ colName ] = []
               else:
                  ncol = 0

            colValues = splitter( line, iline, tdfpath )
            if colValues:
               nval = len( colValues )
            else:
               nval = 0

            if nval != ncol:
               raise TDFError("Bad TDF file '{0}': Unexpected number of values "
                              "({1}) found at line {2} (was expecting {3} "
                              "values):\n{4}".format(tdfpath,nval,iline,ncol,
                                                     line) )
            if colValues:
               icol = 0
               for colValue in colValues:
                  table[ colNames[ icol ] ].append( colValue )
                  icol += 1

      if inHeader:
         raise TDFError("Bad TDF file '{0}': No table of tests definitions "
                        "found".format(tdfpath) )

#  Close the TDF file.
      tdf.close()

#  Create a TestSet of the required type.
      if "type" in headerValues:
         type = headerValues[ "type" ]
         if type == "shell":
            result = ShellTestSet( tdfpath, rootdir, headerValues, table )
         elif type == "makemap":
            result = MakeMapTestSet( tdfpath, rootdir, headerValues, table )
         elif type == "makemap_jsa":
            result = MakeMapJSATestSet( tdfpath, rootdir, headerValues, table )
         else:
            raise TDFError("Bad TDF file '{0}': Header specifies unknown "
                           "test type '{1}'".format(tdfpath,type) )
      else:
         raise TDFError("Bad TDF file '{0}': No test type specified in header".
                        format(tdfpath) )

      return result



#  A class of test based on running arbitrary shell commands.
#  ==========================================================
class ShellTestSet(TestSet):

#  ------------
#  Constructor.
#  ------------
   def __init__( self, tdfpath, rootdir, headerValues, table ):
      super(ShellTestSet,self).__init__( tdfpath, rootdir, table )
      self._verify( headerValues )

#  -------------------------------------------
#  Verify the table contents and header values.
#  -------------------------------------------
   def _verify( self, headerValues ):
      super(ShellTestSet,self)._verify( headerValues )
      if "CMD" not in self._table:
         raise TDFError( "Bad TDF file '{0}': Column 'CMD' not found.".
                         format(self._tdfpath))
      elif "compare" not in headerValues:
         raise TDFError( "Bad TDF file '{0}': Header does not contain a "
                         "'compare' value.".format(self._tdfpath))
      else:
         self._compare = headerValues["compare"]
         if self._compare != "sc2compare" and self._compare != "ndfcompare":
            raise TDFError( "Bad TDF file '{0}': Illegal value ({1}) found for "
                            "'compare' in the header.".format(self._tdfpath,self._compare))

#  -----------------------------
#  Generate the new output NDFs.
#  -----------------------------
   def _generateNDFs( self, testVals ):
      return invoke( testVals[ "CMD" ] )



#  A class of test based on running smurf:makemap to generate a
#  SCUBA-2 map in a single output NDF .
#  ==========================================================
class MakeMapTestSet(TestSet):

   def __init__( self, tdfpath, rootdir, headerValues, table ):
      super(MakeMapTestSet,self).__init__( tdfpath, rootdir, table )
      self._sc2data = None
      self._compare = "sc2compare"
      self._addOutputs()
      self._verify( headerValues )

#  -------------------------------------------
#  Add the "OUT" column to the table of tests.
#  -------------------------------------------
   def _addOutputs( self ):
      self._table["OUT"] = ["map"] * len( self._table["ID"] )

#  -------------------------------------------
#  Verify the table contents and header values.
#  -------------------------------------------
   def _verify( self, headerValues ):
      super(MakeMapTestSet,self)._verify( headerValues )

      for colName in ["OBS", "ARRAYS", "CONFIG", "PARAMS"]:
         if colName not in self._table:
            raise TDFError( "Bad TDF file '{0}': Column '{1}' not found.".
                            format(self._tdfpath,colName))

      if "sc2data" not in headerValues:
         raise TDFError( "Bad TDF file '{0}': Header does not contain a "
                         "'sc2data' value.".format(self._tdfpath))
      else:
         self._sc2data = headerValues["sc2data"]
         if not os.path.isdir(self._sc2data):
            raise TDFError( "Bad TDF file '{0}': Bad value ({1}) found for "
                            "'sc2data' in the header.".format(self._tdfpath,self._sc2data))

#  -----------------------------
#  Generate the new output NDFs.
#  -----------------------------
   def _generateNDFs( self, testVals ):

      arrays = testVals["ARRAYS"]
      if arrays == "s8":
         arrays = "s8a,s8b,s8c,s8d"
      elif arrays == "s4":
         arrays = "s4a,s4b,s4c,s4d"

#  Generate a file cantaining the paths to the input raw data files.
      fd = open( "infiles", "w" )
      for utobs in testVals["OBS"].split(','):
         (ut,obs) = utobs.split('/')
         obs = obs.zfill(5)
         for array in arrays.split(','):
            fd.write("{0}/{1}/{2}/{3}/*\n".format(self._sc2data,array,ut,obs))
      fd.close()

#  Get the path to the config file.
      confdir = os.path.expandvars("$SMURF_DIR/../../share/smurf")
      config = testVals["CONFIG"]
      if config != "null":
         if not os.path.exists(config):
            config = "{0}/{1}".format(confdir,testVals["CONFIG"])
            if not os.path.exists(config):
               config = "{0}/{1}.lis".format(confdir,testVals["CONFIG"])
               if not os.path.exists(config):
                  raise TDFError( "Bad TDF file '{0}': MAKEMAP config file {1} "
                                  "requested but cannot be found.".
                                  format(self._tdfpath,testVals["CONFIG"]))

#  Output file names (may be more than one if it is being split into
#  tiles).
      outfiles = self._getOut()

#  Test the supplied params settings for disallowed parameters.
      params = testVals["PARAMS"]
      if params != "null":
         if re.search( params, 'in=', re.IGNORECASE ):
            raise TDFError( "Bad TDF file '{0}': MAKEMAP parameter list includes "
                            "IN, which is not allowed.".format(self._tdfpath))
         if re.search( params, 'out=', re.IGNORECASE ):
            raise TDFError( "Bad TDF file '{0}': MAKEMAP parameter list includes "
                            "OUT, which is not allowed.".format(self._tdfpath))
         if re.search( params, 'jsatiles', re.IGNORECASE ):
            raise TDFError( "Bad TDF file '{0}': MAKEMAP parameter list includes "
                            "JSATILES, which is not allowed.".format(self._tdfpath))
         if re.search( params, 'tiledims', re.IGNORECASE ):
            raise TDFError( "Bad TDF file '{0}': MAKEMAP parameter list includes "
                            "TILEDIMS, which is not allowed.".format(self._tdfpath))
         if config != "null" and re.search( params, 'config=', re.IGNORECASE ):
            raise TDFError( "Bad TDF file '{0}': MAKEMAP parameter list "
                            "include CONFIG, but a value has been supplied "
                            "in the CONFIG column.".format(self._tdfpath))
      else:
         params = ''

#  Get any extra parameter settings required by the particular class of
#  makemap test.
      extrapars = self._getExtraPars()

#  Construct the makemap command line.
      cmd = "$SMURF_DIR/makemap in=^infiles out={0} {1} {2}".format(outfiles,params,extrapars)
      if config != "null":
         cmd = "{0} config=^{1}".format(cmd,config)

#  Run the command.
      return invoke( cmd )

# ----------------------------------------------------------------------
#  Return a string holding any extra parameter settings required by this
#  particular class of makemap test.
# ----------------------------------------------------------------------
   def _getExtraPars( self ):
      return ''

# -------------------------------------------------------------------------------
#  Return a string holding the value to be assigned to the makemap OUT parameter.
# -------------------------------------------------------------------------------
   def _getOut( self ):
      return 'map'



#  A class of test based on running smurf:makemap to generate a
#  SCUBA-2 map as a set of JSA tiles..
#  ==========================================================
class MakeMapJSATestSet(MakeMapTestSet):

   def __init__( self, tdfpath, rootdir, headerValues, table ):
      super(MakeMapJSATestSet,self).__init__( tdfpath, rootdir, headerValues,
                                              table )
      self._sc2data = None
      self._compare = "sc2compare"
      self._addOutputs()
      self._verify( headerValues )

#  -------------------------------------------
#  Add the "OUT" column to the table of tests.
#  -------------------------------------------
   def _addOutputs( self ):
      self._table["OUT"] = []
      if "TILES" not in self._table:
         raise TDFError( "Bad TDF file '{0}': Column 'TILES' not found.".
                         format(self._tdfpath,colName))
      for tiles in self._table["TILES"]:
         outs = None
         for tile in tiles.split(','):
            tile = tile.strip()
            if not tile.isdigit():
               raise TDFError( "Bad TDF file '{0}': Column 'TILES' "
                               "contains non-integer value '{1}'.".
                               format(self._tdfpath,tile))
            elif outs:
               outs = "{0},tile_{1}".format( outs, tile )
            else:
               outs = "tile_{0}".format( tile )
         self._table["OUT"].append( outs )

# ----------------------------------------------------------------------
#  Return a string holding any extra parameter settings required by this
#  particular class of makemap test.
# ----------------------------------------------------------------------
   def _getExtraPars( self ):
      return 'jsatiles=yes'

# -----------------------------------------------------------------
#  Return a string holding a setting for the makemap OUT parameter.
# -----------------------------------------------------------------
   def _getOut( self ):
      return 'tile'



#  Main Entry point
#  ==========================================================

#  Catch any exception so that we can always clean up, even if control-C
#  is pressed.
try:

#  Declare the script parameters. Their positions in this list define
#  their expected position on the script command line. They can also be
#  specified by keyword on the command line. No validation of default
#  values or values supplied on the command line is performed until the
#  parameter value is first accessed within the script, at which time the
#  user is prompted for a value if necessary. The parameters "MSG_FILTER",
#  "ILEVEL", "GLEVEL" and "LOGFILE" are added automatically by the ParSys
#  constructor.
   params = []
   params.append(starutil.Par0S("ROOTDIR", "Root directory conting test "
                                "definitions" ))
   params.append(starutil.Par0S("UPDATE", "Tests for which the reference "
                                "data is to be updated", None, noprompt=True))
   params.append(starutil.Par0L("SUMMARY", "Produce a summary of the "
                                "previous run?", False,noprompt=True))
   params.append(starutil.Par0L("OK", "Update the test?"))

#  Set the default value for GLEVEL parameter, created by the ParSys
#  constructor. This means that no logfile will be created by default.
   starutil.glevel = starutil.NONE

#  Initialise the parameters to hold any values supplied on the command
#  line.
   parsys = ParSys( params )

#  Get the root directory path, and check it exists. If so, convert it to
#  an absolute path and change directory into it.
   rootdir = parsys["ROOTDIR"].value
   if os.path.exists(rootdir):
      rootdir = os.path.abspath(rootdir)
      os.chdir(rootdir)
   else:
      raise starutil.InvalidParameterError("\nThe specified root directory "
                  "({0}) does not exist.".format(rootdir) )

#  See if a summary of a previous run is to be produced.
   summary = parsys["SUMMARY"].value

#  See if we are to update the reference NDFs rather than perform any new
#  tests. If so, split the string into serparate terms and strip trailing
#  or leading white space.
   msg_out(" ")
   if not summary:
      update = parsys["UPDATE"].value
      if update:
         lup = update.lower()
         if lup == "all" or lup == "prompt":
            update = lup
         else:
            update = [testId.strip() for testId in update.split(',')]
         msg_out("Updating reference NDFs in root directory '{0}'".format(os.getcwd()))
      else:
         msg_out("Performing all tests in root directory '{0}'".format(os.getcwd()))
   else:
      msg_out("Summarising results of previous startest run in root "
              "directory '{0}'".format(os.getcwd()))
      update = None
   msg_out(" ")

#  Assume all tests pass.
   success = True

#  Loop round all ".tdf" files in the root directory.
   for tdfpath in glob.glob("*.tdf"):

#  An error on a previous test could have left the current directory anywhere,
#  so ensure we are in the root directory.
      os.chdir(rootdir)


#  Create an object that describes the tests defined in the current tdf
#  file.
      testset = TestSet.create( tdfpath, rootdir )

#  If required, summarise the previous run of the current testset.
      if summary:
         testset.summary()

#  Otherwise, if required, update the reference NDFs for the current testset.
      elif update:
         update = testset.update( parsys, update )

#  otherwise run the tests, and set the success flag false if any of the
#  tests fail. Use a Try caluse so that failure of one test does not
#  prevent other tests running.
      else:
         try:
            if not testset.run():
               success = False
         except Exception as err:
            success = False
            msg_out( str(err), starutil.CRITICAL )

#  Report an error if any non-existent test IDs were included in UPDATE.
#  Legal ones will have been removed by the above calls to the "update"
#  method.
   if update and update != "all" and update != "prompt":
      msg_out( "Unknown test identifiers supplied for parameter UPDATE: {0}".format(update) )

#  Write the output parameter.
   starutil.put_task_par( "success", "startester", success, "_LOGICAL" )

#  Remove temporary files.
   cleanup()

#  If an StarUtilError of any kind occurred, display the message but hide the
#  python traceback. To see the trace back, uncomment "raise" instead.
except starutil.StarUtilError as err:
#   raise
   print( err )
   cleanup()

# This is to trap control-C etc, so that we can clean up temp files.
except:
   cleanup()
   raise

