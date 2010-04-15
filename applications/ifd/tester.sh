
#+
#  Name:
#     tester.sh

#  Purpose:
#     Automake test script for IFD package.

#  Description:
#     This is a short script which does regression tests on some of
#     the IFD scripts, comparing output against runs done previously.
#     It is not exhaustive.  It was copied from the old-style Starlink
#     IFD makefile.

#  Exit Value:
#     Zero for success, non-zero for failure.

#  Notes:
#     The tests will only run correctly if they are run *after*
#     installation, since the scripts source files in $(bindir) to work.
#     This is not the way that make check normally works.

#  Authors:
#     MBT: Mark Taylor (Starlink)

#  History:
#     12-MAY-2003 (MBT):
#        Initial version (taken from original IFD makefile).
#-

#  Run IFD scripts and save diffs from correct outputs.
rm -f testdif
./ifl2ifd test
diff test.ifd testc.ifd >testdif
./ifd2star test
diff test.ifl testc.ifl >>testdif
./ifd2iraf test
diff test.cl testc.cl >>testdif

#  Diff and test output.
if test `cat testdif | wc -l` -ne 8
then
   echo "Created files differ from control files in other than date."
   cat testdif
   echo "Note check target only works *after* installation"
   rm -f testdif
   #rm -f test.ifd test.cl test.icl test.csh test.par test.tcl
   rm -f test1.ifl test2.ifl test1.par test2.par
   exit 1
else
   rm -f testdif
   #rm -f test.ifd test.cl test.icl test.csh test.par test.tcl
   rm -f test1.ifl test2.ifl test1.par test2.par
fi
