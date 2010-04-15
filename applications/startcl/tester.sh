
#+
#  Name:
#     tester.sh

#  Purpose:
#     Test script for STARTCL package

#  Description:
#     Short test of STARTCL functionality.  It's not exhaustive, but it
#     checks that at atclsh shell runs up satisfactorily and uses the
#     ADAM extension to exchange messages.

#  Author:
#     MBT: Mark Taylor (Starlink)

#  History:
#     13-MAY-2004 (MBT):
#        Initial version.
#-

./atclsh startcl_test2 & ./atclsh startcl_test
