*+  CGS3DR_REDUCE_GRP - REDUCE a group of CGS3 runs
      SUBROUTINE CGS3DR_REDUCE_GRP (STATUS)
*    Description:
*     performs a basic data reduction sequence on a set of CGS3 data files
*     interpreted as a group. Requires first OBJECT run of group as the
*     GRPNUM parameter. Determines other runs from the header information
*     in this one.
*    Invocation:
*     CALL CGS3DR_REDUCE_GRP (STATUS)
*    Parameters:
*     STATUS = INTEGER (UPDATE)
*           ADAM Status return
*    Method:
*     If status bad then return
*     Get number of observation to reduce as a group
*     Reduce the group of observations
*    Deficiencies:
*     None Known
*    Bugs:
*     None Known
*    Authors:
*     A. Bridger (JAC::AB)
*    History:
*      4-Jan-93: Original (JAC::AB)
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
*    Import:
*     None
*    Import-Export:
*     None
*    Export:
*     None
*    Status:
      INTEGER STATUS
*    External references:
*     None
*    Global variables:
      INCLUDE 'CGS3DR_CMN'
*    Local Constants:
*     None
*    Local variables:
      INTEGER GRPNUM
*    Internal References:
*     None
*    Local data:
*     None
*-

*    If status bad then return
      IF (STATUS .NE. SAI__OK) RETURN

*    Get group to reduce.
      CALL PAR_GET0I ('GRPNUM', GRPNUM, STATUS)
      CALL PAR_CANCL ( 'GRPNUM', STATUS )

*    And reduce it
      CALL CGS3DR_REDGRP (GRPNUM, STATUS)

*    Check for errors
      IF (STATUS. NE. SAI__OK) THEN
         CALL MSG_SETI ('GRP', GRPNUM)
         CALL ERR_OUT (' ', 'Failed to reduce group number ^GRP',
     :    STATUS)
      END IF


      END
