*+  CGS3DR_REDUCE_RUN - REDUCE single run for CGS3 data reduction
      SUBROUTINE CGS3DR_REDUCE_RUN (STATUS)
*    Description :
*     Performs a basic data reduction sequence on a single CGS3 run
*    Invocation :
*     CALL CGS3DR_REDUCE_RUN (STATUS)
*    Parameters :
*     STATUS = INTEGER (UPDATE)
*           ADAM Status return
*    Method :
*     If status bad then return
*     Get observation to reduce.
*     Get if dividing by sky
*     Get if polarimetry
*     if (divbysky) reduce sky subspectrum
*     reduce object subspectrum
*     if (divbysky) divide object by sky
*     plot object
*    Deficiencies :
*     None Known
*    Bugs :
*     None Known
*    Authors :
*     A. Bridger (JAC::AB)
*    History :
*      7-Nov-91: Original (JAC::AB)
*     14-Dec-92: Fully implement (JAC::AB)
*     15-Dec-92: Add polarimetry option and plotting (JAC::AB)
*      4-Jan-93: Replace TASK_OBEY and _DONE with CGS3DR_OBEYW (JAC::AB)
*      4-Jan-93: The guts of this put into REDRUN, simplifying this and
*                allowing the creation of REDUCE_GRP. (JAC::AB)
*     28-Feb-96: do a par_cancl after getting run number
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
*    Import :
*     None
*    Import-Export :
*     None
*    Export :
*     None
*    Status :
      INTEGER STATUS
*    External references :
*    Global variables :
      INCLUDE 'CGS3DR_CMN'
*    Local Constants :
*     None
*    Local variables :
      INTEGER      RUNNUM             ! Run number to reduce
*    Internal References :
*     None
*    Local data :
*     None
*-

*    If status bad then return
      IF (STATUS .NE. SAI__OK) RETURN

*    Get observation to reduce.
      CALL PAR_GET0I ('RUNNUM', RUNNUM, STATUS)
      CALL PAR_CANCL ( 'RUNNUM', STATUS )

*    And reduce it
      CALL CGS3DR_REDRUN (RUNNUM, STATUS)

*    Check for errors
      IF (STATUS. NE. SAI__OK) THEN
         CALL MSG_SETI ('RUN', RUNNUM)
         CALL ERR_OUT (' ', 'Failed to reduce run number ^RUN', STATUS)
      END IF


      END
