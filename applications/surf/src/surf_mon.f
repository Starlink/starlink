*+  REDS - main routine for SCUBA offline data reduction package
      SUBROUTINE REDS (STATUS)
*    Description :
*     This is the main routine for the SCUBA reduction A-task.
*    Invocation :
*     CALL REDS (STATUS)
*    Parameters :
*     STATUS        = INTEGER (Given and returned)
*           global status
*    Method :
*    Deficiencies :
*    Bugs :
*    Authors :
*     J.Lightfoot (REVAD::JFL)
*     Tim Jenness (timj@JACH)
*    History :
*     $Id$
*     25-FEB-1993: Original version
*     12-JAN-1995: Ported to UNIX, changed to 'new style'
*     $Log$
*     Revision 1.3  1996/09/16 20:27:18  timj
*     Change PHOTOM to SCUPHOT
*
c Revision 1.2  1996/07/31  18:53:16  timj
c Add skydip option
c
*    endhistory
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'PAR_PAR'
*    Import :
*    Import-Export :
*    Export :
*    Status :
      INTEGER STATUS
*    External references :
*    Global variables :
*    Local Constants :
*    Local variables :
      CHARACTER*(PAR__SZNAM) NAME        ! name of action
*    Internal References :
*    Local data :
*-

      IF (STATUS .NE. SAI__OK) RETURN

      CALL TASK_GET_NAME (NAME, STATUS)

      IF (NAME .EQ. 'CROSSTALK') THEN

         CALL REDS_CROSSTALK (STATUS)

      ELSE IF (NAME .EQ. 'EXTINCTION') THEN

         CALL REDS_EXTINCTION (STATUS)

      ELSE IF (NAME .EQ. 'FLATFIELD') THEN

         CALL REDS_FLATFIELD (STATUS)

      ELSE IF (NAME .EQ. 'GET_DEMOD') THEN

         CALL REDS_GET_DEMOD (STATUS)

      ELSE IF (NAME .EQ. 'GET_FLAT') THEN

         CALL REDS_GET_FLAT (STATUS)

      ELSE IF (NAME .EQ. 'MODIFY') THEN

         CALL REDS_MODIFY (STATUS)

      ELSE IF (NAME .EQ. 'SCUPHOT') THEN

         CALL REDS_PHOTOM (STATUS)

      ELSE IF (NAME .EQ. 'REBIN') THEN

         CALL REDS_REBIN (STATUS)

      ELSE IF (NAME .EQ. 'REDUCE_SWITCH') THEN

         CALL REDS_REDUCE_SWITCH (STATUS)

      ELSE IF (NAME .EQ. 'RESTORE') THEN

         CALL REDS_RESTORE (STATUS)

      ELSE IF (NAME .EQ. 'SKYDIP') THEN

         CALL REDS_SKYDIP (STATUS)

      END IF

      END
