*+  CGS3DR - I-task Monolith for reducing CGS3 data
      SUBROUTINE CGS3DR (STATUS)
*      Call relevant routine to carry out CGS3 data reduction sequences
*    Invocation :
*     CALL CGS3DR (STATUS)
*    Parameters :
*     STATUS = INTEGER
*    Authors :
*     Alan Bridger (JAC::AB)
*    History :
*      6-Nov-91: Original (JAC::AB)
*      9-FEB-93: Added SETPAR and SHOPAR (JAC::AB)
*     14-Nov-95: Add REDUCE_PHOT (JAC::KK)
*     06-Dec-95: change to Itask for unix
*     27-Feb-96: renamed from cgs3_dr
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'PAR_PAR'
*    Import :
      CHARACTER*(PAR__SZNAM)  NAME
*    Status :
      INTEGER  STATUS
*-

      IF (STATUS .NE. SAI__OK) RETURN

      CALL TASK_GET_NAME( NAME, STATUS )

      IF (NAME .EQ. 'INIT') THEN
         CALL CGS3DR_INIT (STATUS)

      ELSE IF (NAME .EQ. 'SETPAR') THEN
         CALL CGS3DR_SETPAR (STATUS)

      ELSE IF (NAME .EQ. 'SHOPAR') THEN
         CALL CGS3DR_SHOPAR (STATUS)

      ELSE IF (NAME .EQ. 'REDUCE_RUN') THEN
         CALL CGS3DR_REDUCE_RUN (STATUS)

      ELSE IF (NAME .EQ. 'REDUCE_GRP') THEN
         CALL CGS3DR_REDUCE_GRP (STATUS)

      ELSE IF (NAME .EQ. 'REDUCE_PHOT') THEN
         CALL CGS3DR_REDUCE_PHOT (STATUS)

      END IF

      END
