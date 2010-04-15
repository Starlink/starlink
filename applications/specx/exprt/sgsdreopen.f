C-------------------------------------------------------------------------

      SUBROUTINE SPECX_GSD_REOPEN (ISCAN, IERR)

C   Routine to open a GSD file containing a generalized observation

      IMPLICIT  NONE

      INTEGER   ADAM__OK
      PARAMETER (ADAM__OK=0)

      INTEGER   ISCAN
      INTEGER   IERR
      INTEGER   STATUS

      INCLUDE  'FLAGCOMM'
      INCLUDE  'GSD_FILE.INC'

C  Close then Open the GSD file

      IF (IERR.NE.0) RETURN
      STATUS = ADAM__OK


      IERR = 0

      CALL SPECX_GSD_CLOSE (IERR)

      CALL SPECX_GSD_OPEN   (ISCAN, IERR)

      IF (IERR.NE.0) GO TO 99

C     Read file header according to version

      IF (VERSION .LT. 4.99) THEN

         CALL SPECX_GSD_V4_HEADER (IERR)

      ELSE

         CALL SPECX_GSD_V5_HEADER (IERR)

      END IF


 99   CONTINUE

C     Standard return

      RETURN
      END

C-------------------------------------------------------------------------
