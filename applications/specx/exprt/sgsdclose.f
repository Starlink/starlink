C-----------------------------------------------------------------

      SUBROUTINE SPECX_GSD_CLOSE (IERR)

C   Routine to open a GSD file containing a generalized observation

C History:
C    8-May-2000 (ajc):
C      Port to Linux
C      Replace 'TYPE *" with 'PRINT *'
C-
      IMPLICIT  NONE

      INTEGER*4 ADAM__OK
      PARAMETER (ADAM__OK=0)

      INTEGER*4 IERR
      INTEGER*4 STATUS

      INCLUDE  'GSD_FILE.INC'

C  Close the GSD file

      STATUS = ADAM__OK

      CALL GSD_CLOSE (IFD, STATUS)
      GSD_OPEN = .FALSE.
      IF (STATUS.NE.ADAM__OK) THEN
        PRINT *,'Error closing GSD file - status',status
        IERR = 10
        GO TO 99
      END IF

C  Standard return

   99 IF (STATUS.NE.ADAM__OK) THEN
        PRINT '('' Status'',2X,I10,4X,''($'',Z8.8,'')'')', status,status
      END IF

      RETURN
      END

C-------------------------------------------------------------------------
