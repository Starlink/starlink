C-----------------------------------------------------------------

      SUBROUTINE SPECX_GSD_CLOSE (IERR)

C   Routine to open a GSD file containing a generalized observation

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
        type *,'Error closing GSD file - status',status
        IERR = 10
        GO TO 99
      END IF

C  Standard return

   99 IF (STATUS.NE.ADAM__OK) THEN
        type '('' Status'',2X,I10,4X,''($'',Z8.8,'')'')', status,status
      END IF

      RETURN
      END

C-------------------------------------------------------------------------
