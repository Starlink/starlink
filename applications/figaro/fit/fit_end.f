C+
      SUBROUTINE FIT_END (STATUS)
C
C     F I T _ E N D
C
C     Closes down the FIT_ tape access routines.  This should
C     be the last call to any FIT_ routine.  Once this is called,
C     FIT_INIT will be needed if the tape is to be accessed again.
C
C     Parameters -  (">" input, "<" output)
C
C     (<) STATUS   (Integer) Returned status code.  0 => OK, non-zero
C                  values indicate tape I/O error codes and can be
C                  decoded using FIT_ERROR.
C
C     Common variables used
C
C     (>) MTUNIT   (Integer) I/O channel for the tape being used.
C     (<) MTERR    (Character) Description of last I/O error
C     (<) MTSTAT   (Integer) Last TIO_ error code.
C     (<) TDENS    (Integer) Tape density
C
C                  MTERR is defined in COMB.INC, the others in COMF.INC
C
C     Subroutines / functions used -
C
C     TIO_GETMSG   (TIO_ package) Decode TIO_ error code.
C     TIO_ERR      ( "      "   ) See if status indicates an error
C     TIO_CLOSE    ( "      "   ) Close down opened tape.
C
C                                               KS / CIT 11th Oct 1983
C     Modified:
C
C     23rd Oct 1987   Now uses TIO routines instead of MTPCKG
C     27th Jun 1990   Now clears the tape density value.
C+
      IMPLICIT NONE
C
C     Parameters
C
      INTEGER STATUS
C
C     Functions
C
      LOGICAL TIO_ERR
C
C     Local variables
C
      INTEGER LENGTH
C
C     Common blocks
C
      INCLUDE 'COMB'
      INCLUDE 'COMF'
C
C     Clear the tape density value (so FIT_INIT won't try to force the
C     same density on the next tape).
C
      TDENS=0
C
C     Close down the tape unit and check error code
C
      CALL TIO_CLOSE(MTUNIT,STATUS)
      IF (TIO_ERR(STATUS)) THEN
         CALL TIO_GETMSG(STATUS,MTERR,LENGTH)
         MTSTAT=STATUS
      ELSE
         STATUS=0
      END IF
C
      END
