C+
      SUBROUTINE FIT_DISMT (STATUS)
C
C     F I T _ D I S M T
C
C     This routine needs to be called if a tape is to be physically
C     dismounted and replaced with another.  VMS 5 and onwards, in
C     particular, if very fussy about tapes being changed behind its
C     back.  This routine should be called after FIT_CLOSE.  A new
C     tape can then be mounted physically on the drive and FIT_INIT
C     called again to initialise access to it.
C
C     Parameters -  (">" input, "<" output)
C
C     (<) STATUS     (Integer) Returned status code.  0 => OK,
C                    non-zero values are I/O error codes which can be
C                    decoded by FIT_ERROR.
C
C     Common variables used -
C
C     (>) MTUNIT     (Integer) The I/O channel for the mag tape.
C     (<) MTERR      (Character) Description of the last tape I/O error.
C     (<) MTSTAT     (Integer) Last TIO_ error code
C     (>) DEVICE     (Character) The device name for the tape drive.
C     (>) LUFILE     (Integer) Logical unit number for disk file, if used.
C     (<) TDENS      (Integr) Tape density
C
C     MTERR, DEVICE defined in COMB.INC, the others in COMF.INC
C
C     Subroutines / functions used -
C
C     TIO_DISMT      (TIO_ package) Dismount a tape.
C     TIO_GETMSG     ( "      "   ) Decode TIO_ error code.
C     TIO_ERR        ( "      "   ) See if status indicates an error
C     TIO_SENSE      ( "      "   ) Determine information about a tape
C
C                                             KS / AAO 1st Feb 1990
C     Modified:
C
C     27th Jun 1990.  Now sets density value in common.  KS/AAO.
C+
      IMPLICIT NONE
C
C     Parmeters
C
      INTEGER STATUS
C
C     Functions
C
      LOGICAL TIO_ERR, TIO_SENSE
C
C     Common blocks
C
      INCLUDE 'COMB'
      INCLUDE 'COMF'
C
C     Local variables
C
      INTEGER LENGTH
C
C     Do nothing if output is not to a tape.
C
      IF (MTUNIT.EQ.0) RETURN
C
C     Set the tape density so that FIT_INIT will know it is expected to
C     force the density of a new tape to the same as this one.
C
      IF (TIO_SENSE(DEVICE,'6250',STATUS)) THEN
         TDENS=6250
      ELSE IF (TIO_SENSE(DEVICE,'1600',STATUS)) THEN
         TDENS=1600
      ELSE IF (TIO_SENSE(DEVICE,'800',STATUS)) THEN
         TDENS=800
      ELSE
         TDENS=0
      END IF
C
C     Dismount the tape, and physically unload it.
C
      CALL TIO_DISMT (DEVICE,.TRUE.,STATUS)
      IF (TIO_ERR(STATUS)) THEN
         CALL TIO_GETMSG(STATUS,MTERR,LENGTH)
         MTSTAT=STATUS
      ELSE
         STATUS=0
      END IF
C
      END
