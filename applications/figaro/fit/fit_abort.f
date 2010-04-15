C+
      SUBROUTINE FIT_ABORT (STATUS)
C
C     F I T _ A B O R T
C
C     Aborts the writing of a FITS image, backspacing the tape to
C     the start of the image and writing an end of tape mark.  This
C     can be used, for example, if the end of tape is reached during
C     the writing of the image and the calling program wishes to
C     abort the output and leave itself with a properly terminated
C     tape.  The tape is left positioned between the two file marks
C     that constitute the end of tape.  If output is to disk, this
C     routine does nothing.  This routine closes down the FIT_
C     package for the current image in the same way as does FIT_CLOSE.
C     FIT_END still needs to be called to close down the whole system.
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
C     (<) FPTR       (Integer) Pointer to next free byte in FBUFF.
C     (<) MTSTAT     (Integer) Last TIO_ error code
C     (>) DEVICE     (Character) The device name for the tape drive.
C     (>) LUFILE     (Integer) Logical unit number for disk file, if used.
C     (<) BCOUNT     (Integer) Is the number of logical records already held
C                    in TBUFF.
C
C     MTERR, DEVICE defined in COMB.INC, the others in COMF.INC
C
C     Subroutines / functions used -
C
C     TIO_SKIP       (TIO_ package) Write a file mark on a foreign mag tape
C     TIO_GETMSG     ( "      "   ) Decode TIO_ error code.
C     TIO_ERR        ( "      "   ) See if status indicates an error
C     TIO_SENSE      ( "      "   ) Test status of tape.
C     FIT_QEOT       (FIT     "   ) See if code indicates end of tape.
C
C                                             KS / AAO 5th Nov 1987
C     Modified:
C
C     30th Jan 1990  KS/AAO. Now clears the common variables used for
C                    blocked output as well.
C
C     Note:
C
C     Although nothing in this code (except the include statement format)
C     seems particularly VAX-specific, some tape operations that work on
C     VAXES may not work on other machines: some systems may not let you
C     do anything with a tape that has hit the end of tape marker.  Even
C     on VMS some things changed with version 5, which got stricter about
C     dismounting tapes, although that isn't strictly relevant to this
C     routine.
C+
      IMPLICIT NONE
C
C     Parmeters
C
      INTEGER STATUS
C
C     Functions
C
      LOGICAL  FIT_QEOT, TIO_ERR, TIO_SENSE
C
C     Common blocks
C
      INCLUDE 'COMB'
      INCLUDE 'COMF'
C
C     Local variables
C
      LOGICAL BOT
      INTEGER LENGTH
C
C     Do nothing if output is not to a tape.
C
      IF (MTUNIT.EQ.0) RETURN
C
C     Clear up from this image - the only thing needed is for the
C     buffer pointers to be initialised.
C
      FPTR=1
      BCOUNT=0
C
C     Skip back to last file mark, skip over it and write another,
C     leaving two successive file marks (a logical end of tape mark)
C     following the previous image.  If the skip back brings us to
C     the start of the tape, just write the end of tape mark.
C     Then skip back over the last file mark.
C
      CALL TIO_SKIP (MTUNIT,-1,STATUS)
      IF (.NOT.TIO_ERR(STATUS)) THEN
         BOT=TIO_SENSE(DEVICE,'BOT',STATUS)
         IF (.NOT.TIO_ERR(STATUS)) THEN
            IF (BOT) THEN
               CALL TIO_MARK(MTUNIT,STATUS)
            ELSE
               CALL TIO_SKIP(MTUNIT,1,STATUS)
            END IF
            IF (.NOT.TIO_ERR(STATUS)) THEN
               CALL TIO_MARK(MTUNIT,STATUS)
               IF (FIT_QEOT(STATUS).OR.(.NOT.TIO_ERR(STATUS))) THEN
                  CALL TIO_SKIP(MTUNIT,-1,STATUS)
               END IF
            END IF
         END IF
      END IF
C
      IF (TIO_ERR(STATUS)) THEN
         CALL TIO_GETMSG(STATUS,MTERR,LENGTH)
         MTSTAT=STATUS
      ELSE
         STATUS=0
      END IF
C
      END
