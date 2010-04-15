C+
      SUBROUTINE FIT_POSN (WHERE,STATUS)
C
C     F I T _ P O S N
C
C     Positions the FITS tape.  The routine FIT_INIT should have
C     been called prior to this.
C
C     Parameters -  (">" input, "W" workspace, "<" output)
C
C     (>) WHERE    (Character) Describes where the tape is to be
C                  positioned.  Only the first character is significant,
C                  and can be in upper or lower case.  At present the
C                  only recognised values are -
C                  S(tart) The tape is rewound.
C                  E(nd) The tape is positioned at the current end of
C                        data.  This should be signified by two file
C                        marks, and the tape will be left between them.
C                  Other strings will be ignored, and the tape will be
C                  left at its present position.
C     (<) STATUS   (Integer) Returned status.  0 => OK, non-zero values
C                  indicate tape I/O errors and can be decoded by
C                  FIT_ERROR.
C
C     Common variables used -
C
C     (>) MTUNIT   (Integer) The I/O channel for the tape in use.
C     (<) MTMESS   (Character) Descriptor for the last tape I/O error.
C     (<) MTSTAT   (Integer) Last TIO_ error code.
C     (W) TBUFF    (Byte array) Is the large buffer in which logical
C                  blocks (built up in FBUFF) are blocked up into actual
C                  tape records.
C     (>) MAXTBL   (Integer) Is the maximum allowed value for TBLOCK (fixed
C                  parameter).
C
C     MTMESS is defined in the file COMB.INC, the others in COMF.INC
C
C     Subroutines / functions used -
C
C     ICH_FOLD     (ICH_ package) Convert string to upper case
C     TIO_REWIND   (TIO_ package) Rewind a mag tape
C     TIO_READ     ( "      "   ) Read a record from a tape
C     TIO_SKIP     ( "      "   ) Skip over file marks on tape
C     TIO_EOF      ( "      "   ) Check for end-of-file status
C     TIO_ERR      ( "      "   ) See if status indicates an error
C
C     Note: This routine is not really of general application, since
C     it assumes that all records on the tape can be read into the
C     FITS buffer without error.  If any are longer than 28800 bytes,
C     the 'E(nd)' algorithm will fail.  Actually, there are conditions
C     under which it will fail anyway, in particular if it starts
C     with the tape positioned past the final two file marks.  A later
C     version may be more general..
C
C                                        KS / CIT 9th Oct 1983
C     Modified:
C
C     23rd Oct 1987  KS/AAO.  Now uses TIO_ routines instead of MTPCKG
C     30th Jan 1990  KS/AAO.  Modified for use with blocked tapes.
C      5th Jul 1993  KS/AAO. Unused variables removed.
C     28th Jul 1993  HME / UoE, Starlink.  Disuse STR$UPCASE.
C+
      IMPLICIT NONE
C
C     Parameters
C
      INTEGER STATUS
      CHARACTER*(*) WHERE
C
C     Common variables
C
      INCLUDE 'COMB'
      INCLUDE 'COMF'
C
C     Functions
C
      LOGICAL TIO_EOF, TIO_ERR
      INTEGER ICH_FOLD
C
C     Local variables
C
      LOGICAL ACTED,MORE
      INTEGER ACTLEN,INVOKE,LENGTH
      CHARACTER CHR*1
C
C     See where the user wants the tape
C
      CHR=WHERE(1:1)
      INVOKE=ICH_FOLD(CHR)
      ACTED=.FALSE.
      IF (CHR.EQ.'S') THEN
C
C        S(tart). Rewind tape.
C
         CALL TIO_REWIND(MTUNIT,STATUS)
         ACTED=.TRUE.
C
      ELSE IF (CHR.EQ.'E') THEN
C
C        E(nd) Search for end of tape.  This is done by skipping
C        to end of file, reading a record and checking for another
C        immediate EOF.  If no EOF is read, the action is repeated.
C
         CALL TIO_SKIP(MTUNIT,-1,STATUS)
         MORE=.TRUE.
         DO WHILE (MORE)
            CALL TIO_SKIP(MTUNIT,1,STATUS)
            IF (TIO_ERR(STATUS)) THEN
               MORE=.FALSE.
            ELSE
               CALL TIO_READ(MTUNIT,2880*MAXTBL,TBUFF,ACTLEN,STATUS)
               IF (TIO_EOF(STATUS)) THEN
                  CALL TIO_SKIP(MTUNIT,-1,STATUS)
                  MORE=.FALSE.
               ELSE IF (TIO_ERR(STATUS)) THEN
                  MORE=.FALSE.
               END IF
            END IF
         END DO
         ACTED=.TRUE.
      END IF
C
C     Check final status code
C
      IF (ACTED) THEN
         IF (TIO_ERR(STATUS)) THEN
            CALL TIO_GETMSG(STATUS,MTERR,LENGTH)
            MTSTAT=STATUS
         ELSE
            STATUS=0
         END IF
      ELSE
         STATUS=0
      END IF
C
      END
