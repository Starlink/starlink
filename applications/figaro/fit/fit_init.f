C+
      SUBROUTINE FIT_INIT (TAPE,STATUS)
C
C     F I T _ I N I T
C
C     Initialises the FIT_ routines.  This should be the first
C     FIT_ routine called (unless the data is to be written to
C     disk instead of tape, in which case FIT_DINIT should be used)
C
C     Parameters -   (">" input, "<" output)
C
C     (>) TAPE     (Character) The name of the tape drive to be
C                  used.  Can be either the actual name, eg '_MTA0:',
C                  or an equivalent logical name.
C     (<) STATUS   (Integer) A returned status code.  0 => OK, non-
C                  zero values are tape I/O error codes, which may be
C                  decoded via FIG_MTERR.
C
C     Common variables used -
C
C     (<) MTUNIT   (Integer) The I/O channel used to access the tape.
C     (<) FPTR     (Integer) The pointer to the main output buffer.
C     (<) MTERR    (Character) Descriptor of last tape I/O error.
C     (<) MTSTAT   (Integer) Last TIO_ error code
C     (<) LUFILE   (Integer) Logical unit number for disk file, if used.
C     (<) NOSWAP   (Logical) Used to suppress byte swapping.
C     (<) NOTERM   (Logical) Used to suppress proper termination of the tape.
C     (<) DEVICE   (Character) Output device name.
C     (<) TBLOCK   (Integer) Is the current blocking factor - number of
C                  logical records in each actual tape record.
C     (<) BCOUNT   (Integer) Is the number of logical records already
C                  held in TBUFF.
C     (>) TDENS    (Integer) Is the tape density.
C
C     MTERR,DEVICE defined in the file COMB.INC, the others in COMF.INC
C
C     Subroutines / functions used -
C
C     TIO_OPEN     (TIO_ package) Open a mag tape.
C     TIO_GETMSG   ( "      "   ) Decode a TIO_ error code.
C     TIO_ERR      ( "      "   ) See if status code indicates an error
C     TIO_SETDEN   ( "      "   ) Sets the tape density
C     TIO_SENSE    ( "      "   ) Gets information about the tape
C
C                                          KS / CIT 14th June 1984
C     Modified:
C
C     22nd Oct 1987.  Now uses the TIO_ library, rather than MTPCKG.
C     28th Oct 1987.  Now sets DEVICE in common.
C     30th Jan 1990.  Now initialises TBLOCK and BCOUNT as well, and
C                     mounts the tape if necessary.  KS/AAO.
C     27th Jun 1990.  Now controls the tape density following a dismount.
C                     KS/AAO.
C      5th Mar 1993.  Now sets the NOTERM flag. KS/AAO.
C+
      IMPLICIT NONE
C
C     Parameters
C
      INTEGER STATUS
      CHARACTER*(*) TAPE
C
C     Functions
C
      LOGICAL TIO_ERR, TIO_SENSE
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
C     Mount the tape if necessary.
C
      CALL TIO_MOUNT(TAPE,STATUS)
      IF (TIO_ERR(STATUS)) THEN
         CALL TIO_GETMSG(STATUS,MTERR,LENGTH)
         MTSTAT=STATUS
         GO TO 500
      END IF
C
C     Open the tape
C
      CALL TIO_OPEN(TAPE,MTUNIT,STATUS)
      IF (TIO_ERR(STATUS)) THEN
         CALL TIO_GETMSG(STATUS,MTERR,LENGTH)
         MTSTAT=STATUS
      ELSE
C
C        Tape has opened OK.  Now for a messy bit to do with the density.
C        If the density is already set, this is almost certainly because
C        a tape has already been used but has been dismounted.  If so,
C        (and if the tape is at the load point), assume that the same density
C        should be used for the new tape - VMS sets drives back to a default
C        density under these circumstances.  Note that the density value is
C        cleared when FIT_END is called, but is set by FIT_DISMT.
C        Ignore the status here - not all tapes have densities (eg Exabytes)
C        so as long as the tape opened, return OK status.
C
         IF ((TDENS.EQ.800).OR.(TDENS.EQ.1600).OR.(TDENS.EQ.6250)) THEN
            IF (TIO_SENSE(TAPE,'BOT',STATUS)) THEN
               CALL TIO_SETDEN(MTUNIT,TDENS,STATUS)
            END IF
         END IF
         STATUS=0
      END IF
C
C     Initialise pointer and block count, and set default blocking factor
C
      FPTR=1
      BCOUNT=0
      TBLOCK=1
C
C     Clear disk logical unit number and set the flag to enable swapping
C     Clear the flag that disables proper termination of the tape.
C
      LUFILE=0
      NOSWAP=.FALSE.
      NOTERM=.FALSE.
C
C     Record name of device in use
C
      DEVICE=TAPE
C
C     Exit
C
  500 CONTINUE
C
      END
