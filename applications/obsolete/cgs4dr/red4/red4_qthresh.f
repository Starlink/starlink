*+  RED4_QTHRESH - Flag as "bad" any data outside a given range
      SUBROUTINE RED4_QTHRESH( DIM, INPUT, TLOW, THIGH, GOOD, BAD,
     :  MASK, NGOOD, NBAD, STATUS )
*    Description :
*     This routine examines the INPUT array and sets the corresponding
*     elements in the MASK array to GOOD if the value lies within the
*     specified range (TLOW-THIGH), or BAD if the value is outside
*     the range. Only the least significant byte of GOOD and BAD are
*     used.
*    Invocation :
*     CALL RED4_QTHRESH( DIM, INPUT, TLOW, THIGH, GOOD, BAD,
*     :  MASK, NGOOD, NBAD, STATUS )
*    Parameters :
*     DIM          = INTEGER( READ )
*           The size of the INPUT and MASK arrays. (They are treated
*           as 1-D arrays, but may be any shape in reality).
*     INPUT( DIM ) = REAL( READ )
*           The input array
*     TLOW         = REAL( READ )
*           Minimum acceptable value
*     THIGH        = REAL( READ )
*           Maximum acceptable value
*     GOOD         = INTEGER( READ )
*           Value to write into MASK element when data falls within range.
*           Only the least significant byte is used, so values outside
*           the range 0-127 are effectively modulo 127.
*     BAD          = INTEGER( READ )
*           Value to write into MASK element when data falls outside range.
*           Only the least significant byte is used, so values outside
*           the range 0-127 are effectively modulo 127.
*     MASK( DIM )  = UBYTE( WRITE )
*           The mask array generated.
*     NGOOD        = INTEGER( WRITE )
*           The number of good pixels found.
*     NBAD         = INTEGER( WRITE )
*           The number of bad pixels found.
*     STATUS       = INTEGER( UPDATE )
*           Global ADAM status
*    Method :
*    Deficiencies :
*     EQUIVALENCE is used to extract the least significant byte from
*     GOOD and BAD. This is rather machine-specific.
*    Bugs :
*     <description of any "bugs" which have not been fixed>
*    Authors :
*     Steven Beard (REVAD::SMB)
*     Phil Daly (JACH::PND)
*    History :
*     15-Feb-1990: Original version.               (SMB)
*     16-Feb-1990: NGOOD and NBAD counters added.  (SMB)
*     22-Feb-1993: Conform to error strategy       (PND)
*     02-Nov-1995: remove equivalence              (PND)
*    endhistory
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'ADAMDEFNS'
      INCLUDE 'ADAMERRS'
*    Import :
      INTEGER
     :  DIM,               ! Size of arrays
     :  GOOD,              ! Data quality value for "good"
     :  BAD                ! Data quality value for "bad"
      REAL
     :  INPUT( DIM ),      ! Input array
     :  TLOW,              ! Minimum acceptable value
     :  THIGH              ! Maximum acceptable value
*    Export :
      BYTE
     :  MASK( DIM )        ! Data quality mask
      INTEGER
     :  NGOOD,             ! Number of good pixels
     :  NBAD               ! Number of bad pixels
*    Status :
      INTEGER STATUS
      INTEGER
     :  I                  ! Loop counter
*-

*   Check for error on entry
      IF ( STATUS .NE. ADAM__OK ) RETURN

*   Initialise the good and bad counters
      NGOOD = 0
      NBAD = 0

*   Scan through all the elements of the input array.
      DO I = 1, DIM

*      If this element is within the allowed range, set the mask
*      to GOOD. Otherwise set it to BAD.
         IF ( ( INPUT( I ) .GE. TLOW ) .AND.
     :        ( INPUT( I ) .LE. THIGH ) ) THEN

            MASK( I ) = MIN( MAX ( -128, GOOD ), 127 )
            NGOOD = NGOOD + 1
         ELSE

            MASK( I ) = MIN( MAX( -128, BAD ), 127 )
            NBAD = NBAD + 1
         END IF
      END DO

      END
