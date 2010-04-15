*+  RED4_QWINDOW - Flag as "bad" any data outside a given window
      SUBROUTINE RED4_QWINDOW( DIM1, DIM2, IMIN, IMAX, JMIN, JMAX,
     :  GOOD, BAD, MASK, NGOOD, NBAD, STATUS )
*    Description :
*     This routine sets elements in the MASK array to GOOD if they
*     lie within the boundaries of the specified window
*     MASK( IMIN:IMAX, JMIN:JMAX ) and BAD if they lie outside.
*     Only the least significant byte of GOOD and BAD are used.
*    Invocation :
*      CALL RED4_QWINDOW( DIM1, DIM2, IMIN, IMAX, JMIN, JMAX,
*     :  GOOD, BAD, MASK, NGOOD, NBAD, STATUS )
*    Parameters :
*     DIM1                = INTEGER( READ )
*           The first dimension of the MASK array
*     DIM2                = INTEGER( READ )
*           The second dimension of the MASK array
*     IMIN                = INTEGER( READ )
*           The lowest column number of the MASK array which is inside
*           the window.
*     IMAX                = INTEGER( READ )
*           The highest column number of the MASK array which is inside
*           the window.
*     JMIN                = INTEGER( READ )
*           The lowest row number of the MASK array which is inside
*           the window.
*     JMAX                = INTEGER( READ )
*           The highest row number of the MASK array which is inside
*           the window.
*     GOOD                = INTEGER( READ )
*           Value to write into MASK element when data falls within range.
*           Only the least significant byte is used, so values outside
*           the range 0-255 are effectively modulo 255.
*     BAD                 = INTEGER( READ )
*           Value to write into MASK element when data falls outside range.
*           Only the least significant byte is used, so values outside
*           the range 0-255 are effectively modulo 255.
*     MASK( DIM1, DIM2 )  = UBYTE( WRITE )
*           The mask array generated.
*     NGOOD               = INTEGER( WRITE )
*           The number of good pixels found.
*     NBAD                = INTEGER( WRITE )
*           The number of bad pixels found.
*     STATUS              = INTEGER( UPDATE )
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
*     23-May-1990: Original version.               (SMB)
*     22-Feb-1993: Conform to error strategy       (PND)
*     02-Nov-1995: Remove equivalence              (PND)
*    endhistory
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'ADAMDEFNS'
      INCLUDE 'ADAMERRS'
*    Import :
      INTEGER
     :  DIM1,              ! First dimension of MASK array
     :  DIM2,              ! Second dimension of MASK array
     :  IMIN,              ! Minimum column number for "good"
     :  IMAX,              ! Maximum column number for "good"
     :  JMIN,              ! Minimum row number for "good"
     :  JMAX,              ! Maximum row number for "good"
     :  GOOD,              ! Data quality value for "good"
     :  BAD                ! Data quality value for "bad"
*    Export :
      BYTE
     :  MASK( DIM1, DIM2 ) ! Data quality mask
      INTEGER
     :  NGOOD,             ! Number of good pixels
     :  NBAD               ! Number of bad pixels
*    Status :
      INTEGER STATUS
      INTEGER
     :  I, J               ! Loop counters
*-

*   Check for error on entry
      IF ( STATUS .NE. ADAM__OK ) RETURN

*   Scan though the completely bad rows below the window at the
*   bottom of the mask and set all the columns in these rows to "bad".
      DO J = 1, JMIN-1
         DO I = 1, DIM1

            MASK( I, J ) = MIN( MAX( -128, BAD ), 127 )
         END DO
      END DO

*   Scan through the partially bad rows in the middle of the mask.
      DO J = JMIN, JMAX

*      Set the columns to the left of the window to "bad"
         DO I = 1, IMIN-1

            MASK( I, J ) = MIN( MAX( -128, BAD ), 127 )
         END DO

*      Set the columns within the window to "good"
         DO I = IMIN, IMAX

            MASK( I, J ) = MIN( MAX( -128, GOOD ), 127 )
         END DO

*      Set the columns to the right of the window to "bad"
         DO I = IMAX+1, DIM1

            MASK( I, J ) = MIN( MAX( -128, BAD ), 127 )
         END DO
      END DO

*   Scan though the completely bad rows above the window at the
*   top of the mask and set all the columns in these rows to "bad".
      DO J = JMAX+1, DIM2
         DO I = 1, DIM1

            MASK( I, J ) = MIN( MAX( -128, BAD ), 127 )
         END DO
      END DO

*   The number of good values will simply be the area of the window.
      NGOOD = (IMAX - IMIN + 1) * (JMAX - JMIN + 1)

*   The number of bad values will be the total area of the mask
*   with the number of good values subtracted.
      NBAD = (DIM1 * DIM2) - NGOOD

      END
