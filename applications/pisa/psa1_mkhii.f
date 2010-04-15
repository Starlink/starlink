      SUBROUTINE PSA1_MKHII( ARRAY, NPIX, BAD, NHIST, HIST, MODE,
     :                       ZERO, WIDTH, STATUS )
*+
*  Name:
*     PSA1_MKHII

*  Purpose:
*     Creates an histogram of a data array.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL PSA1_MKHII( ARRAY, NPIX, BAD, NHIST, HIST, MODE, ZERO,
*                      WIDTH, STATUS )

*  Description:
*     This routine forms a histogram of an array of INTEGER data values.
*     The histogram bins convert to the original values using the formula.
*
*        VALUE = (NHIST-1)*WIDTH+ZERO

*  Arguments:
*     ARRAY( NPIX ) = INTEGER (Given)
*        Input array of values.
*     NPIX = INTEGER (Given)
*        Number of elements in input array.
*     BAD = LOGICAL (Given)
*        Whether the input array contains BAD values or not.
*     NHIST = INTEGER (Given)
*        Size of the input histogram array.
*     HIST( NHIST ) = INTEGER (Returned)
*        The histogram of the input data values.
*     MODE = INTEGER (Returned)
*        The number of the bin which contains the peak count.
*     ZERO = REAL (Returned)
*        The zero point of the histogram in data values.
*     WIDTH = REAL (Returned)
*        The width of a bin of the histogram in data values.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     PDRAPER: Peter Draper (STARLINK)
*     {enter_new_authors_here}

*  History:
*     4-NOV-1992 (PDRAPER):
*        Original version.
*     9-NOV-1992 (PDRAPER):
*        Made generic.
*     17-JUN-1995 (PDRAPER):
*        Added to PISAFIND as replacement for INTEGER*2 specific histogram
*        forming routine. Formerly in CCDPACK.
*     19-JUN-1995 (PDRAPER):
*        Removed the optimising part.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'PRM_PAR'          ! PRIMDAT constants

*  Arguments Given:
      INTEGER NPIX
      INTEGER ARRAY( NPIX )
      INTEGER NHIST

*  Arguments Returned:
      LOGICAL BAD
      INTEGER HIST( NHIST )
      INTEGER MODE
      REAL ZERO
      REAL WIDTH

*  Status:
      INTEGER STATUS            ! Global status

*  Local Variables:
      REAL SCALE                ! Data scaling factor
      INTEGER I                 ! Loop variable
      INTEGER IDIFF             ! Histogram index of current value
      INTEGER IMAX              ! Data maximum
      INTEGER IMIN              ! Data minimum
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Find the minimum and maximum values in the array.
      IMIN = VAL__MAXI
      IMAX = VAL__MINI
      DO 21 I = 1, NPIX
         IF ( ARRAY( I ) .GT. IMAX ) IMAX = ARRAY( I )
         IF ( ARRAY( I ) .LT. IMIN ) IMIN = ARRAY( I )
 21   CONTINUE

*  Check that array was not single valued.
      IF ( IMIN .EQ. IMAX ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( 'PSA1_MKHII',
     : '  Input array is single valued; cannot form histogram.',
     :   STATUS )
         GO TO 99
      END IF

*  Clear the histogram of any previous values.
      DO 11 I = 1, NHIST
         HIST( I ) = 0
 11   CONTINUE

*  Form the histogram.
      SCALE = REAL( NHIST )/ REAL( IMAX - IMIN )
      IF ( BAD ) THEN
         DO 1 I = 1, NPIX
            IF ( ARRAY( I ) .NE. VAL__BADI ) THEN
               IDIFF = NINT( SCALE * REAL( ARRAY( I ) - IMIN ) + 1.0 )
               IF ( IDIFF .GT. NHIST .OR. IDIFF .LT. 1 ) GO TO 1
               HIST( IDIFF ) = HIST( IDIFF ) + 1
            END IF
 1       CONTINUE
      ELSE
         DO 2 I = 1, NPIX
            IDIFF = NINT( SCALE * REAL( ARRAY( I ) - IMIN ) + 1.0 )
            IF ( IDIFF .GT. NHIST .OR. IDIFF .LT. 1 ) GO TO 2
            HIST( IDIFF ) = HIST( IDIFF ) + 1
 2       CONTINUE
      END IF

*  Set the initial bin width.
      WIDTH = 1.0 / SCALE

*  Set the zero point for data values.
      ZERO = REAL( IMIN ) - WIDTH / 2.0

*  Exit label.
 99   CONTINUE
C      write(11,*)nhist,width,zero
C      do i = 1, nhist
C         write(11,*)i,hist(i)
C      enddo

      END
