      SUBROUTINE POL1_SNGMN( EL, NIN, MNFRAC, NOUT, DOUT,
     :                       VOUT, COUT, STATUS )
*+
*  Name:
*     POL1_SNGMN

*  Purpose:
*     Remove any output pixels in a single-beam Stokes cube which are
*     contributed to by too few input pixels.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL POL1_SNGMN( EL, NIN, MNFRAC, NOUT, DOUT, VOUT, COUT,
*                      STATUS )

*  Description:
*     This routine sets pixels in the supplied Stokes cubes bad if they
*     were calculated on the basis of too few input pixels.

*  Arguments:
*     EL = INTEGER (Given)
*        The number of pixels in the arrays.
*     NIN( EL ) = INTEGER (Given)
*        An array in which each element gives the number of input images
*        which had good values at the corresponding element, BEFORE any
*        bad data has been rejected from the input images.
*     MNFRAC = REAL (Given)
*        The minimum fraction of good input images which must be retained
*        in order to create a good output pixel value. The number of images
*        is rounded to the nearest integer, and limited to be at least 3.
*     NOUT( EL ) = REAL (Given)
*        An array in which each element gives the number of input images
*        which had good values at the corresponding element, AFTER any
*        bad data has been rejected from the input images.
*     DOUT( EL, 3 ) = REAL (Given and Returned)
*        The output Stokes vectors. Plane 1 holds I, plane 2 holds Q
*        and plane 3 holds U.
*     VOUT( EL, 3 ) = REAL (Given and Returned)
*        The output variance values.
*     COUT( EL ) = REAL (Given and Returned)
*        The output QU co-variance values.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 1999 Central Laboratory of the Research Councils
*     Copyright (C) 2009 Science & Technology Facilities Council.
*     All Rights Reserved.

*  Authors:
*     DSB: David Berry (STARLINK)
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     13-MAY-1999 (DSB):
*        Original version.
*     31-JUL-2009 (TIMJ):
*        Remove ILEVEL. Use MSG filtering.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'PRM_PAR'          ! VAL__ constants
      INCLUDE 'MSG_PAR'          ! MSG__ constants

*  Arguments Given:
      INTEGER EL
      INTEGER NIN( EL )
      REAL MNFRAC
      REAL NOUT( EL )

*  Arguments Given and Returned:
      REAL DOUT( EL, 3 )
      REAL VOUT( EL, 3 )
      REAL COUT( EL )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER BUF*10           ! Buffer for output message text
      INTEGER I                  ! Loop index
      INTEGER NREJ               ! No of rejected output pixels
*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Initialise the number of rejected pixels.
      NREJ = 0

*  Check each pixel.
      DO I = 1, EL

*  If too few good input images remained when the Stokes vector was
*  calculated, set the Stokes vector bad.
         IF( NINT( NOUT( I ) ) .LT.
     :       NINT( MNFRAC*REAL( NIN( I ) ) ) ) THEN
            DOUT( I, 1 ) = VAL__BADR
            DOUT( I, 2 ) = VAL__BADR
            DOUT( I, 3 ) = VAL__BADR
            VOUT( I, 1 ) = VAL__BADR
            VOUT( I, 2 ) = VAL__BADR
            VOUT( I, 3 ) = VAL__BADR
            COUT( I ) = VAL__BADR

            NREJ = NREJ + 1

         END IF

      END DO

*  If required display the number of rjected pixels.
      CALL MSG_BLANKIF( MSG__VERB, STATUS )

      IF( NREJ .GT. 0 ) THEN
         WRITE( BUF, '(F5.1)' ) 100*REAL( NREJ )/REAL( EL )
         CALL CHR_LDBLK( BUF )
         CALL MSG_SETC( 'PERC', BUF )
         CALL MSG_OUTIF( MSG__VERB, 'POL1_SNGMN_MSG1',
     :        ' ^PERC % of the output '//
     :        'pixels failed the test specified by '//
     :        'parameter MINFRAC.', STATUS )
      ELSE
         CALL MSG_OUTIF( MSG__VERB, 'POL1_SNGMN_MSG1',
     :        ' None of the output '//
     :        'pixels failed the test specified by '//
     :        'parameter MINFRAC.', STATUS )
      END IF

      CALL MSG_BLANKIF( MSG__VERB, STATUS )

      END
