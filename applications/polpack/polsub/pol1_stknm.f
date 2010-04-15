      SUBROUTINE POL1_STKNM( EL, CNT, DOUT, VOUT, ME, STATUS )
*+
*  Name:
*     POL1_STKNM

*  Purpose:
*     Normalize the DATA and VARIANCE arrays

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL POL1_STKNM( EL, CNT, DOUT, VOUT, ME, STATUS )

*  Description:
*     This routine normalizes the supplied arrays to hold mean data value
*     (DOUT), and squared standard error (VOUT).

*  Arguments:
*     EL = INTEGER (Given)
*        Size of each array.
*     CNT( EL ) = REAL (Given)
*        The number of input pixels at each point.
*     DOUT( EL ) = REAL (Given and Returned)
*        On entry, the sum of the input DATA values. On exit, the
*        mean DATA value. Set to VAL__BADR if there are no input pixels.
*     VOUT( EL ) = REAL (Given and Returned)
*        On entry, the sum of the squared input DATA values. On exit,
*        the square of the standard error. Set to VAL__BADR if there
*        are less than 3 input pixels.
*     ME = REAL (Returned)
*        The RMS of the standard error values.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 1999 Central Laboratory of the Research Councils

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     25-MAY-1999 (DSB):
*        Original version.
*     26-MAY-1999 (DSB):
*        Returned variance values corrected from standard error to
*        squared standard error.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'PRM_PAR'          ! VAL__ constants

*  Arguments Given:
      INTEGER EL
      REAL CNT( EL )

*  Arguments Given and Returned:
      REAL DOUT( EL )
      REAL VOUT( EL )

*  Arguments Returned:
      REAL ME

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER I                  ! Index of current input NDF
      INTEGER MEN                ! No of good standard error values
      INTEGER N                  ! No. of input pixels
      REAL M                     ! Mean of input pixels
      REAL V                     ! Vaiance of input pixels
*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Initialise.
      ME = 0.0
      MEN = 0

*  Loop round each pixel, adding the good values into the arrays.
      DO I = 1, EL

*  Check there are some input pixels.
         N = NINT( CNT( I ) )
         IF( N .GT. 0 ) THEN

*  Normalize the data sum to get the mean input pixel value.
            M = DOUT( I ) / N
            DOUT( I ) = M

*  Check there are at least 2 input pixels.
            IF( N .GE. 2 ) THEN

*  Get the population variance from the input values (i.e. divide by
*  N - 1 instead of N).
               V = ( VOUT( I ) - N*M*M )/( N - 1 )

*  Store the squared standard error.
               VOUT( I ) = V / N

*  Increment the sum of the square standard errors.
               ME = ME + VOUT( I )
               MEN = MEN + 1

            ELSE
               VOUT( I ) = VAL__BADR
            END IF

         ELSE
            DOUT( I ) = VAL__BADR
            VOUT( I ) = VAL__BADR
         END IF

      END DO

*  Return the RMS standard error.
      IF( MEN .GT. 0 ) THEN
         ME = SQRT( ME / MEN )
      ELSE
         ME = VAL__BADR
      END IF

      END
