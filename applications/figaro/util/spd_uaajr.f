      SUBROUTINE SPD_UAAJR( XSTART, XEND, NELM, XARRAY, STATUS )
*+
*  Name:
*     SPD_UAAJ{DR}

*  Purpose:
*     Fill an array linearly.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SPD_UAAJR( XSTART, XEND, NELM, XARRAY, STATUS )

*  Description:
*     This routine fills an array with values which are spaced linearly.

*  Arguments:
*     XSTART = REAL (Given)
*        Value for first element of array.
*     XEND = REAL (Given)
*        Value for last element of array.
*     NELM = INTEGER (Given)
*        The size of the array.
*     XARRAY( NELM ) = REAL (Returned)
*        The array to be filled.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     ks: Keith Shortridge (CIT)
*     hme: Horst Meyerdierks (UoE, Starlink)
*     {enter_new_authors_here}

*  History:
*     07 Feb 1984 (ks):
*        Original version.
*     27 Mar 1991 (hme):
*        ADAM prologue.
*     14 Feb 1992 (hme):
*        Generic version SPAALx.
*     18 Jun 1992 (hme):
*        No longer work step by step, which is prone to accumulating
*        rounding errors.
*     21 Jun 1994 (hme):
*        Copy from SPAAL to SPD_UAAJ.
*     24 Nov 1995 (hme):
*        Check for case of single element. In that case make increment
*        zero.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Arguments Given:
      REAL XSTART
      REAL XEND
      INTEGER NELM

*  Arguments Returned:
      REAL XARRAY( NELM )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER I
      DOUBLE PRECISION DWINC

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Work out the increment.
      IF ( NELM .GT. 1 ) THEN
         DWINC = DBLE( XEND - XSTART ) / DBLE( NELM - 1 )
      ELSE
         DWINC = 0D0
      END IF

*  Fill the array.
      DO 1 I = 1, NELM
         XARRAY(I) = XSTART + ( I - 1 ) * DWINC
 1    CONTINUE

*  Return.
      END
