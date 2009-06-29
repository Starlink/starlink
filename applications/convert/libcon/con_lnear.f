      SUBROUTINE CON_LNEAR( NELM, ARRAY, LINEAR, START, INCREM, STATUS )
*+
*  Name:
*     CON_LNEAR

*  Purpose:
*     Checks whether an array is linear.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL CON_LNEAR (NELM, ARRAY, LINEAR, START, INCREM, STATUS)

*  Description:
*     This routine checks if an array's values vary from element to
*     element linearly, i.e the increment between adjacent elements
*     is a constant within a tolerance.  If this is the case, the
*     routine returns the start value and interval between successive
*     elements.  The tolerance is governed by the floating-point
*     machine precision.

*  Arguments:
*     NELM = INTEGER (Given)
*        Array size.
*     ARRAY( NELM ) = REAL (Given)
*        Array to be tested.
*     LINEAR = LOGICAL (Returned)
*        This is .TRUE. if the array is linear.
*     START = REAL (Returned)
*        Value of first array element.  This is set the bad value when
*        LINEAR is .FALSE..
*     INCREM = REAL (Returned)
*        Value of step size between successive array elements.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 1991 Science & Engineering Research Council. All
*     Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either Version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
*     02111-1307, USA.

*  Authors:
*     JM: Jo Murray (STARLINK)
*     MJC: Malcolm J. Currie (STARLINK)
*     {enter_new_authors_here}

*  History:
*     1991 Feb 8 (JM):
*        Original version.
*     2009 June 29 (MJC):
*        Used modern coding style.  Initialised returned variables and
*        returned a more-accurate increment.
*     {enter_further_changes_here}

*-
      
*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'PRM_PAR'          ! PRIMDAT constants

*  Arguments Given:
      INTEGER NELM
      REAL ARRAY( NELM )

*  Arguments Returned:
       LOGICAL LINEAR
       REAL START
       REAL INCREM

*  Status:
      INTEGER STATUS             ! Global status

*  Local variables:
      INTEGER I                  ! Loop variable
      REAL DIFLIM                ! Tolerance used for comparing intervals
      REAL XNEXT                 ! Next array value
      REAL XLAST                 ! Previous array value

*.

*  Initialize LINEAR to FALSE.
      LINEAR = .FALSE.
      INCREM = VAL__BADR
      START = VAL__BADR

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Set start value and incremental value from first pair of elements.
      INCREM = ARRAY( 2 ) - ARRAY( 1 )
      START = ARRAY( 1 )

*  Check that each pair of successive elements are the same interval
*  apart as the previous pair.  If not, the array is non-linear.
      XLAST = ARRAY( 2 )
      DO I = 3, NELM
         XNEXT = ARRAY( I )

*  The tolerance for deciding that the intervals are the same is chosen
*  as ten times the machine precision for real numbers times the size of
*  one of the array elements.
         DIFLIM = VAL__EPSR * 10.0 * MAX( ABS( XNEXT ), ABS( XLAST ) )
         IF ( ABS( XNEXT - XLAST- INCREM ) .GT. DIFLIM ) GO TO 300
         XLAST = XNEXT
      END DO
      LINEAR = .TRUE.

*  Recompute the increment over the full baseline for improved accuracy.
      INCREM = ( ARRAY( NELM ) - ARRAY( 1 ) ) / REAL( NELM - 1 )

  300 CONTINUE

      END
