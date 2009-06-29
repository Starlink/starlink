      SUBROUTINE CON_LNEAR (NELM, ARRAY, LINEAR, START, INCREM, STATUS)
*+
*  Name:
*     CON_LNEAR

*  Purpose:
*     This routine checks whether an array is linear. The value of the 
*     initial element and the value of the interval between successive
*     array elements is also returned.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL CON_LNEAR (NELM, ARRAY, LINEAR, START, INCREM, STATUS)

*  Description:
*     Simply checks if an array is linear and returns the start value
*     and interval between successive elements. 
*     The tolerance is 

*  Copyright:
*     Copyright (C) 2009 Science & Technology Facilities Council. All
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

*-

C                - defined for these purposes as the difference
C                between successive elements being constant to
*                a limit calculated roughly to allow for the
*                real number precision of the machine.

*  Arguments:
*     NELM = INTEGER (Given)
*        Array size.
*     ARRAY(NELM) = REAL (Given)
*        Array to be tested.
*     LINEAR = LOGICAL (Returned)
*        True if array is linear.
*     START = REAL (Returned)
*        Value of first array element. 
*     INCREM = REAL (Returned)
*        Value of step size between successive array elements.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     JM: Jo Murray (STARLINK)
*     {enter_new_authors_here}

*  History:
*     1991 Feb 8 (JM):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
      
*  Type Definitions:
      IMPLICIT    NONE         ! No implicit typing

*  Global Constants:
      INCLUDE    'SAE_PAR'     ! Standard SAE constants
      INCLUDE    'PRM_PAR'     ! PRIMDAT constants

*  Arguments Given:
      INTEGER    NELM          ! Array size
      REAL       ARRAY(NELM)   ! Array to be tested.

*  Arguments Returned:
       LOGICAL   LINEAR        ! True if array is linear
       REAL      START         ! Value of first array element. 
       REAL      INCREM        ! Value of step size 

*  Status:
      INTEGER    STATUS        ! Global status

*  Local variables:
      INTEGER    I             ! Loop variable 
      REAL       DIFLIM        ! Tolerance used for comparing intervals
      REAL       XNEXT
      REAL       XLAST
*.

*   Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*   Initialize LINEAR to FALSE.
      LINEAR = .FALSE.

*   Set start value and incremental value from first pair of elements.
      INCREM = ARRAY(2) - ARRAY(1)
      START = ARRAY(1)

*   Check that each pair of successive elements are the same interval
*   apart as the previous pair. If not, the array is non_linear.
      XLAST = ARRAY(2)
      DO I = 3, NELM
         XNEXT = ARRAY(I)
*       The tolerance for deciding that the intervals are the same
*       is chosen as ten times the machine precison for real numbers
*       times the size of one of the array elements.
         DIFLIM = VAL__EPSR * 10.0 * MAX(ABS(XNEXT),ABS(XLAST))
         IF (ABS(XNEXT-XLAST-INCREM).GT.DIFLIM) GO TO 300
         XLAST=XNEXT
      END DO
      LINEAR = .TRUE.
  300 CONTINUE
      END
