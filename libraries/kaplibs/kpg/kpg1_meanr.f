      SUBROUTINE KPG1_MEANR( N, DATA, MEAN, STATUS )
*+
*  Name:
*     KPG1_MEANx
 
*  Purpose:
*     Finds the mean of the good data values in an array.
 
*  Description:
*     Finds the mean of the good data values in a 1-D array.

*  Language:
*     Starlink Fortran 77
 
*  Invocation
*     CALL KPG1_MEANx( N, DATA, MEAN, STATUS )
 
*  Arguments:
*     N = INTEGER (Given)
*        Number of elements in the array.
*     DATA( N ) = ? (Given)
*        The data array.
*     MEAN = ? (Returned)
*        The mean value.  Set to VAL__BADR if no good data are found.
*     STATUS = INTEGER (Given)
*        The global status.
 
*  Notes:
*     -  There is a routine for each of the standard numeric types.
*     Replace "x" in the routine name by D, R, I, W, UW, B or UB as
*     appropriate.    The array and mean arguments supplied to the
*     routine must have the data type specified.
*     -  The summation is carried out in double precision.
 
*  Copyright:
*     Copyright (C) 1995 Central Laboratory of the Research Councils.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of
*     the License, or (at your option) any later version.
*     
*     This program is distributed in the hope that it will be
*     useful,but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*     
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 59 Temple Place,Suite 330, Boston, MA
*     02111-1307, USA

*  Authors:
*     DSB: D.S. Berry (STARLINK)
*     MJC: Malcolm J. Currie (STARLINK)
*     {enter_new_authors_here}
 
*  History:
*     6-JAN-1995 (DSB):
*        Original version.
*     1995 March 30 (MJC):
*        Made generic from KPG1_MEANx.  Added prologue terminator.
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
      INTEGER N
      REAL DATA( N )
 
*  Arguments Returned:
      REAL MEAN
 
*  Status:
      INTEGER STATUS             ! Global status
 
*  Local Variables:
      INTEGER I                  ! Loop count
      INTEGER NSUM               ! Number of good pixels found so far
      DOUBLE PRECISION SUM       ! Sum of good data values
 
*  Internal References:
      INCLUDE 'NUM_DEC_CVT'      ! NUM_ type conversion declarations
      INCLUDE 'NUM_DEF_CVT'      ! NUM_ type conversion functions
 
*.
 
*  Check the global inherited status.
      IF ( STATUS .NE. SAI__OK ) RETURN
 
*  Initialise the running totals.
      SUM = 0.0D0
      NSUM = 0
 
*  Loop round the data array, summing all the good data values, and
*  keeping a count of the number of good values found.
      DO I = 1, N
 
         IF ( DATA( I ) .NE. VAL__BADR ) THEN
            SUM = SUM + NUM_RTOD( DATA( I ) )
            NSUM = NSUM + 1
         END IF
 
      END DO
 
*  If any good values were found, find the mean of them.  Otherwise,
*  return a bad value.
      IF ( NSUM .GT. 0 ) THEN
         MEAN = NUM_DTOR( SUM / DBLE( NSUM ) )
 
      ELSE
         MEAN = VAL__BADR
 
      END IF
 
      END
