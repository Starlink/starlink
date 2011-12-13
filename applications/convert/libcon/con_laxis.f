      SUBROUTINE CON_LAXIS (ZEROPT, SCALE, NPTS, AXIS, STATUS)
*+
*  Name:
*     CON_LAXIS

*  Purpose:
*     Compute a linear axis.

*  Language:
*     Fortran 77.

*  Invocation:
*     CALL CON_LAXIS (ZEROPT, SCALE, NPTS; AXIS; STATUS)

*  Description:
*     Compute a linear axis, given a zero point and a scale factor.

*  Arguments:
*     ZEROPT  =  REAL (Given)
*        Zero point.
*     SCALE  =  REAL (Given)
*        Scale factor.
*     NPTS  =  INTEGER (Given)
*        Number of points in the axis.
*     AXIS(NPTS)  =  REAL (Returned)
*        Central radial velocity of each point in the axis (km/sec).
*     STATUS  =  INTEGER (Given and Returned)
*        The global status.

*  Algorithm:
*     For each point in the axis
*       Compute the axis value.
*     end for

*  Copyright:
*     Copyright (C) 1997 Central Laboratory of the Research Councils.
*     All Rights Reserved.

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
*     Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
*     02110-1301, USA.

*  Authors:
*     ACD: A C Davenhall (Edinburgh)
*     MJC: Malcolm J. Currie (STARLINK)
*     {enter_new_authors_here}

*  History:
*     3/9/97 (ACD):
*        Original version.
*     2009 June 29 (MJC):
*        Used modern coding style.
*     {enter_further_changes_here}

*-
*  Type Definitions:
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard Starlink constants

*  Arguments Given:
      REAL ZEROPT
      REAL SCALE
      INTEGER NPTS

*  Arguments Returned:
      REAL AXIS( NPTS )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER LOOP               ! Loop index

*.

*  Check the inherited status.
      IF ( STATUS .EQ. SAI__OK ) THEN

*  Compute the value for each point along the axis.
         DO LOOP = 1, NPTS
            AXIS( LOOP ) = ZEROPT + ( REAL( LOOP - 1 ) * SCALE )
         END DO

      END IF

      END
