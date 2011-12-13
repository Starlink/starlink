      SUBROUTINE CON_SAXIS( OFFSET, NPTS, AXIS, STATUS )
*+
*  Name:
*     CON_SAXIS

*  Purpose:
*     Fills one of the sky axes.

*  Language:
*     Fortran 77.

*  Invocation:
*     CALL CON_SAXIS( OFFSET, NPTS; AXIS; STATUS )

*  Description:
*     Compute one of the sky axes for a data cube.  The axis is
*     computed as an offset from a central point on the celestial sphere.

*  Arguments:
*     OFFSET  =  REAL (Given)
*        The offset between successive elements in the sky map.
*     NPTS  =  INTEGER (Given)
*        Number of points in the axis.
*     AXIS(NPTS)  =  REAL (Returned)
*        Central radial velocity of each point in the axis (km/sec).
*     STATUS  =  INTEGER (Given and Returned)
*        The global status.

*  Algorithm:
*     Compute the start offset
*     For each point in the axis
*       Compute the offset.
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
*     MJC: Malcolm J. Currie (Starlink)
*     {enter_new_authors_here}

*  History:
*     7/7/97  (ACD):
*        Original version.
*     23/7/97 (ACD):
*        First stable version.
*     2009 June 29 (MJC):
*        Used modern coding style.
*     {enter_further_changes_here}

*-

*  Type Definitions:
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard Starlink constants

*  Arguments Given:
      REAL OFFSET
      INTEGER NPTS

*  Arguments Returned:
      REAL AXIS( NPTS )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER LOOP               ! Loop index
      REAL START                 ! Start offset

*.

*  Check the global inherited status.
      IF ( STATUS .EQ. SAI__OK ) THEN

*  Compute the start offset.
         START = - ( OFFSET * REAL( ( NPTS / 2 ) + 1 ) )

*  Compute the offset for each point along the axis.
         DO LOOP = 1, NPTS
            AXIS( LOOP ) = START + ( REAL( LOOP ) * OFFSET )
         END DO

      END IF

      END
