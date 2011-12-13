      SUBROUTINE KPG1_KGODR( VAR, NPTS, INARR, INVAR, NGOOD, OUTARR,
     :                         OUTVAR, STATUS )
*+
*  Name:
*     KPG1_KGODx

*  Purpose:
*     Sorts through a dataset and throw away bad values.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPG1_KGODx( VAR, NPTS, INARR, INVAR, NGOOD, OUTARR, OUTVAR,
*                      STATUS )

*  Description:
*     This routine copies good data and its variance from input arrays
*     to to output arrays, leaving behind bad values.

*  Arguments:
*     VAR = LOGICAL (Given)
*        If .TRUE., copy the variance array as well, else only copy
*        the data array.
*     NPTS = INTEGER (Given)
*        The number of points in the input array.
*     INARR( NPTS ) = ? (Given)
*        Input data array.
*     INVAR( NPTS ) = ? (Given)
*        Input variance array.
*     NGOOD = INTEGER (Returned)
*        Number of good points in the input data.
*     INARR( NPTS ) = ? (Returned)
*        Output data array.
*     INVAR( NPTS ) = ? (Returned)
*        Output variance array.
*     STATUS = INTEGER (Given & Returned)
*        Global status value.

*  Notes:
*     -  Data are still good even if variance is bad.  Put another way,
*     when the data is good, its corresponding variance is copied
*     regardless of its value.
*     -  There is a routine for all numeric data types: replace "x" in
*     the routine name by B, D, I, R, UB, UW, or W as appropriate.  The
*     arrays supplied to the routine must have the data type specified.

*  Copyright:
*     Copyright (C) 1996, 1997 Central Laboratory of the Research
*                   Councils.
*     All Rights Reserved.

*  Licence:
*     This programme is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either Version 2 of
*     the License, or (at your option) any later version.
*
*     This programme is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE.  See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this programme; if not, write to the Free Software
*     Foundation, Inc., 51, Franklin Street, Fifth Floor, Boston, MA
*     02110-1301, USA.

*  Authors:
*     TIMJ: Tim Jenness (JACH)
*     MJC: Malcolm J. Currie (STARLINK)
*     {enter_new_authors_here}

*  History:
*     1996 September (TIMJ):
*       Original version
*     1997 May 13 (MJC):
*        Made generic, and some tidying.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! SSE global definitions
      INCLUDE 'PRM_PAR'          ! VAL__ constants

*  Arguments Given
      LOGICAL VAR
      INTEGER NPTS
      REAL INARR( NPTS )
      REAL INVAR( NPTS )

*  Arguments Returned:
      INTEGER NGOOD
      REAL OUTARR( NPTS )
      REAL OUTVAR( NPTS )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER I                  ! Loop counter

*.

*  Check the global inherited status.
      IF ( STATUS .NE. SAI__OK ) RETURN

      NGOOD = 0

      DO I = 1, NPTS

         IF ( INARR( I ) .NE. VAL__BADR ) THEN
            NGOOD = NGOOD + 1
            OUTARR( NGOOD ) = INARR( I )
            IF ( VAR ) OUTVAR( NGOOD ) = INVAR( I )
         END IF
      END DO

      END
