      SUBROUTINE TRN_TR1R( BAD, NX, XIN, IDT, XOUT, STATUS )







*+
*  Name:
*     TRN_TR1R

*  Purpose:
*     transform 1-dimensional REAL data.

*  Language:
*     Starlink Fortran

*  Invocation:
*     CALL TRN_TR1R( BAD, NX, XIN, IDT, XOUT, STATUS )

*  Description:
*     The routine applies a compiled transformation to a set of
*     1-dimensional data points supplied as a single (X) array of
*     REAL coordinate values.

*  Arguments:
*     BAD = LOGICAL (given)
*        Whether the input coordinates may be "bad".
*     NX = INTEGER (given)
*        The number of data points to transform.
*     XIN( NX ) = REAL (given)
*        Input X coordinate values for the data points.
*     IDT = INTEGER (given)
*        ID for the compiled transformation to be applied.
*     XOUT( NX ) = REAL (returned)
*        Array to receive the transformed coordinate values.
*     STATUS = INTEGER (given & returned)
*        Inherited error status.

*  Algorithm:
*     - Call TRN_TRNR to perform the transformation.

*  Copyright:
*     Copyright (C) 1988 Science & Engineering Research Council.
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
*     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
*     02110-1301, USA

*  Authors:
*     R.F. Warren-Smith (DUVAD::RFWS)
*     {enter_new_authors_here}

*  History:
*     18-AUG-1988:  Original version (DUVAD::RFWS)
*     {enter_further_changes_here}

*  Bugs:
*     None known.
*     {note_new_bugs_here}

*-


*  Type Definitions:
      IMPLICIT NONE             ! No implicit typing


*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! DAT_ public constants


*  Arguments Given:
      LOGICAL BAD               ! Whether input coordinates may be "bad"

      INTEGER NX                ! Number of data points to transform

      REAL XIN( * )           ! Input X coordinate values

      INTEGER IDT               ! ID for the compiled transformation to
                                ! be applied

*  Arguments Returned:
      REAL XOUT( * )          ! Array for transformed coordinate
                                ! values


*  Status:
      INTEGER STATUS            ! Error status


*.



*   Check status.
      IF( STATUS .NE. SAI__OK ) RETURN


*   Transform the data points.
      CALL TRN_TRNR( BAD, NX, 1, NX, XIN, IDT, NX, 1, XOUT, STATUS )


*   Exit routine.
      END


