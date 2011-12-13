      SUBROUTINE KPG1_SCLOF( EL, IN, FACTOR, OFFSET, OUT, BAD, STATUS )
*+
*  Name:
*     KPG1_SCLOF

*  Purpose:
*     Applies a simple scaling and base-line shift to the values
*     contained in the input vector.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPG1_SCLOF( EL, IN, FACTOR, OFFSET, OUT, NBAD, STATUS )

*  Description:
*     The input data values are multiplied by the given factor and
*     the given offset is then added on, to form the output data.

*  Arguments:
*     EL = INTEGER (Given)
*        The number of elements in the input and output vectors.
*     IN( EL ) = REAL (Given)
*        The input data vector.
*     FACTOR = DOUBLE PRECISION (Given)
*        The factor by which the input valus are scaled.
*     OFFSET = DOUBLE PRECISION (Given)
*        The offset by which the data values are shifted.
*     OUT( EL ) = REAL (Given)
*        The output data vector.
*     BAD = LOGICAL (Returned)
*        True if any bad pixels found.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 1990 Science & Engineering Research Council.
*     Copyright (C) 2004 Central Laboratory of the Research Councils.
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
*     DSB: David Berry (STARLINK)
*     MJC: Malcolm J. Currie (STARLINK)
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:

*     25-JUN-1990 (DSB):
*        Original version.
*     1990 Sep 27 (MJC):
*        Renamed from the more generic and common SCALE.
*     2004 Oct 1 (TIMJ):
*        NUM_CMN not required.
*     2008 March 19 (MJC):
*        Swap IN and EL arguments to regular order (as part of merging
*        of CONVERT's CON_SCLOF).
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'PRM_PAR'          ! Bad data values.

*  Global Variables:

*  Arguments Given:
      INTEGER  EL
      REAL IN( EL )
      DOUBLE PRECISION FACTOR
      DOUBLE PRECISION OFFSET

*  Arguments Returned:
      REAL OUT( EL )
      LOGICAL BAD

*  Status:
      INTEGER STATUS             ! Global status

*  Local variables:
      INTEGER ELEM               ! The element counter

*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

* Initialise BAD to indicate that no bad pixels have yet been found.
      BAD = .FALSE.

*  Loop round all the elements of the input vector.
      DO ELEM = 1, EL

*  If the data value is good, copy the scaled value to the output.
*  Otherwise, set the output value bad.
         IF ( IN( ELEM ) .NE. VAL__BADR ) THEN
            OUT( ELEM ) = FACTOR * IN( ELEM ) + OFFSET
         ELSE
            OUT( ELEM ) = VAL__BADR
            BAD = .TRUE.
         END IF

      END DO

      END
