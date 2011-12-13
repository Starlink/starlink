      SUBROUTINE CCD1_RMULO( SVAL1, NVAL1, SVAL2, NVAL2, MATCHD,
     :                       MVALS, NRET, STATUS )
*+
*  Name:
*     CCD1_RMULO

*  Purpose:
*     Removes multiple occurences from the input lists and combines.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL CCD1_RMULO( SVAL1, NVAL1, SVAL2, NVAL2, MATCHD, MVALS,
*                      NRET, STATUS )

*  Description:
*     This routine looks at the values in SVAL1 and SVAL2 and
*     compares then for equality. Any strings which are present in
*     both lists are entered only once in MVALS, other strings are
*     entered unchanged.

*  Arguments:
*     SVAL1( NVAL1 ) = CHARACTER * ( * ) (Given)
*        First array of characters which are to be matched.
*     NVAL1 = INTEGER (Given)
*        The number of entries in SVAL1.
*     SVAL2( NVAL2 ) = CHARACTER * ( * ) (Given)
*        First array of characters which are to be matched.
*     NVAL2 = INTEGER (Given)
*        The number of entries in SVAL2.
*     MATCHD( * ) = LOGICAL (Given and Returned)
*        Workspace used to indicate which positions have a match.
*        Should be the same size as MVALS.
*     MVALS( * ) = CHARACTER * ( * ) (Returned)
*        Array of characters strings with multiple occurences removed.
*        This array must be large enough to contain all the values in
*        SVAL1 and SVAL2.
*     NRET = INTEGER (Given)
*        The number of entries in MVALS.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 1992 Science & Engineering Research Council. All
*     Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
*     02110-1301, USA

*  Authors:
*     PDRAPER: Peter Draper (STARLINK)
*     {enter_new_authors_here}

*  History:
*     15-JUL-1992 (PDRAPER):
*        Original version.
*     {enter_further_changes_here}

*  Bugs:
*     {note_new_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Arguments Given:
      INTEGER NVAL1
      INTEGER NVAL2
      CHARACTER * ( * ) SVAL1( NVAL1 )
      CHARACTER * ( * ) SVAL2( NVAL2 )

*  Arguments Given and Returned:
      LOGICAL MATCHD( * )

*  Arguments Returned:
      INTEGER NRET
      CHARACTER * ( * ) MVALS( * )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER I                  ! Loop variable
      INTEGER J                  ! Loop variable
      INTEGER NOUT               ! Number of values in list
      INTEGER NREM               ! Number of values removed from list

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Copy all values into the output array.
      DO 1 I = 1, NVAL1
         MVALS( I ) = SVAL1( I )
         MATCHD( I ) = .FALSE.
 1    CONTINUE
      NOUT = NVAL1
      DO 2 I = 1, NVAL2
         NOUT = NOUT + 1
         MVALS( NOUT ) = SVAL2( I )
         MATCHD( NOUT ) = .FALSE.
 2    CONTINUE

*  Now look for repeats, use the logical array to indicate which values
*  are matched.
      DO 3 I = 1, NOUT - 1
         DO 4 J = I + 1, NOUT
            IF ( MVALS( I ) .EQ. MVALS( J ) ) THEN
               MATCHD( J ) = .TRUE.
            END IF
 4       CONTINUE
 3    CONTINUE

*  Remove matched values from output list.
      NREM = 1
      DO 5 I = 1, NOUT
         IF ( MATCHD( I ) ) THEN

*  Shuffle list down.
            DO 6 J = I, NOUT - NREM
               MVALS( J ) = MVALS( J + 1 )
 6          CONTINUE
            NREM = NREM + 1
         END IF
 5    CONTINUE

*  Set the number of output values.
      NRET = NOUT - NREM + 1

      END
* $Id$
