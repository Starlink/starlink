      SUBROUTINE CCD1_LXYT( ARRIN, NDEC, NREC, NVAL, TR, ARROUT,
     :                      STATUS )
*+
*  Name:
*     CCD1_LXYT

*  Purpose:
*     Transforms the X and Y positions of an array using a linear
*     transformation.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL CCD1_LXYT( ARRIN, NDEC, NREC, NVAL, TR, ARROUT, STATUS )

*  Description:
*     The routine transforms the first two columns within the the ARRIN
*     array using a linear transformation whose coefficients are
*     specified by the TR array. The output array contains the
*     transformed positions and any other values which are present in
*     the input array.

*  Arguments:
*     ARRIN( NDEC, NVAL ) = DOUBLE PRECISION (Given)
*        Array which contains the positions which require
*        transformation.
*     NDEC = INTEGER (Given)
*        First dimension of the ARRIN array when it was declared in the
*        calling routine.
*     NREC = INTEGER (Given)
*        Number of records (rows) in ARRIN which are to transformed.
*     NVAL = INTEGER (Given)
*        Number of values (columns) in each record of ARRIN.
*     TR( 6 ) = DOUBLE PRECISION (Given)
*        The transformation coefficients. The transformation is:
*
*        XDASH = TR( 1 ) + TR( 2 ) * X + TR( 3 ) * Y
*        YDASH = TR( 4 ) + TR( 5 ) * X + TR( 6 ) * Y
*     ARROUT( NREC, NVAL ) = DOUBLE PRECISION (Returned)
*        The output array which contains the transformed positions and a
*        copy of any other data which was present in ARRIN.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     -  The linear transform used by this routines is:
*        XDASH = TR( 1 ) + TR( 2 ) * X + TR( 3 ) * Y
*        YDASH = TR( 4 ) + TR( 5 ) * X + TR( 6 ) * Y

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
*     22-JUL-1992 (PDRAPER):
*        Original version.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Arguments Given:
      INTEGER NDEC
      INTEGER NREC
      INTEGER NVAL
      DOUBLE PRECISION ARRIN( NDEC, NVAL )
      DOUBLE PRECISION TR( 6 )

*  Arguments Returned:
      DOUBLE PRECISION ARROUT( NREC, NVAL )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER I
      INTEGER J
      DOUBLE PRECISION X
      DOUBLE PRECISION Y
      DOUBLE PRECISION XDASH
      DOUBLE PRECISION YDASH

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Transform the positions and copy the extra values into ARROUT.
      DO 1 I = 1, NREC
         X = ARRIN( I, 1 )
         Y = ARRIN( I, 2 )
         XDASH = TR( 1 ) + TR( 2 ) * X + TR( 3 ) * Y
         YDASH = TR( 4 ) + TR( 5 ) * X + TR( 6 ) * Y
         ARROUT( I, 1 ) = XDASH
         ARROUT( I, 2 ) = YDASH
         IF ( NVAL .GT. 2 ) THEN
            DO 2 J = 3, NVAL
               ARROUT( I, J ) = ARRIN( I, J )
 2          CONTINUE
         END IF
 1    CONTINUE

      END
* $Id$
