      SUBROUTINE CCD1_LCC( ARRIN, NREC, NVAL, NCOLIN, NCOLO, ARROUT,
     :                     STATUS )
*+
*  Name:
*     CCD1_LCC

*  Purpose:
*     Copys a column of data into another array

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL CCD1_LCC( ARRIN, NREC, NVAL, NCOLIN, NCOLO, ARROUT, STATUS )

*  Description:
*     This routine copies the specified column of data into
*     the specified output column of ARROUT.

*  Arguments:
*     ARRIN( NREC, NVAL ) = DOUBLE PRECISION (Given)
*        Array of data which is to have one column copied into
*        ARROUT.
*     NREC = INTEGER (Given)
*        First dimension of the input and output arrays.
*     NVAL =  INTEGER (Given)
*        Second dimension of the input and output arrays.
*     NCOLIN = INTEGER (Given)
*        Number of the column of data in ARRIN which is to be copied.
*     NCOLO = INTEGER (Given)
*        Number of the column into which the data is to be copied.
*     ARROUT( NREC, NVAL ) = DOUBLE PRECISION (Returned)
*        Output array containing the column of data.
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
*     23-JUL-1992 (PDRAPER):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Arguments Given:
      INTEGER NREC
      INTEGER NVAL
      DOUBLE PRECISION ARRIN( NREC, NVAL )
      INTEGER NCOLIN
      INTEGER NCOLO

*  Arguments Returned:
      DOUBLE PRECISION ARROUT( NREC, NVAL )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER I                  ! Loop variable

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Just loop transfering the data as appropriate.
      DO 1 I = 1, NREC
         ARROUT( I, NCOLIN ) = ARRIN( I, NCOLO )
 1    CONTINUE

      END
* $Id$
