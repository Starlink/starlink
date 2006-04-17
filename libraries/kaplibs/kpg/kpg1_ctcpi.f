      SUBROUTINE KPG1_CTCPI( CI, NAX, GI, NEL, OUT, STATUS )
*+
*  Name:
*     KPG1_CTCPI

*  Purpose:
*     Copy values from catalogue columns to an array.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPG1_CTCPI( CI, NAX, GI, NEL, OUT, STATUS )

*  Description:
*     This routine gets NEL values for a set of given CAT (see SUN/181) 
*     columns, derived from rows 1 to NEL of a given catalogue, 
*     selection, or index, and stores them in array OUT.

*  Arguments:
*     CI = INTEGER (Given)
*        The CAT identifier for the catalogue, selection or index
*        containing the required data.
*     NAX= INTEGER (Given)
*        The number of columns from which values are to be read.
*     GI( NAX ) = INTEGER (Given)
*        The CAT identifiers for the column, expressions or parameters to be
*        evaluated for rows 1 to NEL of the component identified by CI. If
*        any elements of this array are CAT__NOID, then the corresponding
*        elements of OUT are filled with the row number.
*     NEL = INTEGER (Given)
*        The number of rows to copy.
*     OUT( NEL, NAX ) = INTEGER (Returned)
*        The returned values.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 1998 Central Laboratory of the Research Councils.
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
*     DSB: David S. Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     26-OCT-1998 (DSB):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
      
*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'PRM_PAR'          ! VAL__ constants 
      INCLUDE 'CAT_PAR'          ! CAT constants 

*  Arguments Given:
      INTEGER CI
      INTEGER NAX
      INTEGER GI( NAX )
      INTEGER NEL

*  Arguments Returned:
      INTEGER OUT( NEL, NAX )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER I                  ! Row index
      INTEGER J                  ! Column index
      LOGICAL NULL               ! Was no value available?
*.

*  Check the inherited status. 
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Loop round each row.
      DO I = 1, NEL

*  Read the current row from the catalogue, selection or index into the
*  current row buffer.
         CALL CAT_RGET( CI, I, STATUS) 

*  Loop round each column.
         DO J = 1, NAX

*  If this column ID is valid, get the column value.
            IF( GI( J ) .NE. CAT__NOID ) THEN 
               CALL CAT_EGT0I( GI( J ), OUT( I, J ), NULL, STATUS) 
               IF( STATUS .NE. SAI__OK ) GO TO 999

*  Store a Starlink bad value if the value is null.
               IF( NULL ) OUT( I, J ) = VAL__BADI

*  If this column ID is null, store the row number. 
            ELSE
               OUT( I, J ) = I
            END IF

         END DO              

      END DO

 999  CONTINUE

      END
