      SUBROUTINE ARY1_EXTYP( IDCB, TYPE, STATUS )
*+
*  Name:
*     ARY1_EXTYP

*  Purpose:
*     Obtain the external numeric type of a possible scaled array.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL ARY1_EXTYP( IDCB, TYPE, STATUS )

*  Description:
*     The routine returns the numeric type of the externally visible
*     data in an array. For SIMPLE and PRIMITIVE arrays this is just the
*     numeric type of the data array. For SCALED and DELTA arrays it is
*     the numeric type of the SCALE value stored in the data object.

*  Arguments:
*     IDCB = INTEGER (Given)
*        Index of the data object entry in the DCB.
*     TYPE = CHARACTER * ( * ) (Returned)
*        External numeric type of the array.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 2010 Science & Technology Facilities Council.
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
*     DSB: David S Berry (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     5-NOV-2010 (DSB):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! DAT_ public constants
      INCLUDE 'ARY_PAR'          ! ARY_ public constants
      INCLUDE 'ARY_CONST'        ! ARY_ private constants

*  Global Variables:
      INCLUDE 'ARY_DCB'          ! ARY_ Data Control Block

*  Arguments Given:
      INTEGER IDCB

*  Arguments Returned:
      CHARACTER TYPE * ( DAT__SZTYP )

*  Status:
      INTEGER STATUS             ! Global status

*  Local variables:
      LOGICAL THERE              ! Does the SCALE component exist?

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Ensure that storage form, type and scaling information is available.
      CALL ARY1_DFRM( IDCB, STATUS )
      CALL ARY1_DTYP( IDCB, STATUS )
      CALL ARY1_DSCL( IDCB, STATUS )

*  For SCALED arrays use the data type of the SCALE value in the scaling
*  information.
      IF( DCB_FRM( IDCB ) .EQ. 'SCALED' ) THEN
         CALL CMP_TYPE( DCB_SCLOC( IDCB ), 'SCALE', TYPE, STATUS )

*  For DELTA arrays use the data type of the SCALE value in the data
*  object, if present. Otherwise use the data type of the DATA array.
      ELSE IF( DCB_FRM( IDCB ) .EQ. 'DELTA' ) THEN
         CALL DAT_THERE( DCB_LOC( IDCB ), 'SCALE', THERE, STATUS )
         IF( THERE ) THEN
            CALL CMP_TYPE( DCB_LOC( IDCB ), 'SCALE', TYPE, STATUS )
         ELSE
            TYPE = DCB_TYP( IDCB )
         END IF

*  For othe forms, use the data type of the DATA array.
      ELSE
         TYPE = DCB_TYP( IDCB )
      END IF

      END
