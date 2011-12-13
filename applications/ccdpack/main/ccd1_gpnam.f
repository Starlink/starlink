      SUBROUTINE CCD1_GPNAM( PARAM, GPARAM, STATUS )
*+
*  Name:
*     CCD1_GPNAM

*  Purpose:
*     Get name of global parameter associated with parameter.

*  Language:
*     Starlink Fortran 77.

*  Invocation:
*     CALL CCD1_GPNAM( PARAM, GPARAM, STATUS )

*  Description:
*     This routine maps the name of a CCDPACK parameter to the name
*     with which it is associated in the GLOBAL ADAM parameter
*     database.

*  Arguments:
*     PARAM = CHARACTER * ( * ) (Given)
*        The name of the non-global ADAM parameter.
*     GPARAM = CHARACTER * ( * ) (Returned)
*        The name of the corresponding global parameter.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     This routine does not define the associations.  Changes here must
*     also be reflected in the association fields of the interface
*     definition (.ifd) files.

*  Copyright:
*     Copyright (C) 2001 Central Laboratory of the Research Councils

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
*     MBT: Mark Taylor (STARLINK)
*     {enter_new_authors_here}

*  History:
*     2-MAY-2001 (MBT):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! Standard HDS constants

*  Arguments Given:
      CHARACTER * ( * ) PARAM

*  Arguments Returned:
      CHARACTER * ( * ) GPARAM

*  Status:
      INTEGER STATUS             ! Global status

*  Local Constants:
      INTEGER MAXNAM             ! The number of parameter names
      PARAMETER ( MAXNAM = 19 )

*  Local Variables:
      CHARACTER * ( DAT__SZNAM ) PNAMES( MAXNAM ) ! Parameter names
      CHARACTER * ( DAT__SZNAM ) GNAMES( MAXNAM ) ! Global parameter names
      INTEGER I                  ! Loop variable

*  Local Data:
      DATA PNAMES / 'ADC', 'BOUNDS', 'RNOISE', 'MASK', 'DIRECTION',
     :              'DEFERRED', 'EXTENT', 'PRESERVE', 'GENVAR',
     :              'NDFNAMES', 'USESET', 'LOGTO', 'LOGFILE', 'FLAT',
     :              'BIAS', 'CAL', 'SATURATE', 'SATURATION', 'SETSAT' /
      DATA GNAMES / 'CCDPACK_ADC', 'CCDPACK_BOUNDS', 'CCDPACK_RNOISE',
     :              'CCDPACK_MASK', 'CCDPACK_DIRECT', 'CCDPACK_DEFER',
     :              'CCDPACK_EXTENT', 'CCDPACK_PRESER',
     :              'CCDPACK_GENVAR', 'CCDPACK_NDFNAM',
     :              'CCDPACK_USESET', 'CCDPACK_LOGTO',
     :              'CCDPACK_LOGFILE', 'CCDPACK_FLAT',
     :              'CCDPACK_BIAS', 'CCDPACK_CAL', 'CCDPACK_SATUR',
     :              'CCDPACK_SATVAL', 'CCDPACK_SETSAT' /

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Loop over all known parameters.
      DO I = 1, MAXNAM

*  Check for a match.
         IF ( PARAM .EQ. PNAMES( I ) ) THEN

*  Success.  Record the corresponding global and exit the loop.
            GPARAM = GNAMES( I )
            GO TO 1
         END IF
      END DO

*  Dropped out of loop without finding a match.  Signal an error.
      STATUS = SAI__ERROR
      CALL MSG_SETC( 'PARAM', PARAM )
      CALL ERR_REP( 'CCD1_GPNAM_NONAM',
     :              'CCD1_GPNAM: Parameter ''^PARAM'' unknown', STATUS )

 1    CONTINUE

      END
* $Id$
