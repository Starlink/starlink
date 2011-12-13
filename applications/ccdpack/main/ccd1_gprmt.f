      SUBROUTINE CCD1_GPRMT( PARAM, PROMPT, STATUS )
*+
*  Name:
*     CCD1_GPRMT

*  Purpose:
*     Get the prompt used for a global parameter.

*  Language:
*     Starlink Fortran 77.

*  Invocation:
*     CALL CCD1_GPRMT( PARAM, PROMPT, STATUS )

*  Description:
*     This routine maps the name of a CCDPACK parameter to a suitable
*     prompt string.  It is intended for use by CCD1_KPLD, which has
*     to modify the prompt strings for global keyed parameters, so
*     only prompts for those parameters which may be keyed need to
*     be known by this routine.

*  Arguments:
*     PARAM = CHARACTER * ( * ) (Given)
*        The non-global name of the ADAM parameter.
*     PROMPT = CHARACTER * ( * ) (Returned)
*        The name of the corresponding prompt.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     This routine does not define the prompts; changes here should
*     be reflected in the prompt fields of the interface definition
*     (.ifd) files.

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
*     14-MAY-2001 (MBT):
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
      CHARACTER * ( * ) PROMPT

*  Status:
      INTEGER STATUS             ! Global status

*  Local Constants:
      INTEGER MAXNAM             ! The number of parameter names
      PARAMETER ( MAXNAM = 8 )

*  Local Variables:
      CHARACTER * ( DAT__SZNAM ) PNAMES( MAXNAM ) ! Parameter names
      CHARACTER * ( 80 ) PROMPS( MAXNAM ) ! Prompt strings
      INTEGER I                  ! Loop variable

*  Local Data:
      DATA PNAMES / 'ADC',
     :              'BOUNDS',
     :              'DEFERRED',
     :              'DIRECTION',
     :              'EXTENT',
     :              'MASK',
     :              'RNOISE',
     :              'SATURATION' /
      DATA PROMPS / 'Number of electrons per ADU',
     :              'Pixel indices of bias strips (in pairs)',
     :              'Deferred charge (ADUs)',
     :              'Readout direction (X or Y)',
     :              'Useful CCD region (xmin,xmax,ymin,ymax)',
     :              'Mask data file',
     :              'Readout noise (ADUs)',
     :              'Saturation value' /

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Loop over all known parameters.
      DO I = 1, MAXNAM

*  Check for a match.
         IF ( PARAM .EQ. PNAMES( I ) ) THEN

*  Success.  Record the corresponding global and exit the loop.
            PROMPT = PROMPS( I )
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
