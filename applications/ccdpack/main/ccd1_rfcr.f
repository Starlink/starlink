      SUBROUTINE CCD1_RFCR( IDFLT, SETSAT, SATVAL, EXTSAT, IDOUT,
     :                      FTYPE, DTYPE, STATUS )
*+
*  Name:
*     CCD1_RFCR

*  Purpose:
*     To report the parameters used in a run of FLATCOR.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL CCD1_RFCR( IDCAL, IDFLT, SETSAT, SATVAL, EXTSAT, IDOUT,
*                     FTYPE, PTYPE, STATUS )

*  Description:
*     The routine writes out the names of the output and flatfield NDFs
*     to the user, and whether the data has been processed including
*     saturated data, and the output NDF data type. All this is echoed
*     to the CCDPACK log file if requested.

*  Arguments:
*     IDFLT = INTEGER (Given)
*        Identifier of the flatfield calibration frame.
*     SETSAT = LOGICAL (Given)
*        True if saturated values were processed.
*     SATVAL = DOUBLE PRECISION (Given)
*        The saturation value.
*     EXTSAT = LOGICAL (Given)
*        Whether the saturation value was obtained from the NDF
*        extension or not.
*     IDOUT = INTEGER (Given)
*        Identifier if the output NDF.
*     FTYPE = CHARACTER * ( * ) (Given)
*        Flatfield daat type.
*     DTYPE = CHARACTER * ( * ) (Given)
*        The output NDF type.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 1991-1992, 1994 Science & Engineering Research
*     Council. All Rights Reserved.

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
*     25-JUN-1991 (PDRAPER):
*        Original Version.
*     13-JAN-1992 (PDRAPER):
*        Added new typing commands
*     19-JAN-1994 (PDRAPER):
*        Added EXTSAT argument.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Arguments Given:
      INTEGER IDFLT
      LOGICAL SETSAT
      DOUBLE PRECISION SATVAL
      LOGICAL EXTSAT
      INTEGER IDOUT
      CHARACTER * ( * ) FTYPE
      CHARACTER * ( * ) DTYPE

*  Status:
      INTEGER STATUS             ! Global status

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Write blank line.
      CALL CCD1_MSG( ' ', ' ', STATUS )

*  Write out the flatfield calibration NDF name.
      CALL NDF_MSG( 'RFCR_FLT', IDFLT )
      CALL CCD1_MSG( ' ', '  Flatfield calibration NDF: ^RFCR_FLT',
     :               STATUS )

*  Add the type of the flatfield.
      CALL MSG_SETC( 'RFCR_TYPE', FTYPE )
      CALL CCD1_MSG( ' ', '  Flatfield data type: ^RFCR_TYPE', STATUS )


*  Comment on the saturation processing
      IF ( SETSAT ) THEN
          CALL CCD1_MSG(  ' ','  Saturated values were processed',
     :                  STATUS )
      ELSE
          CALL CCD1_MSG(  ' ','  Saturated values were not processed',
     :                  STATUS )
      END IF

*  And the saturation value.
      IF ( SETSAT ) THEN
         CALL MSG_SETR( 'RFCR_SATVAL', REAL( SATVAL ) )
         IF ( EXTSAT ) THEN
            CALL CCD1_MSG( ' ', '  Saturation value: ^RFCR_SATVAL*',
     :                     STATUS )
         ELSE
            CALL CCD1_MSG( ' ', '  Saturation value: ^RFCR_SATVAL',
     :                     STATUS )
         END IF
      END IF

*  Name the output NDF and record its output type.
      CALL NDF_MSG( 'RFCR_OUT', IDOUT )
      CALL CCD1_MSG( ' ', '  Output NDF: ^RFCR_OUT', STATUS )

*  Output type.
      CALL MSG_SETC( 'RFCR_TYPE', DTYPE )
      CALL CCD1_MSG( ' ', '  Output type        : ^RFCR_TYPE', STATUS )
      END
* $Id$
