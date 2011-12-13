      SUBROUTINE CCD1_RCCR( FTYPE, IDCAL, EXPOSE, EXTEXP, SETSAT,
     :                      SATVAL, EXTSAT, IDOUT, PRESER, CTYPE,
     :                      DTYPE, STATUS )
*+
*  Name:
*     CCD1_RCCR

*  Purpose:
*     To report the parameters used in a run of CALCOR.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL CCD1_RCCR( FTYPE, IDCAL, EXPOSE, EXTEXP, SETSAT,
*                     SATVAL, EXTSAT, IDOUT, PRESER, CTYPE,
*                     DTYPE, STATUS )

*  Description:
*     The routine writes out the names of the calibration and output
*     NDFs to the user, the exposure factor, whether the data has been
*     processed including saturated data, and the NDF data types.
*     All this is echoed to the CCDPACK log file if requested.

*  Arguments:
*     FTYPE = CHARACTER * ( * ) (Given)
*        The type of correction applied. Should be one of "DARK",
*        "FLASH" or "NONE".
*     IDCAL = INTEGER (Given)
*        Identifier of the calibration frame.
*     EXPOSE = DOUBLE PRECISION (Given)
*        The data exposure factor with respect to the calibration
*        normalisation.
*     EXTEXP = LOGICAL (Given)
*        Whether the exposure factors were obtained from the NDF
*        extensions or not.
*     SETSAT = LOGICAL (Given)
*        True if saturated values were processed.
*     SATVAL = DOUBLE PRECISION (Given)
*        The saturation value.
*     EXTSAT = LOGICAL (Given)
*        Whether saturation value was obtained from the NDF extension or
*        not.
*     IDOUT = INTEGER (Given)
*        Identifier if the output NDF.
*     PRESER = LOGICAL (Given)
*        True if the input data type was preserved on output.
*     CTYPE = CHARACTER * ( * ) (Given)
*        The calibration NDF type.
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
*     12-JUN-1991 (PDRAPER):
*        Original version.
*     24-JUN-1991 (PDRAPER):
*        Changed to use use log file system.
*     10-JAN-1992 (PDRAPER):
*        Changed for new fully generic compatibility.
*     13-JAN-1994 (PDRAPER):
*        Added USEEXT argument.
*     19-JAN-1994 (PDRAPER):
*        Removed USEEXT, added EXTSAT, EXTEXP and FTYPE.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Arguments Given:
      CHARACTER * ( * ) FTYPE
      INTEGER IDCAL
      DOUBLE PRECISION EXPOSE
      LOGICAL EXTEXP
      LOGICAL SETSAT
      DOUBLE PRECISION SATVAL
      LOGICAL EXTSAT
      INTEGER IDOUT
      LOGICAL PRESER
      CHARACTER * ( * ) CTYPE
      CHARACTER * ( * ) DTYPE

*  Status:
      INTEGER STATUS             ! Global status

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Write blank line.
      CALL CCD1_MSG( ' ', ' ' , STATUS )

*  Write out the calibration NDF name.
      CALL NDF_MSG( 'RCAL_CAL', IDCAL )
      CALL CCD1_MSG( ' ', '  Calibration NDF: ^RCAL_CAL', STATUS )

*  The data type of the calibration frame.
      CALL MSG_SETC( 'RCAL_CTYPE', CTYPE )
      CALL CCD1_MSG( ' ',
     :'  Calibration data type      : ^RCAL_CTYPE', STATUS )

*  Calibration exposure time.
      CALL MSG_SETR( 'RCCR_EXPOSE', REAL( EXPOSE ) )
      IF ( EXTEXP ) THEN
         CALL CCD1_MSG( ' ',
     :'  Calibration exposure factor: ^RCCR_EXPOSE*', STATUS )
      ELSE
         CALL CCD1_MSG( ' ',
     :'  Calibration exposure factor: ^RCCR_EXPOSE', STATUS )
      END IF

*  Comment on the type of correction.
      IF ( FTYPE .EQ. 'DARK' ) THEN
         CALL CCD1_MSG( ' ', '  Corrected data for DARK level', STATUS)
      ELSE IF ( FTYPE .EQ. 'FLASH' ) THEN
         CALL CCD1_MSG( ' ', '  Corrected data for pre-FLASH', STATUS)
      END IF

*  Comment on the saturation processing
      IF ( SETSAT ) THEN
          CALL CCD1_MSG( ' ','  Saturated values were processed',
     :                   STATUS )
      ELSE
          CALL CCD1_MSG( ' ','  Saturated values were not processed',
     :                   STATUS )
      END IF

*  And the saturation value.
      IF ( SETSAT ) THEN
         CALL MSG_SETR( 'RCAL_SATVAL', REAL( SATVAL ) )
         IF ( EXTSAT ) THEN
            CALL CCD1_MSG( ' ',
     :'  Saturation value           : ^RCAL_SATVAL*', STATUS )
         ELSE
            CALL CCD1_MSG( ' ',
     :'  Saturation value           : ^RCAL_SATVAL', STATUS )
         END IF
      END IF

*  Name the output NDF and record its output type.
      CALL NDF_MSG( 'RCAL_OUT', IDOUT )
      CALL CCD1_MSG( ' ', '  Output NDF: ^RCAL_OUT', STATUS )

*  Output type.
      CALL MSG_SETC( 'RCAL_DTYPE', DTYPE )
      CALL CCD1_MSG( ' ',
     :'  Output data type           : ^RCAL_DTYPE', STATUS )

*  Say if this is a preserved type.
      IF ( PRESER ) THEN
         CALL CCD1_MSG( ' ',
     :'  Input data type was preserved', STATUS )
      ELSE
         CALL CCD1_MSG( ' ',
     :'  Input data type converted into floating point', STATUS )
      END IF

      END
* $Id$
