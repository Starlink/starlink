      SUBROUTINE NDF_AFORM( INDF, COMP, IAXIS, FORM, STATUS )
*+
*  Name:
*     NDF_AFORM

*  Purpose:
*     Obtain the storage form of an NDF axis array.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL NDF_AFORM( INDF, COMP, IAXIS, FORM, STATUS )

*  Description:
*     The routine returns the storage form of a specified NDF axis
*     array component as an upper case character string (e.g.
*     'PRIMITIVE').

*  Arguments:
*     INDF = INTEGER (Given)
*        NDF identifier.
*     COMP = CHARACTER * ( * ) (Given)
*        Name of the axis array component whose storage form is
*        required: 'CENTRE', 'VARIANCE' or 'WIDTH'.
*     IAXIS = INTEGER (Given)
*        Number of the NDF axis for which information is required.
*     FORM = CHARACTER * ( * ) (Returned)
*        Storage form of the axis array.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     -  The symbolic constant NDF__SZFRM may be used for declaring the
*     length of a character variable to hold the storage form of an NDF
*     axis array. This constant is defined in the include file NDF_PAR.
*     -  At present, the NDF_ routines only support "primitive" and
*     "simple" arrays, so only the values 'PRIMITIVE' and 'SIMPLE' can
*     be returned.

*  Algorithm:
*     -  Import the NDF identifier.
*     -  Check the axis number for validity.
*     -  Test the array name against each valid value in turn, calling
*     the appropriate routine to obtain the storage form.
*     -  If the array name was not recognised, then report an error.
*     -  If an error occurred, then report context information.

*  Copyright:
*     Copyright (C) 1990 Science & Engineering Research Council.
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
*     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
*     02110-1301, USA

*  Authors:
*     RFWS: R.F. Warren-Smith (STARLINK)
*     {enter_new_authors_here}

*  History:
*     10-OCT-1990 (RFWS):
*        Original version.
*     15-OCT-1990 (RFWS):
*        Installed support for the axis variance and width arrays.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! DAT_ public constants
      INCLUDE 'NDF_CONST'        ! NDF_ private constants
      INCLUDE 'NDF_ERR'          ! NDF_ error codes

*  Arguments Given:
      INTEGER INDF
      CHARACTER * ( * ) COMP
      INTEGER IAXIS

*  Arguments Returned:
      CHARACTER * ( * ) FORM

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      LOGICAL NDF1_SIMLR         ! String compare with abbreviation

*  Local Variables:
      INTEGER IACB               ! Index to NDF entry in the ACB
      INTEGER IAX1               ! First axis to process
      INTEGER IAX2               ! Last axis to process (junk)

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Import the NDF identifier.
      CALL NDF1_IMPID( INDF, IACB, STATUS )

*  Check the axis number for validity.
      CALL NDF1_VAN( IACB, IAXIS, .FALSE., IAX1, IAX2, STATUS )
      IF ( STATUS .EQ. SAI__OK ) THEN

*  Test the array name against each valid value in turn, calling the
*  appropriate routine to obtain the storage form.

*  CENTRE array:
*  ============
         IF ( NDF1_SIMLR( COMP, 'CENTRE', NDF__MINAB ) .OR.
     :        NDF1_SIMLR( COMP, 'CENTER', NDF__MINAB ) ) THEN
            CALL NDF1_ADFRM( IAX1, IACB, FORM, STATUS )

*  VARIANCE array:
*  ==============
         ELSE IF ( NDF1_SIMLR( COMP, 'VARIANCE', NDF__MINAB ) ) THEN
            CALL NDF1_AVFRM( IAX1, IACB, FORM, STATUS )

*  WIDTH array:
*  ===========
         ELSE IF ( NDF1_SIMLR( COMP, 'WIDTH', NDF__MINAB ) ) THEN
            CALL NDF1_AWFRM( IAX1, IACB, FORM, STATUS )

*  If the array name was not recognised, then report an error.
         ELSE
            STATUS = NDF__CNMIN
            CALL MSG_SETC( 'BADNAME', COMP )
            CALL ERR_REP( 'NDF_AFORM_NAME',
     :                    'Invalid axis array component name ' //
     :                    '''^BADNAME'' specified (possible ' //
     :                    'programming error).', STATUS )
         END IF
      END IF

*  If an error occurred, then report context information and call the
*  error tracing routine.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'NDF_AFORM_ERR',
     :   'NDF_AFORM: Error obtaining the storage form of an NDF ' //
     :   'axis array.', STATUS )
         CALL NDF1_TRACE( 'NDF_AFORM', STATUS )
      END IF

      END
