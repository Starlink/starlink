      SUBROUTINE NDF_FORM( INDF, COMP, FORM, STATUS )
*+
*  Name:
*     NDF_FORM

*  Purpose:
*     Obtain the storage form of an NDF array component.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL NDF_FORM( INDF, COMP, FORM, STATUS )

*  Description:
*     The routine returns the storage form of an NDF array component as
*     an upper case character string (e.g. 'SIMPLE').

*  Arguments:
*     INDF = INTEGER (Given)
*        NDF identifier.
*     COMP = CHARACTER * ( * ) (Given)
*        Name of the NDF array component whose storage form is required:
*        'DATA', 'QUALITY' or 'VARIANCE'.
*     FORM = CHARACTER * ( * ) (Returned)
*        Storage form of the component.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     -  The symbolic constant NDF__SZFRM may be used for declaring the
*     length of a character variable to hold the storage form of an NDF
*     array component. This constant is defined in the include file
*     NDF_PAR.
*     -  At present, the NDF_ routines only support "primitive", "simple",
*     "delta" and "scaled" arrays, so only the values 'PRIMITIVE', 'SIMPLE',
*     'DELTA' and 'SCALED' can be returned.

*  Algorithm:
*     -  Import the NDF identifier.
*     -  Compare the NDF component name with each value in turn,
*     allowing abbreviations.
*     -  Take the appropriate action, or report an error if an
*     inappropriate component name was specified.
*     -  If the component name was not recognised, then report an error.
*     -  If an error occurred, then report context information.

*  Copyright:
*     Copyright (C) 1989, 1990 Science & Engineering Research Council.
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
*     20-OCT-1989 (RFWS):
*        Original version.
*     6-DEC-1989 (RFWS):
*        Added support for the variance component.
*     7-DEC-1989 (RFWS):
*        Changed to give more specific error messages if an
*        inappropriate component name is specified.
*     30-JAN-1990 (RFWS):
*        Installed support for the quality component.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! DAT_ public constants
      INCLUDE 'NDF_PAR'          ! NDF_ public constants
      INCLUDE 'NDF_CONST'        ! NDF_ private constants
      INCLUDE 'NDF_ERR'          ! NDF_ error codes

*  Global Variables:
      INCLUDE 'NDF_ACB'          ! NDF_ Access Control Block
*        ACB_DID( NDF__MXACB ) = INTEGER (Read)
*           ARY_ system identifier for the NDF's data array.

*  Arguments Given:
      INTEGER INDF
      CHARACTER * ( * ) COMP

*  Arguments Returned:
      CHARACTER * ( * ) FORM

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      LOGICAL NDF1_SIMLR         ! String compare with abbreviation

*  Local variables:
      INTEGER IACB               ! Index to NDF entry in the ACB

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Import the NDF identifier.
      CALL NDF1_IMPID( INDF, IACB, STATUS )
      IF ( STATUS .EQ. SAI__OK ) THEN

*  Compare the component name with each permitted value in turn,
*  allowing abbreviation.

*  AXIS component.
*  ==============
*  Report an error, since this component has no storage form.
         IF ( NDF1_SIMLR( COMP, 'AXIS', NDF__MINAB ) ) THEN
            STATUS = NDF__CNMIN
            CALL ERR_REP( 'NDF_FORM_AXI',
     :      'An AXIS component does not have a storage form ' //
     :      '(possible programming error).', STATUS )

*  DATA component.
*  ==============
*  Obtain the storage form from the ARY_ system data array identifier
*  in the ACB.
         ELSE IF ( NDF1_SIMLR( COMP, 'DATA', NDF__MINAB ) ) THEN
            CALL ARY_FORM( ACB_DID( IACB ), FORM, STATUS )

*  EXTENSION.
*  =========
*  Report an error, since extensions do not have a storage form.
         ELSE IF ( NDF1_SIMLR( COMP, 'EXTENSION', NDF__MINAB ) ) THEN
            STATUS = NDF__CNMIN
            CALL ERR_REP( 'NDF_FORM_EXT',
     :      'An EXTENSION does not have a storage form (possible ' //
     :      'programming error).', STATUS )

*  HISTORY component.
*  =================
*  Report an error, since this component does not have a storage form.
         ELSE IF ( NDF1_SIMLR( COMP, 'HISTORY', NDF__MINAB ) ) THEN
            STATUS = NDF__CNMIN
            CALL ERR_REP( 'NDF_FORM_HIS',
     :      'A HISTORY component does not have a storage form ' //
     :      '(possible programming error).', STATUS )

*  LABEL component.
*  ===============
*  Report an error, since this component does not have a storage form.
         ELSE IF ( NDF1_SIMLR( COMP, 'LABEL', NDF__MINAB ) ) THEN
            STATUS = NDF__CNMIN
            CALL ERR_REP( 'NDF_FORM_LAB',
     :      'A LABEL component does not have a storage form ' //
     :      '(possible programming error).', STATUS )

*  QUALITY component.
*  =================
*  Inspect the quality component to determine its storage form.
         ELSE IF ( NDF1_SIMLR( COMP, 'QUALITY', NDF__MINAB ) ) THEN
            CALL NDF1_QFRM( IACB, FORM, STATUS )

*  TITLE component.
*  ===============
*  Report an error, since this component does not have a storage form.
         ELSE IF ( NDF1_SIMLR( COMP, 'TITLE', NDF__MINAB ) ) THEN
            STATUS = NDF__CNMIN
            CALL ERR_REP( 'NDF_FORM_TIT',
     :      'A TITLE component does not have a storage form ' //
     :      '(possible programming error).', STATUS )

*  UNITS component.
*  ===============
*  Report an error, since this component does not have a storage form.
         ELSE IF ( NDF1_SIMLR( COMP, 'UNITS', NDF__MINAB ) ) THEN
            STATUS = NDF__CNMIN
            CALL ERR_REP( 'NDF_FORM_UNI',
     :      'A UNITS component does not have a storage form ' //
     :      '(possible programming error).', STATUS )

*  VARIANCE component.
*  ==================
*  Inspect the variance component to determine its storage form.
         ELSE IF ( NDF1_SIMLR( COMP, 'VARIANCE', NDF__MINAB ) ) THEN
            CALL NDF1_VFRM( IACB, FORM, STATUS )

*  If the NDF component name was not recognised, then report an error.
         ELSE
            STATUS = NDF__CNMIN
            CALL MSG_SETC( 'BADCOMP', COMP )
            CALL ERR_REP( 'NDF_FORM_COMP',
     :                    'Invalid array component name ' //
     :                    '''^BADCOMP'' specified (possible ' //
     :                    'programming error).', STATUS )
         END IF
      END IF

*  If an error occurred, then report context information and call the
*  error tracing routine.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'NDF_FORM_ERR',
     :   'NDF_FORM: Error obtaining the storage form of an NDF ' //
     :   'array component.', STATUS )
         CALL NDF1_TRACE( 'NDF_FORM', STATUS )
      END IF

      END
