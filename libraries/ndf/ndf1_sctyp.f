      SUBROUTINE NDF1_SCTYP( IACB, COMP, ITYPE, STATUS )
*+
*  Name:
*     NDF1_SCTYP

*  Purpose:
*     Obtain the numeric data type of a scaled NDF array component
*     identified by its ACB entry.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL NDF1_SCTYP( IACB, COMP, ITYPE, STATUS )

*  Description:
*     The routine returns the numeric data type of a scaled array
*     component of an NDF as an upper case character string.  The NDF
*     is identified by its entry in the ACB. The returned type describes
*     the values stored in the array, before they are unscaled using
*     the associated scale and zero values. Use NDF1_TYP if you
*     need the data type of the array after it has been unscaled.

*  Arguments:
*     IACB = INTEGER (Given)
*        Index to the NDF's entry in the ACB.
*     COMP = CHARACTER * ( * ) (Given)
*        Name of the NDF array component whose type is required;
*        'DATA' or 'VARIANCE'.
*     ITYPE = INTEGER (Returned)
*        Numeric data type code of the component; a symbolic constant
*        with a name of the form NDF__SCTYPx, where x identifies the data
*        type (these constants are defined in the include file
*        NDF_CONST).
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     -  A comma-separated list of NDF component names may also be
*     supplied to this routine. In this case the result returned will
*     be the integer code for the data type with the lowest precision
*     such that the values held in any of the specified components can
*     be converted to that type without loss of numerical precision.

*  Copyright:
*     Copyright (C) 2006 Particle Physics and Astronomy Research
*     Council. All Rights Reserved.

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
*     DSB: David S Berry (JAC)
*     {enter_new_authors_here}

*  History:
*     12-JUL-2006 (DSB):
*        Original version.
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
      INTEGER IACB
      CHARACTER * ( * ) COMP

*  Arguments Returned:
      INTEGER ITYPE

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      LOGICAL NDF1_SIMLR         ! String compare with abbreviation

*  Local variables:
      CHARACTER * ( NDF__SZTYP ) TYPEC ! Component data type string
      INTEGER F                  ! Position of first non-blank character
      INTEGER I1                 ! Position of first component character
      INTEGER I2                 ! Position of last component character
      INTEGER ITYPEC             ! Integer type code of component
      INTEGER L                  ! Position of last non-blank character
      INTEGER MXTYPE             ! "Maximised" numeric type code
      INTEGER NCOMP              ! Number non-blank components specified

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Initialise the component count.
      NCOMP = 0

*  Initialise the character pointer to the start of the component list.
*  Then loop to extract each element from the component list.
      I1 = 1
1     CONTINUE                ! Start of "DO WHILE" loop
      IF ( ( STATUS .EQ. SAI__OK ) .AND.
     :     ( I1 .LE. LEN( COMP ) ) ) THEN

*  Find the final character of the next element in the component list
*  (the last character before a comma or end of string).
         I2 = INDEX( COMP( I1 : ), ',' )
         IF ( I2 .EQ. 0 ) THEN
            I2 = LEN( COMP )
         ELSE
            I2 = I2 + I1 - 2
         END IF
         IF ( I2 .GE. I1 ) THEN

*  Locate the first and last non-blank characters in the element,
*  checking that it is not entirely blank.
            CALL CHR_FANDL( COMP( I1 : I2 ), F, L )
            IF ( L .GE. F ) THEN
               F = F + I1 - 1
               L = L + I1 - 1

*  Increment the component count.
               NCOMP = NCOMP + 1

*  Compare the component name with each value in turn (allowing
*  abbreviation), and take the appropriate action, or report an error
*  if an inappropriate component name has been given.

*  AXIS component.
*  ==============
*  Report an error, since this component does not have a data type.
               IF ( NDF1_SIMLR( COMP( F : L ), 'AXIS',
     :                          NDF__MINAB ) ) THEN
                  STATUS = NDF__CNMIN
                  CALL ERR_REP( 'NDF1_SCTYP_AXI',
     :            'A numeric type cannot be obtained for an AXIS ' //
     :            'component (possible programming error).',
     :            STATUS )

*  DATA component.
*  ==============
*  Obtain its data type from the ARY_ system data array identifier in
*  the ACB.
               ELSE IF ( NDF1_SIMLR( COMP( F : L ), 'DATA',
     :                               NDF__MINAB ) ) THEN
                  CALL ARY_SCTYP( ACB_DID( IACB ), TYPEC, STATUS )

*  EXTENSION.
*  =========
*  Report an error, since extensions do not have numeric data types.
               ELSE IF ( NDF1_SIMLR( COMP( F : L ), 'EXTENSION',
     :                               NDF__MINAB ) ) THEN
                  STATUS = NDF__CNMIN
                  CALL ERR_REP( 'NDF1_SCTYP_EXT',
     :            'A numeric type cannot be obtained for an ' //
     :            'EXTENSION (possible programming error).', STATUS )

*  HISTORY component.
*  =================
*  Report an error, since this component does not have a data type.
               ELSE IF ( NDF1_SIMLR( COMP( F : L ), 'HISTORY',
     :                               NDF__MINAB ) ) THEN
                  STATUS = NDF__CNMIN
                  CALL ERR_REP( 'NDF1_SCTYP_HIS',
     :            'A numeric type cannot be obtained for a HISTORY ' //
     :            'component (possible programming error).',
     :            STATUS )

*  LABEL component.
*  ===============
*  Report an error, since this component does not have a data type.
               ELSE IF ( NDF1_SIMLR( COMP( F : L ), 'LABEL',
     :                               NDF__MINAB ) ) THEN
                  STATUS = NDF__CNMIN
                  CALL ERR_REP( 'NDF1_SCTYP_LAB',
     :            'A numeric type cannot be obtained for a LABEL ' //
     :            'component (possible programming error).',
     :            STATUS )

*  QUALITY component.
*  =================
*  Set a type value of '_UBYTE', which is the only data type permitted
*  for this component.
               ELSE IF ( NDF1_SIMLR( COMP( F : L ), 'QUALITY',
     :                               NDF__MINAB ) ) THEN
                  TYPEC = '_UBYTE'

*  TITLE component.
*  ===============
*  Report an error, since this component does not have a data type.
               ELSE IF ( NDF1_SIMLR( COMP, 'TITLE',
     :                               NDF__MINAB ) ) THEN
                  STATUS = NDF__CNMIN
                  CALL ERR_REP( 'NDF1_SCTYP_TIT',
     :            'A numeric type cannot be obtained for a TITLE ' //
     :            'component (possible programming error).',
     :            STATUS )

*  UNITS component.
*  ===============
*  Report an error, since this component does not have a data type.
               ELSE IF ( NDF1_SIMLR( COMP( F : L ), 'UNITS',
     :                               NDF__MINAB ) ) THEN
                  STATUS = NDF__CNMIN
                  CALL ERR_REP( 'NDF1_SCTYP_UNI',
     :            'A numeric type cannot be obtained for a UNITS ' //
     :            'component (possible programming error).',
     :            STATUS )

*  VARIANCE component.
*  ==================
*  Inspect the variance component to obtain its numeric data type.
               ELSE IF ( NDF1_SIMLR( COMP( F : L ), 'VARIANCE',
     :                               NDF__MINAB ) ) THEN
                  CALL NDF1_VSTYP( IACB, TYPEC, STATUS )

*  If the NDF component name was not recognised, then report an error.
               ELSE
                  STATUS = NDF__CNMIN
                  CALL MSG_SETC( 'BADCOMP', COMP( F : L ) )
                  CALL ERR_REP( 'NDF1_SCTYP_COMP',
     :                          'Invalid array component name ' //
     :                          '''^BADCOMP'' specified (possible ' //
     :                          'programming error).', STATUS )
               END IF

*  Convert the component data type string into the corresponding
*  integer type code.
               CALL NDF1_PSTYP( TYPEC, ITYPEC, STATUS )

*  ITYPE accumulates the type code for the data type of the result. For
*  the first component, simply set its value.
               IF ( STATUS .EQ. SAI__OK ) THEN
                  IF ( NCOMP .EQ. 1 ) THEN
                     ITYPE = ITYPEC

*  For subsequent components, "maximise" the type code obtained for the
*  latest component with that obtained for previous components.
                  ELSE
                     CALL NDF1_MXTYP( ITYPE, ITYPEC, MXTYPE, STATUS )
                     ITYPE = MXTYPE
                  END IF
               END IF
            END IF
         END IF

*  Increment the character pointer to the start of the next element in
*  the component list and return to process the next element.
         I1 = I2 + 2
         GO TO 1
      END IF

*  If no error has occurred, but no non-blank component names have been
*  processed, then report an error.
      IF ( ( STATUS .EQ. SAI__OK ) .AND. ( NCOMP .EQ. 0 ) ) THEN
         STATUS = NDF__NOCMP
         CALL ERR_REP( 'NDF1_SCTYP_NONE',
     :                 'No array component name specified (possible ' //
     :                 'programming error).', STATUS )
      END IF

*  Call error tracing routine and exit.
      IF ( STATUS .NE. SAI__OK ) CALL NDF1_TRACE( 'NDF1_SCTYP', STATUS )

      END
