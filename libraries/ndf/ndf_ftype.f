      SUBROUTINE NDF_FTYPE( INDF, COMP, FTYPE, STATUS )
*+
*  Name:
*     NDF_FTYPE

*  Purpose:
*     Obtain the full type of an NDF array component.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL NDF_FTYPE( INDF, COMP, FTYPE, STATUS )

*  Description:
*     The routine returns the full type of one of the array components
*     of an NDF as an upper-case character string (e.g. '_REAL' or
*     'COMPLEX_BYTE').

*  Arguments:
*     INDF = INTEGER (Given)
*        NDF identifier.
*     COMP = CHARACTER * ( * ) (Given)
*        Name of the NDF array component whose type is required: 'DATA',
*        'QUALITY' or 'VARIANCE'.
*     FTYPE = CHARACTER * ( * ) (Returned)
*        Full type of the component.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     -  A comma-separated list of component names may also be supplied
*     to this routine. In this case the result returned will be the
*     lowest precision full type to which the values held in all the
*     specified components can be converted without unnecessary loss of
*     information.
*     -  The numeric type of a scaled array is determined by the numeric
*     type of the scale and zero terms, not by the numeric type of the
*     underlying array elements.
*     -  The value returned for the QUALITY component is always
*     '_UBYTE'.
*     -  The symbolic constant NDF__SZFTP may be used for declaring the
*     length of a character variable to hold the full type of an NDF
*     array component. This constant is defined in the include file
*     NDF_PAR.

*  Copyright:
*     Copyright (C) 1994 Particle Physics & Astronomy Research Council

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
*     14-NOV-1989 (RFWS):
*        Minor change to prologue.
*     19-MAR-1990 (RFWS):
*        Added support for comma-separated component name lists.
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
      CHARACTER * ( * ) FTYPE

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      LOGICAL NDF1_SIMLR         ! String compare with abbreviation

*  Local variables:
      CHARACTER * ( NDF__SZTYP ) DATYP( NDF__TYPUB : NDF__MXTYP ) ! Data
      CHARACTER * ( NDF__SZTYP ) TYPEC ! Numeric data type of component
      INTEGER F                  ! Position of first non-blank character
      INTEGER I1                 ! Position of first component character
      INTEGER I2                 ! Position of last component character
      INTEGER IACB               ! Index to NDF entry in the ACB
      INTEGER ITYPE              ! Integer type code of result
      INTEGER ITYPEC             ! Integer type code of component
      INTEGER L                  ! Position of last non-blank character
      INTEGER MXTYPE             ! "Maximised" numeric type code
      INTEGER NCOMP              ! Number non-blank components specified
      LOGICAL CMPLX              ! Complex value flag

*  Local Data:
      DATA DATYP( NDF__TYPB ) / '_BYTE' / ! Data type code translations
      DATA DATYP( NDF__TYPD ) / '_DOUBLE' /
      DATA DATYP( NDF__TYPI ) / '_INTEGER' /
      DATA DATYP( NDF__TYPK ) / '_INT64' /
      DATA DATYP( NDF__TYPR ) / '_REAL' /
      DATA DATYP( NDF__TYPUB ) / '_UBYTE' /
      DATA DATYP( NDF__TYPUW ) / '_UWORD' /
      DATA DATYP( NDF__TYPW ) / '_WORD' /

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Import the NDF identifier.
      CALL NDF1_IMPID( INDF, IACB, STATUS )
      IF ( STATUS .EQ. SAI__OK ) THEN

*  Initialise the component count and complex value flag.
         NCOMP = 0
         CMPLX = .FALSE.

*  Initialise the character pointer to the start of the component list.
*  Then loop to extract each element from the component list.
         I1 = 1
1        CONTINUE                ! Start of "DO WHILE" loop
         IF ( ( STATUS .EQ. SAI__OK ) .AND.
     :        ( I1 .LE. LEN( COMP ) ) ) THEN

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
*  Report an error, since this component does not have a type.
                  IF ( NDF1_SIMLR( COMP( F : L ), 'AXIS',
     :                             NDF__MINAB ) ) THEN
                     STATUS = NDF__CNMIN
                     CALL ERR_REP( 'NDF_FTYPE_AXI',
     :                    'A full type cannot be obtained for an ' //
     :                    'AXIS component (possible programming ' //
     :                    'error).', STATUS )

*  DATA component.
*  ==============
*  Obtain its full type and complex value flag from the ARY_ system data
*  array identifier in the ACB.
                  ELSE IF ( NDF1_SIMLR( COMP( F : L ), 'DATA',
     :                                  NDF__MINAB ) ) THEN
                     CALL ARY_TYPE( ACB_DID( IACB ), TYPEC, STATUS )
                     IF ( .NOT. CMPLX ) THEN
                        CALL ARY_CMPLX( ACB_DID( IACB ), CMPLX, STATUS )
                     END IF

*  EXTENSION.
*  =========
*  Report an error, since extensions do not have a type.
                  ELSE IF ( NDF1_SIMLR( COMP( F : L ), 'EXTENSION',
     :                                  NDF__MINAB ) ) THEN
                     STATUS = NDF__CNMIN
                     CALL ERR_REP( 'NDF_FTYPE_EXT',
     :                    'A full type cannot be obtained for an ' //
     :                    'EXTENSION (possible programming error).',
     :                    STATUS )

*  HISTORY component.
*  =================
*  Report an error, since this component does not have a type.
                  ELSE IF ( NDF1_SIMLR( COMP( F : L ), 'HISTORY',
     :                                  NDF__MINAB ) ) THEN
                     STATUS = NDF__CNMIN
                     CALL ERR_REP( 'NDF_FTYPE_HIS',
     :                    'A full type cannot be obtained for a ' //
     :                    'HISTORY component (possible programming ' //
     :                    'error).', STATUS )

*  LABEL component.
*  ===============
*  Report an error, since this component does not have a type.
                  ELSE IF ( NDF1_SIMLR( COMP( F : L ), 'LABEL',
     :                                  NDF__MINAB ) ) THEN
                     STATUS = NDF__CNMIN
                     CALL ERR_REP( 'NDF_FTYPE_LAB',
     :                    'A full type cannot be obtained for a ' //
     :                    'LABEL component (possible programming ' //
     :                    'error).', STATUS )

*  QUALITY component.
*  =================
*  Set a numeric type value of '_UBYTE', which is the only type
*  permitted for this component. The complex value flag is .FALSE., so
*  do nothing to change the current value of CMPLX.
                  ELSE IF ( NDF1_SIMLR( COMP( F : L ), 'QUALITY',
     :                                  NDF__MINAB ) ) THEN
                     TYPEC = '_UBYTE'

*  TITLE component.
*  ===============
*  Report an error, since this component does not have a type.
                  ELSE IF ( NDF1_SIMLR( COMP, 'TITLE',
     :                                  NDF__MINAB ) ) THEN
                     STATUS = NDF__CNMIN
                     CALL ERR_REP( 'NDF_FTYPE_TIT',
     :                    'A full type cannot be obtained for a ' //
     :                    'TITLE component (possible programming ' //
     :                    'error).', STATUS )

*  UNITS component.
*  ===============
*  Report an error, since this component does not have a type.
                  ELSE IF ( NDF1_SIMLR( COMP( F : L ), 'UNITS',
     :                                  NDF__MINAB ) ) THEN
                     STATUS = NDF__CNMIN
                     CALL ERR_REP( 'NDF_FTYPE_UNI',
     :                    'A full type cannot be obtained for a ' //
     :                    'UNITS component (possible programming ' //
     :                    'error).', STATUS )

*  VARIANCE component.
*  ==================
*  Inspect the variance component to obtain its numeric type and complex
*  value flag.
                  ELSE IF ( NDF1_SIMLR( COMP( F : L ), 'VARIANCE',
     :                                  NDF__MINAB ) ) THEN
                     CALL NDF1_VTYP( IACB, TYPEC, STATUS )
                     IF ( .NOT. CMPLX ) THEN
                        CALL NDF1_VCPX( IACB, CMPLX, STATUS )
                     END IF

*  If the NDF component name was not recognised, then report an error.
                  ELSE
                     STATUS = NDF__CNMIN
                     CALL MSG_SETC( 'BADCOMP', COMP( F : L ) )
                     CALL ERR_REP( 'NDF_FTYPE_COMP',
     :                             'Invalid array component name ' //
     :                             '''^BADCOMP'' specified ' //
     :                             '(possible programming error).',
     :                             STATUS )
                  END IF

*  Convert the component type string into the corresponding integer type
*  code.
                  CALL NDF1_PSTYP( TYPEC, ITYPEC, STATUS )

*  ITYPE accumulates the type code for the type of the result. For the
*  first component, simply set its value.
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
            CALL ERR_REP( 'NDF_FTYPE_NONE',
     :                    'No array component name specified ' //
     :                    '(possible programming error).', STATUS )
         END IF

*  Convert the final type code and complex value flag into a character
*  string and return it.
         IF ( STATUS .EQ. SAI__OK ) THEN
            IF ( CMPLX ) THEN
               CALL NDF1_CCPY( 'COMPLEX' // DATYP( ITYPE ), FTYPE,
     :                         STATUS )
            ELSE
               CALL NDF1_CCPY( DATYP( ITYPE ), FTYPE, STATUS )
            END IF
         END IF
      END IF

*  If an error occurred, then report context information and call the
*  error tracing routine.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'NDF_FTYPE_ERR',
     :        'NDF_FTYPE: Error obtaining the full type of an NDF ' //
     :        'array component.', STATUS )
         CALL NDF1_TRACE( 'NDF_FTYPE', STATUS )
      END IF

      END
