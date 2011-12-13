      SUBROUTINE NDF_CMPLX( INDF, COMP, CMPLX, STATUS )
*+
*  Name:
*     NDF_CMPLX

*  Purpose:
*     Determine whether an NDF array component holds complex values.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL NDF_CMPLX( INDF, COMP, CMPLX, STATUS )

*  Description:
*     The routine returns a logical value indicating whether the
*     specified array component of an NDF holds complex values.

*  Arguments:
*     INDF = INTEGER (Given)
*        NDF identifier.
*     COMP = CHARACTER * ( * ) (Given)
*        Name of the NDF array component: 'DATA', 'QUALITY' or
*        'VARIANCE'.
*     CMPLX = LOGICAL (Returned)
*        Whether the component holds complex values.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     -  A comma-separated list of components may also be specified, in
*     which case the logical "OR" of the results for each component
*     will be returned.
*     -  The value returned for the QUALITY component is always
*     .FALSE..

*  Algorithm:
*     -  Import the NDF identifier.
*     -  Initialise the result and the component count.
*     -  Initialise the character pointer to the start of the component
*     list. Then loop to extract each component from the list.
*     -  Find the final character of the next element in the list.
*     -  Locate the first and last non-blank characters in the element,
*     checking that it is not entirely blank.
*     -  Increment the component count.
*     -  Compare the component name with each value in turn, taking the
*     appropriate action or reporting an error message if an
*     inappropriate component name has been given.
*     -  If the component name was not recognised, then report an
*     error.
*     -  Increment the character pointer to the start of the next
*     element in the component list and return to process it.
*     -  If no error has occurred, but the number of components
*     processed is zero, then report an error.
*     -  If an error has occurred, then report context information.

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
*     29-SEP-1989 (RFWS):
*        Original, derived from the equivalent ARY_ system routine.
*     20-OCT-1989 (RFWS):
*        Removed reference to unnecessary DCB data array components.
*     6-DEC-1989 (RFWS):
*        Changed to allow a list of component names to be supplied and
*        to give more specific error messages if inappropriate
*        component names are specified. Also installed support for the
*        VARIANCE component.
*     7-DEC-1989 (RFWS):
*        Added a check that the number of components processed is not
*        zero.
*     1-MAR-1990 (RFWS):
*        Fixed illegal character string concatenation.
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
      LOGICAL CMPLX

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      LOGICAL NDF1_SIMLR         ! String compare with abbreviation

*  Local variables:
      INTEGER F                  ! Position of first non-blank character
      INTEGER I1                 ! Position of first component character
      INTEGER I2                 ! Position of last component character
      INTEGER IACB               ! Index to ACB entry
      INTEGER L                  ! Position of last non-blank character
      INTEGER NCOMP              ! Number non-blank components specified

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Import the NDF identifier.
      CALL NDF1_IMPID( INDF, IACB, STATUS )
      IF ( STATUS .EQ. SAI__OK ) THEN

*  Initialise the result and the component count.
         CMPLX = .FALSE.
         NCOMP = 0

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
*  Report an error, since this component cannot have complex values.
                  IF ( NDF1_SIMLR( COMP( F : L ), 'AXIS',
     :                             NDF__MINAB ) ) THEN
                     STATUS = NDF__CNMIN
                     CALL ERR_REP( 'NDF_CMPLX_AXI',
     :               'An AXIS component cannot have complex values ' //
     :               '(possible programming error).', STATUS )

*  DATA component.
*  ==============
*  If the DATA component was specified, then determine whether the
*  NDF's data array is complex from its ARY_ system identifier in the
*  ACB.
                  ELSE IF ( NDF1_SIMLR( COMP( F : L ), 'DATA',
     :                                  NDF__MINAB ) ) THEN
                     IF ( .NOT. CMPLX ) THEN
                        CALL ARY_CMPLX( ACB_DID( IACB ), CMPLX, STATUS )
                     END IF

*  EXTENSION.
*  =========
*  Report an error, since extensions cannot have complex values.
                  ELSE IF ( NDF1_SIMLR( COMP( F : L ), 'EXTENSION',
     :                                  NDF__MINAB ) ) THEN
                     STATUS = NDF__CNMIN
                     CALL ERR_REP( 'NDF_CMPLX_EXT',
     :               'An EXTENSION cannot have complex values ' //
     :               '(possible programming error).', STATUS )

*  HISTORY component.
*  =================
*  Report an error, since this component cannot have complex values.
                  ELSE IF ( NDF1_SIMLR( COMP( F : L ), 'HISTORY',
     :                                  NDF__MINAB ) ) THEN
                     STATUS = NDF__CNMIN
                     CALL ERR_REP( 'NDF_CMPLX_HIS',
     :               'A HISTORY component cannot have complex ' //
     :               'values (possible programming error).', STATUS )

*  LABEL component.
*  ===============
*  Report an error, since this component cannot have complex values.
                  ELSE IF ( NDF1_SIMLR( COMP( F : L ), 'LABEL',
     :                                  NDF__MINAB ) ) THEN
                     STATUS = NDF__CNMIN
                     CALL ERR_REP( 'NDF_CMPLX_LAB',
     :               'A LABEL component cannot have complex ' //
     :               'values (possible programming error).', STATUS )

*  QUALITY component.
*  =================
*  If the QUALITY component was specified, then set a .FALSE. result,
*  since the quality array is always non-complex. In practice, this
*  amounts to doing nothing.
                  ELSE IF ( NDF1_SIMLR( COMP( F : L ), 'QUALITY',
     :                      NDF__MINAB ) ) THEN
                     CONTINUE

*  TITLE component.
*  ===============
*  Report an error, since this component cannot have complex values.
                  ELSE IF ( NDF1_SIMLR( COMP( F : L ), 'TITLE',
     :                                  NDF__MINAB ) ) THEN
                     STATUS = NDF__CNMIN
                     CALL ERR_REP( 'NDF_CMPLX_TIT',
     :               'A TITLE component cannot have complex ' //
     :               'values (possible programming error).', STATUS )

*  UNITS component.
*  ===============
*  Report an error, since this component cannot have complex values.
                  ELSE IF ( NDF1_SIMLR( COMP( F : L ), 'UNITS',
     :                                  NDF__MINAB ) ) THEN
                     STATUS = NDF__CNMIN
                     CALL ERR_REP( 'NDF_CMPLX_UNI',
     :               'A UNITS component cannot have complex ' //
     :               'values (possible programming error).', STATUS )

*  VARIANCE component.
*  ==================
*  Inspect the variance component to see if it contains complex values.
                  ELSE IF ( NDF1_SIMLR( COMP( F : L ), 'VARIANCE',
     :                                  NDF__MINAB ) ) THEN
                     IF ( .NOT. CMPLX ) THEN
                        CALL NDF1_VCPX( IACB, CMPLX, STATUS )
                     END IF

*  If the NDF component name was not recognised, then report an error.
                  ELSE
                     STATUS = NDF__CNMIN
                     CALL MSG_SETC( 'BADCOMP', COMP( F : L ) )
                     CALL ERR_REP( 'NDF_CMPLX_CMP',
     :                             'Invalid array component name ' //
     :                             '''^BADCOMP'' specified ' //
     :                             '(possible programming error).',
     :                             STATUS )
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
            CALL ERR_REP( 'NDF_CMPLX_NONE',
     :                    'No array component name specified ' //
     :                    '(possible programming error).', STATUS )
         END IF
      END IF

*  If an error occurred, then report context information and call the
*  error tracing routine.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'NDF_CMPLX_ERR',
     :   'NDF_CMPLX: Error determining whether an NDF array ' //
     :   'component holds complex values.', STATUS )
         CALL NDF1_TRACE( 'NDF_CMPLX', STATUS )
      END IF

      END
