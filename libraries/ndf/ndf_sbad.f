      SUBROUTINE NDF_SBAD( BAD, INDF, COMP, STATUS )
*+
*  Name:
*     NDF_SBAD

*  Purpose:
*     Set the bad-pixel flag for an NDF array component.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL NDF_SBAD( BAD, INDF, COMP, STATUS )

*  Description:
*     The routine sets the value of the bad-pixel flag for an NDF array
*     component. A call to this routine with BAD set to .TRUE. declares
*     that the specified component may contain bad pixel values for
*     which checks must be made by algorithms which subsequently
*     process its values. A call with BAD set to .FALSE. declares that
*     there are definitely no bad values present and that subsequent
*     checks for such values may be omitted.

*  Arguments:
*     BAD = LOGICAL (Given)
*        Bad-pixel flag value to be set.
*     INDF = INTEGER (Given)
*        NDF identifier.
*     COMP = CHARACTER * ( * ) (Given)
*        Name of the NDF array component; 'DATA' or 'VARIANCE'.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     -  A comma-separated list of component names may also be
*     supplied, in which case the bad-pixel flag will be set to the
*     same value for each component in turn.
*     -  If a component is mapped for access when this routine is
*     called, then the bad-pixel flag will be associated with the
*     mapped values. This information will only be transferred to the
*     actual data object when the component is unmapped (but only if it
*     was mapped for UPDATE or WRITE access). The value transferred may
*     be modified if conversion errors occur during the unmapping
*     process.
*     -  This routine has no effect on components which are in an
*     undefined state; the bad-pixel flag for such components always
*     remains set to .TRUE. (or .FALSE. in the case of the QUALITY
*     component).

*  Algorithm:
*     -  Import the NDF identifier.
*     -  Initialise the component count.
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
*     22-NOV-1989 (RFWS):
*        Original version.
*     12-DEC-1989 (RFWS):
*        Installed support for the variance component.
*     11-JAN-1990 (RFWS):
*        Added support for comma-separated component lists.
*     1-MAR-1990 (RFWS):
*        Fixed illegal character string concatenation.
*     21-MAR-1990 (RFWS):
*        Changed to handle the bad pixel flag for mapped data values.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
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
*        ACB_DMBAD( NDF__MXACB ) = LOGICAL (Write)
*           Bad pixel flag for the mapped data values.
*        ACB_DMBMD( NDF__MXACB ) = LOGICAL (Write)
*           Whether the ACB_VMBAD value has been modified.

*  Arguments Given:
      LOGICAL BAD
      INTEGER INDF
      CHARACTER * ( * ) COMP

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      LOGICAL NDF1_SIMLR         ! String compare with abbreviation

*  Local Variables:
      INTEGER F                  ! Position of first non-blank character
      INTEGER I1                 ! Position of first component character
      INTEGER I2                 ! Position of last component character
      INTEGER IACB               ! Index to NDF entry in the ACB
      INTEGER L                  ! Position of last non-blank character
      INTEGER NCOMP              ! Number non-blank components specified

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Import the NDF identifier.
      CALL NDF1_IMPID( INDF, IACB, STATUS )

*  Check that write access to the NDF is available.
      CALL NDF1_CHACC( IACB, 'WRITE', STATUS )
      IF ( STATUS .EQ. SAI__OK ) THEN

*  Initialise the component count.
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

*  AXIS component:
*  ==============
*  Report an error, since this component has no bad pixel flag.
                  IF ( NDF1_SIMLR( COMP( F : L ), 'AXIS',
     :                             NDF__MINAB ) ) THEN
                     STATUS = NDF__CNMIN
                     CALL ERR_REP( 'NDF_SBAD_AXI',
     :               'A bad-pixel flag value cannot be set for an ' //
     :               'AXIS component (possible programming error).',
     :               STATUS )

*  DATA component:
*  ==============
*  If the data component is mapped for access, then modify the bad pixel
*  flag for the mapped values and note this has been done.
                  ELSE IF ( NDF1_SIMLR( COMP( F : L ), 'DATA',
     :                                  NDF__MINAB ) ) THEN
                     IF ( ACB_DMAP( IACB ) ) THEN
                        ACB_DMBAD( IACB ) = BAD
                        ACB_DMBMD( IACB ) = .TRUE.

*  Otherwise, use the ARY_ system to set the bad pixel flag value for
*  the data array.
                     ELSE
                        CALL ARY_SBAD( BAD, ACB_DID( IACB ), STATUS )
                     END IF

*  EXTENSION:
*  =========
*  Report an error, since extensions have no bad pixel flag.
                  ELSE IF ( NDF1_SIMLR( COMP( F : L ), 'EXTENSION',
     :                                  NDF__MINAB ) ) THEN
                     STATUS = NDF__CNMIN
                     CALL ERR_REP( 'NDF_SBAD_EXT',
     :               'A bad-pixel flag value cannot be set for an ' //
     :               'EXTENSION (possible programming error).', STATUS )

*  HISTORY component:
*  =================
*  Report an error, since this component has no bad pixel flag.
                  ELSE IF ( NDF1_SIMLR( COMP( F : L ), 'HISTORY',
     :                                  NDF__MINAB ) ) THEN
                     STATUS = NDF__CNMIN
                     CALL ERR_REP( 'NDF_SBAD_HIS',
     :               'A bad-pixel flag value cannot be set for a ' //
     :               'HISTORY component (possible programming error).',
     :               STATUS )

*  LABEL component:
*  ===============
*  Report an error, since this component has no bad pixel flag.
                  ELSE IF ( NDF1_SIMLR( COMP( F : L ), 'LABEL',
     :                                  NDF__MINAB ) ) THEN
                     STATUS = NDF__CNMIN
                     CALL ERR_REP( 'NDF_SBAD_LAB',
     :               'A bad-pixel flag value cannot be set for a ' //
     :               'LABEL component (possible programming error).',
     :               STATUS )

*  QUALITY component:
*  =================
*  Report an error, since this component has no bad pixel flag.
                  ELSE IF ( NDF1_SIMLR( COMP( F : L ), 'QUALITY',
     :                                  NDF__MINAB ) ) THEN
                     STATUS = NDF__CNMIN
                     CALL ERR_REP( 'NDF_SBAD_QUA',
     :               'A bad-pixel flag value cannot be set for a ' //
     :               'QUALITY component (possible programming error).',
     :               STATUS )

*  TITLE component:
*  ===============
*  Report an error, since this component has no bad pixel flag.
                  ELSE IF ( NDF1_SIMLR( COMP( F : L ), 'TITLE',
     :                                  NDF__MINAB ) ) THEN
                     STATUS = NDF__CNMIN
                     CALL ERR_REP( 'NDF_SBAD_TIT',
     :               'A bad-pixel flag value cannot be set for a ' //
     :               'TITLE component (possible programming error).',
     :               STATUS )

*  UNITS component:
*  ===============
*  Report an error, since this component has no bad pixel flag.
                  ELSE IF ( NDF1_SIMLR( COMP( F : L ), 'UNITS',
     :                                  NDF__MINAB ) ) THEN
                     STATUS = NDF__CNMIN
                     CALL ERR_REP( 'NDF_SBAD_UNI',
     :               'A bad-pixel flag value cannot be set for a ' //
     :               'UNITS component (possible programming error).',
     :               STATUS )

*  VARIANCE component:
*  ==================
*  Set the bad pixel flag.
                  ELSE IF ( NDF1_SIMLR( COMP( F : L ), 'VARIANCE',
     :                                  NDF__MINAB ) ) THEN
                     CALL NDF1_VSBD( BAD, IACB, STATUS )

*  If the component name is not recognised, then report an error.
                  ELSE
                     STATUS = NDF__CNMIN
                     CALL MSG_SETC( 'BADCOMP', COMP( F : L ) )
                     CALL ERR_REP( 'NDF_SBAD_COMP',
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
            CALL ERR_REP( 'NDF_SBAD_NONE',
     :                    'No array component name specified ' //
     :                    '(possible programming error).', STATUS )
         END IF
      END IF

*  If an error occurred, then report context information and call the
*  error tracing routine.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'NDF_SBAD_ERR',
     :   'NDF_SBAD: Error setting the bad-pixel flag for an NDF ' //
     :   'array component.', STATUS )
         CALL NDF1_TRACE( 'NDF_SBAD', STATUS )
      END IF

      END
