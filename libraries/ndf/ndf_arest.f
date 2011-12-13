      SUBROUTINE NDF_AREST( INDF, COMP, IAXIS, STATUS )
*+
*  Name:
*     NDF_AREST

*  Purpose:
*     Reset an NDF axis component to an undefined state.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL NDF_AREST( INDF, COMP, IAXIS, STATUS )

*  Description:
*     The routine resets an NDF axis component so that its value
*     becomes undefined. It may be used to remove unwanted optional NDF
*     axis components.

*  Arguments:
*     INDF = INTEGER (Given)
*        NDF identifier.
*     COMP = CHARACTER * ( * ) (Given)
*        Name of the axis component to be reset: 'LABEL', 'UNITS',
*        'VARIANCE' or 'WIDTH'.
*     IAXIS = INTEGER (Given)
*        Number of the NDF axis to be modified.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     -  A comma-separated list of axis component names may also be
*     supplied, in which case each component will be reset in turn.
*     -  A value of zero may be supplied for the IAXIS argument, in
*     which case the same component(s) will be reset on all the NDF's
*     axes.
*     -  An axis component name of 'CENTRE' may not be specified for
*     this routine because the pixel centre information cannot be reset
*     for each axis of an NDF individually. This information may only
*     be removed from an NDF by resetting the entire axis component.
*     This can be done by calling the routine NDF_RESET and specifying
*     a component name of 'AXIS'.
*     -  This routine may only be used to reset an axis component via a
*     base NDF. If an NDF section is supplied, then it will return
*     without action. No error will result.
*     -  An NDF axis array component cannot be reset while it is mapped
*     for access, even if this is via another NDF identifier. This
*     routine will fail, and set a STATUS value, if this is the case.

*  Algorithm:
*     -  Import the NDF identifier.
*     -  Check the axis number for validity.
*     -  Check that WRITE access to the NDF is available.
*     -  Initialise the component count.
*     -  Initialise the character pointer to the start of the component
*     list. Then loop to extract each element from the component list.
*     -  Find the final character of the next element in the component
*     list (the last character before a comma or end of string).
*     -  Locate the first and last non-blank characters in the element,
*     checking that it is not entirely blank.
*     -  Increment the component count.
*     -  Compare the component name with each value in turn (allowing
*     abbreviation), and take the appropriate action.
*     -  If the component name is not recognised, then report an error.
*     -  Increment the character pointer to the start of the next
*     element in the component list and return to process the next
*     element.
*     -  If no error has occurred, but no non-blank component names
*     have been processed, then report an error.
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
*     15-OCT-1990 (RFWS):
*        Original version.
*     13-NOV-1990 (RFWS):
*        Revised the prologue.
*     28-NOV-1990 (RFWS):
*        Added error message for EXTENSION components, which are not yet
*        supported.
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

*  Arguments Given:
      INTEGER INDF
      CHARACTER * ( * ) COMP
      INTEGER IAXIS

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      LOGICAL NDF1_SIMLR         ! String compare with abbreviation

*  Local Variables:
      INTEGER F                  ! Position of first non-blank character
      INTEGER I1                 ! Position of first component character
      INTEGER I2                 ! Position of last component character
      INTEGER IACB               ! Index to NDF entry in the ACB
      INTEGER IAX                ! Loop counter for axes
      INTEGER IAX1               ! First axis to process
      INTEGER IAX2               ! Last axis to process
      INTEGER L                  ! Position of last non-blank character
      INTEGER NCOMP              ! Number non-blank components specified

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Import the NDF identifier.
      CALL NDF1_IMPID( INDF, IACB, STATUS )

*  Check the axis number for validity.
      CALL NDF1_VAN( IACB, IAXIS, .TRUE., IAX1, IAX2, STATUS )

*  Check that WRITE access to the NDF is available.
      CALL NDF1_CHACC( IACB, 'WRITE', STATUS )
      IF ( STATUS .EQ. SAI__OK ) THEN

*  Initialise the component count.
         NCOMP = 0

*  Initialise the character pointer to the start of the component list.
*  Then loop to extract each element from the component list.
         I1 = 1
 1       CONTINUE
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
*  abbreviation), and take the appropriate action.

*  CENTRE component:
*  ================
*  The data array cannot be reset on an individual axis basis, so report
*  an error.
                  IF ( NDF1_SIMLR( COMP( F : L ), 'CENTRE',
     :                             NDF__MINAB ) .OR.
     :                 NDF1_SIMLR( COMP( F : L ), 'CENTER',
     :                             NDF__MINAB ) ) THEN
                     STATUS = NDF__CNMIN
                     CALL ERR_REP( 'NDF_AREST_CENTRE',
     :                             'The CENTRE array for an ' //
     :                             'individual NDF axis cannot be ' //
     :                             'reset; use NDF_RESET to reset ' //
     :                             'the entire axis coordinate ' //
     :                             'system (possible programming ' //
     :                             'error).', STATUS )

*  EXTENSION component:
*  ===================
*  Not yet supported, so report an error.
                  ELSE IF ( NDF1_SIMLR( COMP( F : L ), 'EXTENSION',
     :                                  NDF__MINAB ) ) THEN
                     STATUS = NDF__NOTSP
                     CALL ERR_REP( 'NDF_AREST_EXTN',
     :                             'Sorry, axis EXTENSION ' //
     :                             'components are not yet ' //
     :                             'supported.', STATUS )

*  LABEL component:
*  ===============
*  Reset the component(s).
                  ELSE IF ( NDF1_SIMLR( COMP( F : L ), 'LABEL',
     :                                  NDF__MINAB ) ) THEN
                     DO 2 IAX = IAX1, IAX2
                        CALL NDF1_ACRST( IAX, NDF__ALAB, IACB, STATUS )
 2                   CONTINUE

*  UNITS component:
*  ===============
*  Reset the component(s).
                  ELSE IF ( NDF1_SIMLR( COMP( F : L ), 'UNITS',
     :                                  NDF__MINAB ) ) THEN
                     DO 3 IAX = IAX1, IAX2
                        CALL NDF1_ACRST( IAX, NDF__AUNI, IACB, STATUS )
 3                   CONTINUE

*  VARIANCE component:
*  ==================
*  Reset the axis variance array(s).
                  ELSE IF ( NDF1_SIMLR( COMP( F : L ), 'VARIANCE',
     :                                  NDF__MINAB ) ) THEN
                     DO 4 IAX = IAX1, IAX2
                        CALL NDF1_AVRST( IAX, IACB, STATUS )
 4                   CONTINUE

*  WIDTH component:
*  ===============
*  Reset the axis width array(s).
                  ELSE IF ( NDF1_SIMLR( COMP( F : L ), 'WIDTH',
     :                                  NDF__MINAB ) ) THEN
                     DO 5 IAX = IAX1, IAX2
                        CALL NDF1_AWRST( IAX, IACB, STATUS )
 5                   CONTINUE

*  If the component name is not recognised, then report an error.
                  ELSE
                     STATUS = NDF__CNMIN
                     CALL MSG_SETC( 'BADCOMP', COMP( F : L ) )
                     CALL ERR_REP( 'NDF_AREST_COMP',
     :                             'Invalid axis component name ' //
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
            CALL ERR_REP( 'NDF_AREST_NONE',
     :                    'No axis component name specified ' //
     :                    '(possible programming error).', STATUS )
         END IF
      END IF

*  If an error occurred, then report context information and call the
*  error tracing routine.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'NDF_AREST_ERR',
     :   'NDF_AREST: Error resetting an NDF axis component to an ' //
     :   'undefined state.', STATUS )
         CALL NDF1_TRACE( 'NDF_AREST', STATUS )
      END IF

      END
