      SUBROUTINE NDF_ASTAT( INDF, COMP, IAXIS, STATE, STATUS )
*+
*  Name:
*     NDF_ASTAT

*  Purpose:
*     Determine the state of an NDF axis component (defined or
*     undefined).

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL NDF_ASTAT( INDF, COMP, IAXIS, STATE, STATUS )

*  Description:
*     The routine returns a logical value indicating whether a
*     specified NDF axis component has a defined value (or values).

*  Arguments:
*     INDF = INTEGER (Given)
*        NDF identifier.
*     COMP = CHARACTER * ( * ) (Given)
*        Name of the axis component: 'CENTRE', 'LABEL', 'UNITS',
*        'VARIANCE' or 'WIDTH'.
*     IAXIS = INTEGER (Given)
*        Number of the NDF axis for which information is required.
*     STATE = LOGICAL (Returned)
*        Whether the specified component is defined.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     -  A comma-separated list of axis component names may also be
*     given, in which case the routine will return the logical "AND" of
*     the states of the specified components (i.e. a .TRUE. result will
*     be returned only if all the components have defined values).
*     -  A value of zero may be given for the IAXIS argument, in which
*     case the routine will return the logical "AND" of the results for
*     all the NDF's axes.

*  Algorithm:
*     - Import the NDF identifier.
*     - Check the axis number for validity.
*     - Initialise the result and the component count.
*     - Obtain an index to the data object entry in the DCB.
*     - Initialise the character pointer to the start of the component
*     list.  Then loop to extract each element from the component list.
*     -  Find the final character of the next element in the component
*     list (the last character before a comma or end of string).
*     -  Locate the first and last non-blank characters in the element,
*     checking that it is not entirely blank.
*     -  Increment the component count.
*     -  Compare the component name with each value in turn (allowing
*     abbreviation), and take the appropriate action, or report an
*     error if an inappropriate component name has been given.
*     -  If the component name was not recognised, then report an
*     error.
*     -  Increment the character pointer to the start of the next
*     element in the component list and return to process the next
*     element.
*     -  If no error has occurred, but no non-blank component names
*     have been processed, then report an error.
*     -  If an error occurred, then report context information.

*  Copyright:
*     Copyright (C) 1990, 1991 Science & Engineering Research Council.
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
*     17-OCT-1990 (RFWS):
*        Original version.
*     19-OCT-1990 (RFWS):
*        Added an error message for the extension component.
*     14-OCT-1991 (RFWS):
*        Corrected typo in prologue.
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
      INCLUDE 'ARY_PAR'          ! ARY_ public constants

*  Global Variables:
      INCLUDE 'NDF_DCB'          ! NDF_ Data Control Block
*        DCB_ACLOC( NDC__MXDIM, NDF__MXACN, NDF__MXDCB ) = CHARACTER * (
*        DAT__SZLOC ) (Read)
*           Locators to axis character components.
*        DCB_ADID( NDF__MXDIM, NDF__MXDCB ) = INTEGER (Read)
*           ARY_ system identifiers for axis data arrays.
*        DCB_AVID( NDF__MXDIM, NDF__MXDCB ) = INTEGER (Read)
*           ARY_ system identifiers for axis variance arrays.
*        DCB_AWID( NDF__MXDIM, NDF__MXDCB ) = INTEGER (Read)
*           ARY_ system identifier for axis width arrays.

      INCLUDE 'NDF_ACB'          ! NDF_ Access Control Block
*        ACB_IDCB( NDF__MXACB ) = INTEGER (Read)
*           Index to data object entry in the DCB.

*  Arguments Given:
      INTEGER INDF
      CHARACTER * ( * ) COMP
      INTEGER IAXIS

*  Arguments Returned:
      LOGICAL STATE

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
      INTEGER IDCB               ! Index to data object entry in the DCB
      INTEGER L                  ! Position of last non-blank character
      INTEGER NCOMP              ! Number non-blank components specified

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Import the NDF identifier.
      CALL NDF1_IMPID( INDF, IACB, STATUS )

*  Check the axis number for validity.
      CALL NDF1_VAN( IACB, IAXIS, .TRUE., IAX1, IAX2, STATUS )
      IF ( STATUS .EQ. SAI__OK ) THEN

*  Initialise the result and the component count.
         STATE = .TRUE.
         NCOMP = 0

*  Obtain an index to the data object entry in the DCB.
         IDCB = ACB_IDCB( IACB )

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

*  CENTRE component:
*  ================
*  Loop to process all relevant axes.
                  IF ( NDF1_SIMLR( COMP( F : L ), 'CENTRE',
     :                             NDF__MINAB ) .OR.
     :                 NDF1_SIMLR( COMP( F : L ), 'CENTER',
     :                             NDF__MINAB ) ) THEN
                     DO 2 IAX = IAX1, IAX2
                        IF ( STATE ) THEN

*  Ensure that axis data array information is available.
                           CALL NDF1_DAD( IAX, IDCB, STATUS )
                           IF ( STATUS .EQ. SAI__OK ) THEN

*  If the axis data array does not exist, then its state is .FALSE..
*  Otherwise, use its identifier to determine its state.
                              STATE = DCB_ADID( IAX, IDCB ) .NE.
     :                                ARY__NOID
                              IF ( STATE ) THEN
                                 CALL ARY_STATE( DCB_ADID( IAX, IDCB ),
     :                                           STATE, STATUS )
                              END IF
                           END IF
                        END IF
 2                   CONTINUE

*  EXTENSION component:
*  ===================
*  Not yet supported, so report an error.
                  ELSE IF ( NDF1_SIMLR( COMP( F : L ), 'EXTENSION',
     :                                  NDF__MINAB ) ) THEN
                     STATUS = NDF__NOTSP
                     CALL ERR_REP( 'NDF_ASTAT_EXT',
     :                             'Sorry, axis EXTENSIONs are not ' //
     :                             'yet supported.', STATUS )

*  LABEL component:
*  ===============
*  Loop to process all relevant axes.
                  ELSE IF ( NDF1_SIMLR( COMP( F : L ), 'LABEL',
     :                                  NDF__MINAB ) ) THEN
                     DO 3 IAX = IAX1, IAX2
                        IF ( STATE ) THEN

*  Ensure that axis character component information is available.
                           CALL NDF1_DAC( IAX, NDF__ALAB, IDCB, STATUS )
                           IF ( STATUS .EQ. SAI__OK ) THEN

*  See whether the label component exists.
                              STATE = DCB_ACLOC( IAX, NDF__ALAB, IDCB )
     :                                .NE. DAT__NOLOC
                           END IF
                        END IF
 3                   CONTINUE

*  UNITS component:
*  ===============
*  Loop to process all relevant axes.
                  ELSE IF ( NDF1_SIMLR( COMP( F : L ), 'UNITS',
     :                                  NDF__MINAB ) ) THEN
                     DO 4 IAX = IAX1, IAX2
                        IF ( STATE ) THEN

*  Ensure that axis character component information is available.
                           CALL NDF1_DAC( IAX, NDF__AUNI, IDCB, STATUS )
                           IF ( STATUS .EQ. SAI__OK ) THEN

*  See whether the label component exists.
                              STATE = DCB_ACLOC( IAX, NDF__AUNI, IDCB )
     :                                .NE. DAT__NOLOC
                           END IF
                        END IF
 4                   CONTINUE

*  VARIANCE component:
*  ==================
*  Loop to process all relevant axes.
                  ELSE IF ( NDF1_SIMLR( COMP( F : L ), 'VARIANCE',
     :                                  NDF__MINAB ) ) THEN
                     DO 5 IAX = IAX1, IAX2
                        IF ( STATE ) THEN

*  Ensure that axis variance array information is available.
                           CALL NDF1_DAV( IAX, IDCB, STATUS )
                           IF ( STATUS .EQ. SAI__OK ) THEN

*  If the axis variance array does not exist, then its state is
*  .FALSE..  Otherwise, use its identifier to determine its state.
                              STATE = DCB_AVID( IAX, IDCB ) .NE.
     :                                ARY__NOID
                              IF ( STATE ) THEN
                                 CALL ARY_STATE( DCB_AVID( IAX, IDCB ),
     :                                           STATE, STATUS )
                              END IF
                           END IF
                        END IF
 5                   CONTINUE

*  WIDTH component:
*  ===============
*  Loop to process all relevant axes.
                  ELSE IF ( NDF1_SIMLR( COMP( F : L ), 'WIDTH',
     :                                  NDF__MINAB ) ) THEN
                     DO 6 IAX = IAX1, IAX2
                        IF ( STATE ) THEN

*  Ensure that axis width array information is available.
                           CALL NDF1_DAW( IAX, IDCB, STATUS )
                           IF ( STATUS .EQ. SAI__OK ) THEN

*  If the axis width array does not exist, then its state is .FALSE..
*  Otherwise, use its identifier to determine its state.
                              STATE = DCB_AWID( IAX, IDCB ) .NE.
     :                                ARY__NOID
                              IF ( STATE ) THEN
                                 CALL ARY_STATE( DCB_AWID( IAX, IDCB ),
     :                                           STATE, STATUS )
                              END IF
                           END IF
                        END IF
 6                   CONTINUE

*  If the component name was not recognised, then report an error.
                  ELSE
                     STATUS = NDF__CNMIN
                     CALL MSG_SETC( 'BADCOMP', COMP( F : L ) )
                     CALL ERR_REP( 'NDF_ASTAT_COMP',
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
            CALL ERR_REP( 'NDF_ASTAT_NONE',
     :                    'No axis component name specified ' //
     :                    '(possible programming error).', STATUS )
         END IF
      END IF

*  If an error occurred, then report context information and call the
*  error tracing routine.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'NDF_ASTAT_ERR',
     :   'NDF_ASTAT: Error determining the state of an NDF axis ' //
     :   'component.', STATUS )
         CALL NDF1_TRACE( 'NDF_ASTAT', STATUS )
      END IF

      END
